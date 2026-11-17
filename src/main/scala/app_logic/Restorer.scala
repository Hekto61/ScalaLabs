import MyJson.*
import java.io.RandomAccessFile

object Restorer {
  def verifySet(manifest: ManifestData, mediaRoot: os.Path, settings: AppSettings): Either[AppError, Unit] = {
    if (!manifest.completed) Left(ManifestNotCompleted)
    else {
      val errors = manifest.mediaOrder.zipWithIndex.flatMap { case (drive, idx) =>
        val mediaNum = idx + 1
        val driveFolder = mediaRoot / drive.path
        val expectedParts = manifest.parts.filter(_.mediaNumber == mediaNum)
        
        val descPath = driveFolder / settings.mediaDescriptorFile
        val descError = if (!os.exists(descPath)) Some(VerificationFailed(s"Носитель $mediaNum: нет дескриптора")) else {
          try {
            val desc = read[MediaDescriptor](os.read(descPath))
            if (desc.setId != manifest.setId || desc.mediaNumber != mediaNum || !desc.completed)
              Some(VerificationFailed(s"Носитель $mediaNum: неверный дескриптор или ID"))
            else None
          } catch { case _: Exception => Some(VerificationFailed(s"Носитель $mediaNum: повреждён дескриптор")) }
        }
        
        val partErrors = expectedParts.flatMap { part =>
          val p = driveFolder / part.fileName
          if (!os.exists(p)) Some(VerificationFailed(s"Носитель $mediaNum: нет части ${part.fileName}"))
          else {
            val size = os.size(p)
            calculateFileChecksum(p, manifest.checksumAlgorithm) match {
              case Left(e) => Some(e)
              case Right(hash) if size != part.size || hash != part.checksum =>
                Some(VerificationFailed(s"Носитель $mediaNum: повреждена часть ${part.fileName}"))
              case _ => None
            }
          }
        }
        descError ++ partErrors
      }
      if (errors.isEmpty) Right(()) else Left(errors.head)
    }
  }

  def restoreSet(manifest: ManifestData, targetDir: os.Path, mediaRoot: os.Path, settings: AppSettings, onConflict: os.Path => Boolean): Either[AppError, Unit] = {
    verifySet(manifest, mediaRoot, settings).flatMap { _ =>
      try {
        os.makeDir.all(targetDir)
        if (manifest.preserveEmptyDirectories) {
          manifest.sources.filter(_.isDirectory).foreach(d => os.makeDir.all(targetDir / os.RelPath(d.relativePath)))
        }

        var skippedFiles = Set.empty[String]
        var renameMap = Map.empty[String, os.Path]

        manifest.sources.filter(!_.isDirectory).foreach { src =>
          val outPath = targetDir / os.RelPath(src.relativePath)
          os.makeDir.all(outPath / os.up)
          if (os.exists(outPath)) {
            if (onConflict(outPath)) os.remove(outPath)
            else skippedFiles += src.relativePath
          }
          if (src.size == 0L && !skippedFiles.contains(src.relativePath)) {
            os.write.over(renameMap.getOrElse(src.relativePath, outPath), Array.emptyByteArray)
          }
        }

        manifest.parts.sortBy(_.number).foreach { part =>
          val partPath = mediaRoot / part.mediaPath / part.fileName
          val partBytes = os.read.bytes(partPath)
          var offset = 0
          part.chunks.foreach { chunk =>
            if (!skippedFiles.contains(chunk.relativePath)) {
              val outPath = renameMap.getOrElse(chunk.relativePath, targetDir / os.RelPath(chunk.relativePath))
              val raf = new RandomAccessFile(outPath.toIO, "rw")
              try {
                raf.seek(chunk.fileOffset)
                raf.write(partBytes.slice(offset, offset + chunk.length.toInt))
              } finally raf.close()
            }
            offset += chunk.length.toInt
          }
        }
        Right(())
      } catch { case e: Exception => Left(IoError(s"Ошибка восстановления: ${e.getMessage}")) }
    }
  }
}