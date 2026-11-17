import MyJson.*
import java.io.RandomAccessFile

object DataWriter {
  def validateSources(manifest: ManifestData, settings: AppSettings): Either[AppError, Unit] = {
    val changed = manifest.sources.filter(!_.isDirectory).filter { s =>
      val p = os.Path(s.originalPath)
      if (!os.exists(p) || os.size(p) != s.size) true
      else calculateFileChecksum(p, settings.checkSumAlgorithm) match {
        case Right(hash) => hash != s.checksum
        case Left(_) => true
      }
    }
    if (changed.nonEmpty) Left(SourcesChanged) else Right(())
  }

  def writePart(part: PartInfo, sourceMap: Map[String, SourceItem], driveFolder: os.Path, settings: AppSettings): Either[AppError, PartInfo] = {
    try {
      val outBuffer = new java.io.ByteArrayOutputStream()
      for (chunk <- part.chunks) {
        val srcItem = sourceMap(chunk.relativePath)
        val raf = new RandomAccessFile(srcItem.originalPath, "r")
        try {
          raf.seek(chunk.fileOffset)
          val bytes = new Array[Byte](chunk.length.toInt)
          raf.readFully(bytes)
          outBuffer.write(bytes)
        } finally raf.close()
      }
      val partBytes = outBuffer.toByteArray
      os.write.over(driveFolder / part.fileName, partBytes)
      val expectedHash = calculateChecksum(partBytes, settings.checkSumAlgorithm)
      
      val verified = if (settings.verifyAfterWrite) {
        val p = driveFolder / part.fileName
        os.exists(p) && os.size(p) == part.size && calculateFileChecksum(p, settings.checkSumAlgorithm).contains(expectedHash)
      } else true

      if (verified) Right(part.copy(checksum = expectedHash, writtenAndVerified = true))
      else Left(VerificationFailed(s"Хэш части ${part.fileName} не совпал"))
    } catch {
      case e: Exception => Left(IoError(s"Ошибка записи части ${part.fileName}: ${e.getMessage}"))
    }
  }

  def prepareMedia(mediaNum: Int, manifest: ManifestData, mediaRoot: os.Path, settings: AppSettings): Either[AppError, Unit] = {
    val drive = manifest.mediaOrder(mediaNum - 1)
    val driveFolder = mediaRoot / drive.path
    
    if (!os.exists(driveFolder)) Left(DriveNotFound(drive.path))
    else if (drive.isProtected) Left(DriveProtected(drive.mark))
    else {
      val descriptorPath = driveFolder / settings.mediaDescriptorFile
      if (os.exists(descriptorPath)) {
        try {
          val existingDesc = read[MediaDescriptor](os.read(descriptorPath))
          if (existingDesc.setId != manifest.setId) Left(ForeignSetFound(drive.mark, existingDesc.setId))
          else Right(())
        } catch { case _: Exception => Right(()) } 
      } else Right(())
    }
  }
}