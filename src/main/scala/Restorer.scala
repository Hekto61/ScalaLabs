import MyJson.*
import java.io.RandomAccessFile
import scala.io.StdIn.readLine

object Restorer {
  // проверка набора (без восстановления)
  def verifySet(manifest: ManifestData, mediaRoot: os.Path, settings: AppSettings): Boolean = {
    println(s"\n=== Проверка набора ${manifest.name} (ID: ${manifest.setId}) ===")
    logEvent(settings, s"Начата проверка набора ${manifest.name} (ID: ${manifest.setId})")

    if (!manifest.completed) {
      println("Ошибка: набор не был завершён!")
      return false
    }

    var allOk = true

    for ((drive, idx) <- manifest.mediaOrder.zipWithIndex) {
      val mediaNum = idx + 1
      val driveFolder = mediaRoot / drive.path
      val expectedParts = manifest.parts.filter(_.mediaNumber == mediaNum)
      var mediaOk = true

      // Проверка media-info.json
      val descPath = driveFolder / settings.mediaDescriptorFile
      if (!os.exists(descPath)) {
        println(s"Носитель $mediaNum (${drive.mark}): ошибка (отсутствует ${settings.mediaDescriptorFile})")
        mediaOk = false
        allOk = false
      } else {
        try {
          val desc = read[MediaDescriptor](os.read(descPath))
          if (desc.setId != manifest.setId || desc.mediaNumber != mediaNum || !desc.completed) {
            println(s"Носитель $mediaNum (${drive.mark}): ошибка (неверный дескриптор или идентификатор!)")
            mediaOk = false
            allOk = false
          }
        } catch {
          case _: Exception =>
            println(s"Носитель $mediaNum (${drive.mark}): ошибка (повреждён ${settings.mediaDescriptorFile})")
            mediaOk = false
            allOk = false
        }
      }

      // Проверка по SHA-256
      for (part <- expectedParts) {
        val partPath = driveFolder / part.fileName
        if (!os.exists(partPath)) {
          println(s"Носитель $mediaNum: ошибка! Отсутствует часть ${part.fileName} (номер ${part.number})")
          mediaOk = false
          allOk = false
        } else {
          val actualSize = os.size(partPath)
          val actualHash = calculateFileChecksum(partPath, manifest.checksumAlgorithm)
          if (actualSize != part.size || actualHash != part.checksum) {
            println(s"Носитель $mediaNum: ошибка")
            println(s"  Повреждена часть: ${part.fileName} (номер ${part.number})")
            println(s"  Ожидаемый размер: ${part.size}, Фактический: $actualSize")
            println(s"  Ожидаемая сумма:  ${part.checksum}")
            println(s"  Фактическая сумма: $actualHash")
            mediaOk = false
            allOk = false
          }
        }
      }

      if (mediaOk) {
        println(s"Носитель $mediaNum (${drive.mark}): успешно")
      }
    }

    if (allOk) {
      println("\nВсе части успешно проверены! Набор полностью цел.")
      logEvent(settings, s"Проверка набора ${manifest.name} завершена успешно")
    } else {
      println("\nВосстановление полного набора невозможно.")
      logEvent(settings, s"Проверка набора ${manifest.name} выявила ошибки")
    }
    allOk
  }

  //Восстановление
  def restoreSet(
                  manifest: ManifestData,
                  targetDir: os.Path,
                  mediaRoot: os.Path,
                  settings: AppSettings
                ): Unit = {
    if (!manifest.completed) {
      println("Ошибка: набор не завершён! Восстановление невозможно.")
      return
    }

    val totalSize = manifest.sources.filter(!_.isDirectory).map(_.size).sum
    println(s"\n=== Сводка перед восстановлением ===")
    println(s"Набор: ${manifest.name}")
    println(s"Носителей: ${manifest.mediaCount}")
    println(s"Частей: ${manifest.parts.size}")
    println(s"Исходных объектов: ${manifest.sources.size}")
    println(s"Исходный объём: ${formatSize(totalSize)}")
    println(s"Каталог восстановления: $targetDir\n")

    // Проверка целостности
    if (!verifySet(manifest, mediaRoot, settings)) {
      println("Восстановление отменено из-за повреждения частей.")
      return
    }

    logEvent(settings, s"Начато восстановление набора ${manifest.name} в $targetDir")
    os.makeDir.all(targetDir)

    // Восстанавливаем пустые папки (если задано в настройках)
    if (manifest.preserveEmptyDirectories) {
      manifest.sources.filter(_.isDirectory).foreach { dir =>
        os.makeDir.all(targetDir / os.RelPath(dir.relativePath))
      }
    }

    var skippedFiles = Set.empty[String]
    var renameMap = Map.empty[String, os.Path]

    for (src <- manifest.sources.filter(!_.isDirectory)) {
      val outPath = targetDir / os.RelPath(src.relativePath)
      os.makeDir.all(outPath / os.up)

      if (os.exists(outPath)) {
        settings.overrideMode match {
          case "forbid" =>
            println(s"Файл уже существует (перезапись запрещена): $outPath. Пропускаем.")
            skippedFiles += src.relativePath
          case "overwrite" =>
            os.remove(outPath)
          case "rename" =>
            val renamed = outPath / os.up / s"restored_${outPath.last}"
            println(s"Файл существует, восстанавливаем как: $renamed")
            if (os.exists(renamed)) os.remove(renamed)
            renameMap += (src.relativePath -> renamed)
          case _ => // "ask"
            print(s"Файл $outPath уже существует. Перезаписать? (y/n): ")
            val ans = readLine().trim.toLowerCase
            if (ans == "y" || ans == "yes" || ans == "д") os.remove(outPath)
            else skippedFiles += src.relativePath
        }
      }
      if (src.size == 0L && !skippedFiles.contains(src.relativePath)) {
        val finalPath = renameMap.getOrElse(src.relativePath, outPath)
        os.write.over(finalPath, Array.emptyByteArray)
      }
    }

    // Части обратно в файлы
    for (part <- manifest.parts.sortBy(_.number)) {
      val driveFolder = mediaRoot / part.mediaPath
      val partPath = driveFolder / part.fileName
      val partBytes = os.read.bytes(partPath)
      var offsetInPart = 0

      for (chunk <- part.chunks) {
        if (!skippedFiles.contains(chunk.relativePath)) {
          val defaultOut = targetDir / os.RelPath(chunk.relativePath)
          val outPath = renameMap.getOrElse(chunk.relativePath, defaultOut)
          val chunkBytes = partBytes.slice(offsetInPart, offsetInPart + chunk.length.toInt)

          val raf = new RandomAccessFile(outPath.toIO, "rw")
          try {
            raf.seek(chunk.fileOffset)
            raf.write(chunkBytes)
          } finally {
            raf.close()
          }
        }
        offsetInPart += chunk.length.toInt
      }
    }

    // Проверка контрольных сумм восстановления
    var restoredCount = 0
    var verifiedCount = 0
    var corruptedCount = 0

    for (src <- manifest.sources.filter(!_.isDirectory)) {
      if (!skippedFiles.contains(src.relativePath)) {
        restoredCount += 1
        val outPath = renameMap.getOrElse(src.relativePath, targetDir / os.RelPath(src.relativePath))
        val actualHash = calculateFileChecksum(outPath, manifest.checksumAlgorithm)
        if (actualHash == src.checksum) {
          verifiedCount += 1
        } else {
          println(s"Ошибка контрольной суммы восстановленного файла: $outPath")
          corruptedCount += 1
        }
      }
    }

    val status = if (corruptedCount == 0) "УСПЕШНО" else "ЗАВЕРШЕНО С ОШИБКАМИ"
    println(s"\n================ ОТЧЁТ О ВОССТАНОВЛЕНИИ ================")
    println(s"Набор: ${manifest.name}")
    println(s"Каталог восстановления: $targetDir")
    println(s"Восстановлено файлов: $restoredCount")
    println(s"Проверено успешно: $verifiedCount")
    println(s"Повреждено или пропущено: ${corruptedCount + skippedFiles.size}")
    println(s"Итоговый статус: $status")
    println(s"========================================================\n")
    logEvent(settings, s"Восстановление завершено со статусом: $status")
  }
}