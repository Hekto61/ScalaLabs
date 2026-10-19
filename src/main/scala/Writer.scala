import MyJson.*
import java.io.RandomAccessFile
import scala.io.StdIn.readLine

object DataWriter{

  def executeWrite(
                    initialManifest: ManifestData,
                    mediaRoot: os.Path,
                    settings: AppSettings
                  ): Option[ManifestData] = {
    val workDir = os.pwd / settings.workDirectory
    os.makeDir.all(workDir)
    val stateFile = workDir / "transfer-state.json"

    val changedFiles = initialManifest.sources.filter(!_.isDirectory).filter { s =>
      val p = os.Path(s.originalPath)
      !os.exists(p) || os.size(p) != s.size || calculateFileChecksum(p, settings.checkSumAlgorithm) != s.checksum
    }
    if (changedFiles.nonEmpty) {
      println("Ошибка! Следующие исходные файлы изменились или были удалены после построения плана:")
      changedFiles.foreach(f => println(s"  - ${f.originalPath}"))
      println("Запись отменена. Пожалуйста, перестройте план.")
      logEvent(settings, "Запись отменена: обнаружены изменения исходных файлов после планирования")
      return None
    }

    var currentManifest = initialManifest
    val sourceMap = currentManifest.sources.map(s => s.relativePath -> s).toMap
    val mediaNumbers = currentManifest.parts.map(_.mediaNumber).distinct.sorted

    for (mediaNum <- mediaNumbers) {
      val drive = currentManifest.mediaOrder(mediaNum - 1)
      val driveFolder = mediaRoot / drive.path
      val mediaParts = currentManifest.parts.filter(_.mediaNumber == mediaNum)

      if (mediaParts.forall(_.writtenAndVerified)) {
        println(s"Носитель $mediaNum (${drive.mark}) уже полностью записан и проверен. Пропускаем.")
      } else {
        println(s"\nПодготовьте носитель $mediaNum из ${currentManifest.mediaCount}: ${drive.mark} (папка: ${drive.path})")
        print("Нажмите Enter для подтверждения подключения носителя (или введите 'pause' для прерывания): ")
        val ans = readLine().trim.toLowerCase
        if (ans == "pause") {
          println("Запись приостановлена пользователем. Состояние сохранено.")
          logEvent(settings, s"Запись набора ${currentManifest.name} приостановлена пользователем на носителе $mediaNum")
          return Some(currentManifest)
        }

        // Проверки носителя
        if (!os.exists(driveFolder)) {
          println(s"Ошибка: каталог носителя $driveFolder не существует! Запись приостановлена.")
          logEvent(settings, s"Ошибка: носитель ${drive.mark} недоступен")
          return Some(currentManifest)
        }
        if (drive.isProtected) {
          println(s"Ошибка: носитель ${drive.mark} защищён от записи! Запись приостановлена.")
          logEvent(settings, s"Ошибка: носитель ${drive.mark} защищён от записи")
          return Some(currentManifest)
        }

        // Нет ли на носителе чужого набора
        val descriptorPath = driveFolder / settings.mediaDescriptorFile
        if (os.exists(descriptorPath)) {
          try {
            val existingDesc = read[MediaDescriptor](os.read(descriptorPath))
            if (existingDesc.setId != currentManifest.setId) {
              println(s"Ошибка! На носителе ${drive.mark} обнаружен другой набор (${existingDesc.setName}, ID: ${existingDesc.setId})!")
              logEvent(settings, s"Ошибка: подключён носитель из другого набора (${existingDesc.setId})")
              return Some(currentManifest)
            }
          } catch { case _: Exception => () }
        }

        // Проверка свободного места для незаписанных частей
        val unwrittenParts = mediaParts.filter(!_.writtenAndVerified)
        val neededSpace = unwrittenParts.map(_.size).sum
        val availableNow = getAllowedFreeSpace(mediaRoot, drive, settings)
        if (availableNow < neededSpace) {
          val deficit = neededSpace - availableNow
          println(s"\nЗапись приостановлена. Носитель: ${drive.mark}. Не хватает места: ${formatSize(deficit)}.")
          logEvent(settings, s"Запись приостановлена на носителе ${drive.mark}: нехватка места")
          return Some(currentManifest)
        }

        // На первом носителе хранится предварительный манифест
        if (mediaNum == 1) {
          os.write.over(driveFolder / settings.manifestFile, write(currentManifest, indent = 2))
        }

        // Запись частей
        for (part <- unwrittenParts) {
          println(s"  Запись части ${part.number}: ${part.fileName} (${formatSize(part.size)})...")
          val partPath = driveFolder / part.fileName

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
              } finally {
                raf.close()
              }
            }

            val partBytes = outBuffer.toByteArray
            os.write.over(partPath, partBytes)
            val expectedHash = calculateChecksum(partBytes, settings.checkSumAlgorithm)

            // Проверка записанной части
            val verified = if (settings.verifyAfterWrite) {
              val exists = os.exists(partPath)
              val sizeOk = exists && os.size(partPath) == part.size
              val actualHash = if (exists) calculateFileChecksum(partPath, settings.checkSumAlgorithm) else ""
              exists && sizeOk && (actualHash == expectedHash)
            } else true

            if (!verified) {
              println(s"Ошибка проверки части ${part.fileName} после записи!")
              logEvent(settings, s"Ошибка верификации части ${part.fileName}")
              return Some(currentManifest)
            }


            val updatedPart = part.copy(checksum = expectedHash, writtenAndVerified = true)
            currentManifest = currentManifest.copy(
              parts = currentManifest.parts.map(p => if (p.number == part.number) updatedPart else p)
            )

            val state = TransferState(currentManifest, part.number)
            os.write.over(stateFile, write(state, indent = 2))
            logEvent(settings, s"Часть ${part.fileName} записана и проверена")

          } catch {
            case e: Exception =>
              println(s"Ошибка при записи части ${part.fileName}: ${e.getMessage}")
              logEvent(settings, s"Ошибка записи части ${part.fileName}: ${e.getMessage}")
              return Some(currentManifest)
          }
        }

        val descriptor = MediaDescriptor(
          setName = currentManifest.name,
          setId = currentManifest.setId,
          mediaNumber = mediaNum,
          totalMediaCount = currentManifest.mediaCount,
          partNumbers = mediaParts.map(_.number),
          completed = true
        )
        os.write.over(descriptorPath, write(descriptor, indent = 2))

        println(s"\nНоситель $mediaNum из ${currentManifest.mediaCount} успешно записан и проверен.")
        println(s"Можно безопасно отключить: ${drive.mark}")
        if (mediaNum < currentManifest.mediaCount) {
          val nextDrive = currentManifest.mediaOrder(mediaNum)
          println(s"Следующий носитель: ${nextDrive.mark}")
        }
        logEvent(settings, s"Носитель $mediaNum (${drive.mark}) успешно завершён")
      }
    }

    // Завершение набора и обновление манифеста
    currentManifest = currentManifest.copy(completed = true)
    val firstDriveFolder = mediaRoot / currentManifest.mediaOrder.head.path
    val finalManifestPath = if (os.exists(firstDriveFolder)) {
      val p = firstDriveFolder / settings.manifestFile
      os.write.over(p, write(currentManifest, indent = 2))
      p
    } else {
      val fallbackPath = workDir / settings.manifestFile
      os.write.over(fallbackPath, write(currentManifest, indent = 2))
      println(s"ВНИМАНИЕ: Первый носитель недоступен! Манифест сохранён в: $fallbackPath")
      fallbackPath
    }

    if (os.exists(stateFile)) os.remove(stateFile)

    printCreationReport(currentManifest, finalManifestPath)
    logEvent(settings, s"Набор ${currentManifest.name} успешно создан")
    Some(currentManifest)
  }

  def printCreationReport(manifest: ManifestData, manifestPath: os.Path): Unit = {
    val totalSize = manifest.sources.filter(!_.isDirectory).map(_.size).sum
    println(s"\n================ ОТЧЁТ О СОЗДАНИИ НАБОРА ================")
    println(s"Набор: ${manifest.name} (ID: ${manifest.setId})")
    println(s"Статус: ${if (manifest.completed) "ЗАВЕРШЁН" else "НЕ ЗАВЕРШЁН"}")
    println(s"Исходных объектов: ${manifest.sources.size} (${formatSize(totalSize)})")
    println(s"Частей: ${manifest.parts.size}")
    println(s"Носители: ${manifest.mediaOrder.map(_.mark).mkString(", ")}")
    println(s"Путь к манифесту: $manifestPath")
    println(s"=========================================================\n")
  }
}