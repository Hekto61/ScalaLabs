import java.time.LocalDateTime
import java.util.UUID

object Planner {

  def buildPlan(
                 setName: String,
                 sources: List[SourceItem],
                 selectedDrives: List[Drive],
                 mediaRoot: os.Path,
                 settings: AppSettings
               ): Option[ManifestData] = {

    if (sources.isEmpty) {
      println("Ошибка: список исходных файлов пуст!")
      return None
    }
    if (selectedDrives.isEmpty) {
      println("Ошибка: не выбрано ни одного носителя!")
      return None
    }

    val filesOnly = sources.filter(!_.isDirectory)
    val totalDataSize = filesOnly.map(_.size).sum
    val serviceReserve = 64L * 1024 // под джисоны место

    val driveSpaces = selectedDrives.map { drive =>
      val usable = Math.max(0L, getAllowedFreeSpace(mediaRoot, drive, settings) - serviceReserve)
      if (usable < settings.minimumPartSize) {
        println(s"Предупреждение: носитель ${drive.mark} не вмещает даже минимальную часть (${formatSize(settings.minimumPartSize)})!")
      }
      (drive, usable)
    }

    val totalAvailable = driveSpaces.map(_._2).sum

    if (totalAvailable < totalDataSize) {
      val deficit = totalDataSize - totalAvailable
      println(s"\nНедостаточно места для создания набора.")
      println(s"Требуется: ${formatSize(totalDataSize)}")
      println(s"Доступно с учётом резерва: ${formatSize(totalAvailable)}")
      println(s"Не хватает: ${formatSize(deficit)}")
      println("Добавьте ещё один носитель или измените состав исходных данных.\n")
      logEvent(settings, s"Ошибка планирования набора $setName: не хватает ${formatSize(deficit)}")
      return None
    }

    var currentDriveIdx = 0
    var remainingOnDrive = driveSpaces(currentDriveIdx)._2
    var partNumber = 1
    var parts = List.empty[PartInfo]

    var fileIdx = 0
    var currentFileOffset = 0L

    while (fileIdx < filesOnly.size) {
      while (currentDriveIdx < driveSpaces.size && remainingOnDrive <= 0) {
        currentDriveIdx += 1
        if (currentDriveIdx < driveSpaces.size) {
          remainingOnDrive = driveSpaces(currentDriveIdx)._2
        }
      }

      val currentDrive = driveSpaces(currentDriveIdx)._1
      // Размер части ограничен и настройкой defaultPartSize, и свободным местом на текущей флешке
      val maxCurrentPartSize = Math.min(settings.defaultPartSize, remainingOnDrive)
      var currentPartSize = 0L
      var currentPartChunks = List.empty[FileChunkRef]

      // Наполняем текущую часть кусками файлов
      while (fileIdx < filesOnly.size && currentPartSize < maxCurrentPartSize) {
        val file = filesOnly(fileIdx)
        val fileRemaining = file.size - currentFileOffset

        if (fileRemaining == 0L) {
          fileIdx += 1
          currentFileOffset = 0L
        } else {
          val spaceInPart = maxCurrentPartSize - currentPartSize
          val takeBytes = Math.min(fileRemaining, spaceInPart)

          currentPartChunks = currentPartChunks :+ FileChunkRef(
            relativePath = file.relativePath,
            fileOffset = currentFileOffset,
            length = takeBytes
          )

          currentPartSize += takeBytes
          currentFileOffset += takeBytes

          if (currentFileOffset >= file.size) {
            fileIdx += 1
            currentFileOffset = 0L
          }
        }
      }

      if (currentPartChunks.nonEmpty) {
        val partFileName = formatPartName(settings.fileNameTemplate, setName, partNumber)
        parts = parts :+ PartInfo(
          number = partNumber,
          fileName = partFileName,
          mediaNumber = currentDriveIdx + 1,
          mediaMark = currentDrive.mark,
          mediaPath = currentDrive.path,
          size = currentPartSize,
          chunks = currentPartChunks
        )
        partNumber += 1
        remainingOnDrive -= currentPartSize
      }
    }

    val usedMediaCount = parts.map(_.mediaNumber).distinct.size
    val setId = UUID.randomUUID().toString.take(6)

    // Считаем контрольные суммы SHA-256 для всех исходных файлов
    val sourcesWithHashes = sources.map { s =>
      if (!s.isDirectory && s.readable) {
        s.copy(checksum = calculateFileChecksum(os.Path(s.originalPath), settings.checkSumAlgorithm))
      } else s
    }

    val manifest = ManifestData(
      setId = setId,
      name = setName,
      createdAt = LocalDateTime.now().toString,
      mediaCount = usedMediaCount,
      sources = sourcesWithHashes,
      parts = parts,
      mediaOrder = selectedDrives.take(usedMediaCount),
      checksumAlgorithm = settings.checkSumAlgorithm,
      preserveEmptyDirectories = settings.preserveEmptyDirectories,
      completed = false
    )

    printPlan(manifest, mediaRoot, settings)
    logEvent(settings, s"Построен план для набора $setName (ID: $setId): частей ${parts.size}, носителей $usedMediaCount")
    Some(manifest)
  }

  def printPlan(manifest: ManifestData, mediaRoot: os.Path, settings: AppSettings): Unit = {
    val totalSize = manifest.sources.filter(!_.isDirectory).map(_.size).sum
    println(s"\n=== План набора ${manifest.name} (ID: ${manifest.setId}) ===")
    println(s"Исходный объём: ${formatSize(totalSize)}")
    println(s"Носителей задействовано: ${manifest.mediaCount}")
    println(s"Частей: ${manifest.parts.size}\n")

    manifest.mediaOrder.zipWithIndex.foreach { case (drive, idx) =>
      val mediaNum = idx + 1
      val mediaParts = manifest.parts.filter(_.mediaNumber == mediaNum)
      val writtenBytes = mediaParts.map(_.size).sum
      val allowedFree = getAllowedFreeSpace(mediaRoot, drive, settings)
      val remaining = Math.max(0L, allowedFree - writtenBytes)
      val partRange = if (mediaParts.nonEmpty) s"${mediaParts.head.number}–${mediaParts.last.number}" else "нет"

      println(s"Носитель $mediaNum: ${drive.mark}")
      println(s"  Части: $partRange")
      println(s"  Будет записано: ${formatSize(writtenBytes)}")
      println(s"  Останется: ${formatSize(remaining)}\n")
    }
  }
}