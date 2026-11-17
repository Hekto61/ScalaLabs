import java.time.LocalDateTime
import java.util.UUID
import MyJson.*

object Planner {
  def buildPlan(setName: String, sources: List[SourceItem], selectedDrives: List[Drive], mediaRoot: os.Path, settings: AppSettings): Either[AppError, ManifestData] = {
    if (sources.isEmpty) Left(EmptySources)
    else if (selectedDrives.isEmpty) Left(EmptyDrives)
    else {
      val filesOnly = sources.filter(!_.isDirectory)
      val totalDataSize = filesOnly.map(_.size).sum
      val serviceReserve = 64L * 1024

      val driveSpaces = selectedDrives.map { d =>
        val usable = Math.max(0L, getAllowedFreeSpace(mediaRoot, d, settings) - serviceReserve)
        (d, usable)
      }
      val totalAvailable = driveSpaces.map(_._2).sum

      if (totalAvailable < totalDataSize) Left(NotEnoughSpace(totalDataSize, totalAvailable))
      else {
        var currentDriveIdx = 0
        var remainingOnDrive = driveSpaces(0)._2
        var partNumber = 1
        var parts = List.empty[PartInfo]
        var fileIdx = 0
        var currentFileOffset = 0L

        while (fileIdx < filesOnly.size) {
          while (currentDriveIdx < driveSpaces.size && remainingOnDrive <= 0) {
            currentDriveIdx += 1
            if (currentDriveIdx < driveSpaces.size) remainingOnDrive = driveSpaces(currentDriveIdx)._2
          }
          if (currentDriveIdx >= driveSpaces.size) fileIdx = filesOnly.size 
          else {
            val currentDrive = driveSpaces(currentDriveIdx)._1
            val maxCurrentPartSize = Math.min(settings.defaultPartSize, remainingOnDrive)
            var currentPartSize = 0L
            var currentPartChunks = List.empty[FileChunkRef]

            while (fileIdx < filesOnly.size && currentPartSize < maxCurrentPartSize) {
              val file = filesOnly(fileIdx)
              val fileRemaining = file.size - currentFileOffset
              if (fileRemaining == 0L) {
                fileIdx += 1
                currentFileOffset = 0L
              } else {
                val takeBytes = Math.min(fileRemaining, maxCurrentPartSize - currentPartSize)
                currentPartChunks :+= FileChunkRef(file.relativePath, currentFileOffset, takeBytes)
                currentPartSize += takeBytes
                currentFileOffset += takeBytes
                if (currentFileOffset >= file.size) { fileIdx += 1; currentFileOffset = 0L }
              }
            }
            if (currentPartChunks.nonEmpty) {
              parts :+= PartInfo(partNumber, formatPartName(settings.fileNameTemplate, setName, partNumber), currentDriveIdx + 1, currentDrive.mark, currentDrive.path, currentPartSize, chunks = currentPartChunks)
              partNumber += 1
              remainingOnDrive -= currentPartSize
            }
          }
        }

        val usedMediaCount = parts.map(_.mediaNumber).distinct.size
        val setId = UUID.randomUUID().toString.take(6)

        val sourcesWithHashesEither = sources.foldLeft[Either[AppError, List[SourceItem]]](Right(Nil)) {
          case (Right(acc), s) if !s.isDirectory && s.readable =>
            calculateFileChecksum(os.Path(s.originalPath), settings.checkSumAlgorithm).map(hash => acc :+ s.copy(checksum = hash))
          case (Right(acc), s) => Right(acc :+ s)
          case (err, _) => err
        }

        sourcesWithHashesEither.map { sourcesWithHashes =>
          ManifestData(setId, setName, LocalDateTime.now().toString, usedMediaCount, sourcesWithHashes, parts, selectedDrives.take(usedMediaCount), settings.checkSumAlgorithm, settings.preserveEmptyDirectories, completed = false)
        }
      }
    }
  }
}