import java.nio.file.Files

object SourceManager {
  def addFile(currentSources: List[SourceItem], filePath: os.Path, settings: AppSettings): Either[AppError, SourceItem] = {
    if (!os.exists(filePath)) Left(FileNotFound(filePath.toString))
    else if (os.isDir(filePath)) Left(PathIsDir(filePath.toString))
    else {
      val relPath = filePath.last
      if (currentSources.exists(_.relativePath == relPath)) Left(PathConflict(relPath))
      else {
        try {
          Right(SourceItem(
            originalPath = filePath.toString,
            relativePath = relPath,
            size = os.size(filePath),
            lastModified = os.mtime(filePath),
            readable = Files.isReadable(filePath.toNIO)
          ))
        } catch { case e: Exception => Left(IoError(e.getMessage)) }
      }
    }
  }

  def addDirectory(currentSources: List[SourceItem], dirPath: os.Path, settings: AppSettings): Either[AppError, List[SourceItem]] = {
    if (!os.exists(dirPath) || !os.isDir(dirPath)) Left(DirNotFound(dirPath.toString))
    else {
      try {
        val parentDir = dirPath / os.up
        val newSources = os.walk(dirPath).toList.flatMap { path =>
          val relPath = path.relativeTo(parentDir).toString
          if (currentSources.exists(_.relativePath == relPath)) None
          else if (os.isDir(path)) {
            if (os.list(path).isEmpty && settings.preserveEmptyDirectories)
              Some(SourceItem(path.toString, relPath, 0L, os.mtime(path), readable = true, isDirectory = true))
            else None
          } else {
            Some(SourceItem(path.toString, relPath, os.size(path), os.mtime(path), Files.isReadable(path.toNIO)))
          }
        }
        Right(currentSources ++ newSources)
      } catch { case e: Exception => Left(IoError(s"Ошибка обхода каталога: ${e.getMessage}")) }
    }
  }
}