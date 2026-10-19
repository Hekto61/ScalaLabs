import java.nio.file.Files

object SourceManager {
  def addFile(
               currentSources: List[SourceItem],
               filePath: os.Path,
               settings: AppSettings
             ): List[SourceItem] = {
    if (!os.exists(filePath) || os.isDir(filePath)) {
      println(s"Ошибка: файл не существует ($filePath)")
      logEvent(settings, s"Ошибка: попытка добавить несуществующий файл $filePath")
      currentSources
    } else {
      val relPath = filePath.last // имя файла
      if (currentSources.exists(_.relativePath == relPath)) {
        println(s"Ошибка! Конфликт путей: объект с относительным путём '$relPath' уже есть в наборе!")
        logEvent(settings, s"Конфликт относительных путей: $relPath")
        currentSources
      } else {
        val newItem = SourceItem(
          originalPath = filePath.toString,
          relativePath = relPath,
          size = os.size(filePath),
          lastModified = os.mtime(filePath),
          readable = Files.isReadable(filePath.toNIO)
        )
        println(s"Добавлен файл: $relPath (${formatSize(newItem.size)})")
        logEvent(settings, s"Добавлен файл в набор: $filePath")
        currentSources :+ newItem
      }
    }
  }


  def addDirectory(
                    currentSources: List[SourceItem],
                    dirPath: os.Path,
                    settings: AppSettings
                  ): List[SourceItem] = {
    if (!os.exists(dirPath) || !os.isDir(dirPath)) {
      println(s"Ошибка: каталог не существует ($dirPath)")
      logEvent(settings, s"Ошибка: попытка добавить несуществующий каталог $dirPath")
      currentSources
    } else {
      val parentDir = dirPath / os.up
      var updatedSources = currentSources
      var addedCount = 0

      os.walk(dirPath).foreach { path =>
        val relPath = path.relativeTo(parentDir).toString

        if (updatedSources.exists(_.relativePath == relPath)) {
          println(s"Пропущен из-за конфликта путей: $relPath")
        } else if (os.isDir(path)) {
          if (os.list(path).isEmpty && settings.preserveEmptyDirectories) {
            updatedSources = updatedSources :+ SourceItem(
              originalPath = path.toString,
              relativePath = relPath,
              size = 0L,
              lastModified = os.mtime(path),
              readable = true,
              isDirectory = true
            )
          }
        } else {
          updatedSources = updatedSources :+ SourceItem(
            originalPath = path.toString,
            relativePath = relPath,
            size = os.size(path),
            lastModified = os.mtime(path),
            readable = Files.isReadable(path.toNIO)
          )
          addedCount += 1
        }
      }

      println(s"Из каталога '${dirPath.last}' добавлено файлов: $addedCount")
      logEvent(settings, s"Добавлен каталог $dirPath (файлов: $addedCount)")
      updatedSources
    }
  }

  def printSummary(setName: String, sources: List[SourceItem]): Unit = {
    val files = sources.filter(!_.isDirectory)
    val dirs = sources.filter(_.isDirectory)
    val totalSize = files.map(_.size).sum
    val unreadableCount = sources.count(!_.readable)
    val conflictsCount = sources.groupBy(_.relativePath).count(_._2.size > 1)

    println(s"\n=== Сводка набора: $setName ===")
    println(s"Файлов: ${files.size}")
    println(s"Пустых каталогов: ${dirs.size}")
    println(s"Общий объём: ${formatSize(totalSize)}")
    println(s"Недоступно для чтения: $unreadableCount")
    println(s"Конфликтов путей: $conflictsCount\n")

    sources.foreach { item =>
      val info = if (item.isDirectory) "[Пустая папка]" else formatSize(item.size)
      println(s"  * ${item.relativePath} ($info)")
    }
  }
}