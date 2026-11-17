import MyJson.*
import adt._

object CommandExecutor {
  def execute(cmd: AppCommand, st: AppState, onConflict: os.Path => Boolean): Either[AppError, String] = cmd match {
    case CmdHelp => Right("")
    
    case CmdCreateSet(name) =>
      st.currentSetName = name; st.sources = List.empty; st.currentPlan = None
      logEvent(st.settings, s"Создан новый набор: $name")
      Right(s"Создан набор: $name")
      
    case CmdReload =>
      st.reload().map { _ =>
        logEvent(st.settings, "Перезагружена конфигурация")
        "Конфигурация перезагружена."
      }
      
    case CmdExit =>
      st.running = false
      logEvent(st.settings, "Работа завершена")
      Right("До свидания!")
      
    case CmdAddFile(path) =>
      val p = PathUtil.resolve(path)
      SourceManager.addFile(st.sources, p, st.settings).map { item =>
        st.sources :+= item
        logEvent(st.settings, s"Добавлен файл: $path")
        s"Добавлен файл: ${item.relativePath}"
      }
      
    case CmdAddDir(path) =>
      val p = PathUtil.resolve(path)
      SourceManager.addDirectory(st.sources, p, st.settings).map { items =>
        val added = items.size - st.sources.size
        st.sources = items
        logEvent(st.settings, s"Добавлен каталог: $path")
        s"Из каталога '${p.last}' добавлено объектов: $added. Всего в наборе: ${items.size}"
      }
      
    case CmdShowSources =>
      val files = st.sources.filter(!_.isDirectory)
      val dirs = st.sources.filter(_.isDirectory)
      val totalSize = files.map(_.size).sum
      val unreadableCount = st.sources.count(!_.readable)
      val conflictsCount = st.sources.groupBy(_.relativePath).count(_._2.size > 1)
      val summary = s"""
        |=== Сводка набора: ${st.currentSetName} ===
        |Файлов: ${files.size}
        |Пустых каталогов: ${dirs.size}
        |Общий объём: ${formatSize(totalSize)}
        |Недоступно для чтения: $unreadableCount
        |Конфликтов путей: $conflictsCount
        |
        |${st.sources.map(s => s"  * ${s.relativePath} (${if (s.isDirectory) "[Пустая папка]" else formatSize(s.size)})").mkString("\n")}
        |""".stripMargin
      Right(summary)
      
    case CmdClearSources =>
      st.sources = List.empty; st.currentPlan = None
      Right("Список файлов очищен.")
      
    case CmdDemoFiles =>
      val demoDir = os.pwd / "demo_source"
      os.makeDir.all(demoDir / "Documents" / "Tables")
      os.makeDir.all(demoDir / "Photos")
      os.makeDir.all(demoDir / "EmptyFolder")
      val rnd = new scala.util.Random(42)
      os.write.over(demoDir / "Documents" / "report.docx",            rnd.nextBytes(3 * 1024 * 1024))
      os.write.over(demoDir / "Documents" / "Tables" / "budget.xlsx", rnd.nextBytes(4 * 1024 * 1024))
      os.write.over(demoDir / "Photos"    / "vacationVideo.mp4",      rnd.nextBytes(9 * 1024 * 1024))
      Right(s"Тестовые данные созданы в $demoDir (~16 МБ). Выполните: add-directory demo_source")
      
    case CmdShowMedia =>
      val info = st.selectedDrives.zipWithIndex.map { case (d, i) =>
        val phys = getPhysicalFreeSpace(st.mediaRoot, d)
        val allowed = getAllowedFreeSpace(st.mediaRoot, d, st.settings)
        s"${i + 1}. ${d.mark} (папка: ${d.path}, всего: ${formatSize(d.volume)}, свободно: ${formatSize(phys)}, доступно с резервом: ${formatSize(allowed)})"
      }.mkString("\n")
      Right(s"Текущий порядок носителей:\n$info")
      
    case CmdAddMedia(mark) =>
      st.catalogue.drives.find(_.mark.equalsIgnoreCase(mark)) match {
        case Some(d) if st.selectedDrives.exists(_.id == d.id) => Left(ParseError("Носитель уже в списке."))
        case Some(d) => st.selectedDrives :+= d; Right(s"Носитель ${d.mark} добавлен.")
        case None => Left(DriveNotFound(mark))
      }
      
    case CmdRemoveMedia(idx) =>
      if (idx >= 1 && idx <= st.selectedDrives.size) {
        val rem = st.selectedDrives(idx - 1)
        st.selectedDrives = st.selectedDrives.patch(idx - 1, Nil, 1)
        Right(s"Носитель ${rem.mark} удалён.")
      } else Left(ParseError("Неверный номер носителя."))
      
    case CmdMoveMedia(from, to) =>
      if (from >= 1 && from <= st.selectedDrives.size && to >= 1 && to <= st.selectedDrives.size) {
        val item = st.selectedDrives(from - 1)
        st.selectedDrives = st.selectedDrives.patch(from - 1, Nil, 1).patch(to - 1, List(item), 0)
        Right(s"Носитель ${item.mark} перемещён на позицию $to.")
      } else Left(ParseError("Неверные индексы."))
      
    case CmdLimitMedia(idx, mb) =>
      if (idx >= 1 && idx <= st.selectedDrives.size) {
        val d = st.selectedDrives(idx - 1)
        st.selectedDrives = st.selectedDrives.updated(idx - 1, d.copy(maxAllowedOverride = mb * 1024L * 1024L))
        Right(s"Лимит для ${d.mark} установлен: $mb МБ")
      } else Left(ParseError("Неверный номер носителя."))
      
    case CmdPlan =>
      Planner.buildPlan(st.currentSetName, st.sources, st.selectedDrives, st.mediaRoot, st.settings).map { m =>
        st.currentPlan = Some(m)
        logEvent(st.settings, s"Построен план набора ${m.name}")
        s"План построен. Частей: ${m.parts.size}, Носителей: ${m.mediaCount}"
      }
      
    case CmdManifest =>
      val mPath = st.manifestPath
      if (!os.exists(mPath)) Left(ManifestNotFound(mPath.toString))
      else try {
        val m = read[ManifestData](os.read(mPath))
        val status = if (m.completed) "Завершён" else "Не завершён"
        val parts = m.parts.map(p => s"  ${p.fileName} | Носитель: ${p.mediaMark} | Размер: ${formatSize(p.size)} | SHA-256: ${p.checksum.take(10)}...").mkString("\n")
        Right(s"=== Манифест: ${m.name} (ID: ${m.setId}) ===\nСтатус: $status\nНосителей: ${m.mediaCount}\nЧастей: ${m.parts.size}\n$parts")
      } catch { case _: Exception => Left(IoError("Ошибка чтения манифеста!")) }
      
    case CmdVerify =>
      val mPath = st.manifestPath
      if (!os.exists(mPath)) Left(ManifestNotFound(mPath.toString))
      else try {
        val m = read[ManifestData](os.read(mPath))
        Restorer.verifySet(m, st.mediaRoot, st.settings).map { _ =>
          logEvent(st.settings, s"Набор ${m.name} успешно проверен")
          "Все части успешно проверены! Набор цел."
        }
      } catch { case _: Exception => Left(IoError("Ошибка чтения манифеста!")) }
      
    case CmdRestore(dir) =>
      val mPath = st.manifestPath
      if (!os.exists(mPath)) Left(ManifestNotFound(mPath.toString))
      else try {
        val m = read[ManifestData](os.read(mPath))
        val target = PathUtil.resolve(dir)
        Restorer.restoreSet(m, target, st.mediaRoot, st.settings, onConflict).map { _ =>
          logEvent(st.settings, s"Набор ${m.name} восстановлен в $target")
          s"Восстановление завершено в $target"
        }
      } catch { case _: Exception => Left(IoError("Ошибка чтения манифеста!")) }
      
    case _ => Left(ParseError("Команда не реализована в Executor."))
  }
}