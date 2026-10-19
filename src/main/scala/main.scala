import MyJson.*
import scala.io.StdIn.readLine

@main
def main(): Unit = {
  val configPath = os.pwd / "src" / "main" / "scala" / "config.json"
  val mediaRoot = os.pwd / "src" / "main" / "scala" / "virtual_media"

  var settings = loadOrCreateSettings(configPath)
  var catalogue = loadOrCreateCatalogue(mediaRoot)
  logEvent(settings, "Приложение запущено")

  // Текущее состояние работы пользователя
  var currentSetName: String = "Пользователь"
  var sources: List[SourceItem] = List.empty
  var selectedDrives: List[Drive] = catalogue.drives
  var currentPlan: Option[ManifestData] = None

  println("==================================================================")
  println("    Распределение файлов по нескольким носителям (Вариант 7)")
  println("==================================================================")
  println(s"Загружено виртуальных носителей: ${catalogue.drives.size} (папка: $mediaRoot)")
  println("Введите 'help' для просмотра всех доступных команд.")

  // Проверка нет ли прерванной записи с прошлого раза
  val stateFile = os.pwd / settings.workDirectory / "transfer-state.json"
  if (os.exists(stateFile)) {
    try {
      val savedState = read[TransferState](os.read(stateFile))
      println(s"\n[!] Обнаружена незавершённая операция:")
      println(s"    Набор: ${savedState.manifest.name} (ID: ${savedState.manifest.setId})")
      println(s"    Последняя проверенная часть: ${savedState.lastVerifiedPart} из ${savedState.manifest.parts.size}")
      println(s"    Введите 'resume', чтобы продолжить запись!")
    } catch { case _: Exception => () }
  }

  var running = true
  while (running) {
    print(s"\n[$currentSetName]> ")
    val rawInput = Option(readLine()).getOrElse("exit").trim
    val parts = rawInput.split("\\s+", 2)
    val cmd = parts(0).toLowerCase
    val arg = if (parts.length > 1) parts(1).trim else ""

    cmd match {
      case "help" | "menu" =>
        println(
          """Доступные команды:
            |  create-set <имя>        - Создать новый переносимый набор
            |  add-file <путь>         - Добавить отдельный файл в набор
            |  add-directory <путь>    - Добавить каталог со всеми вложенными файлами
            |  sources                 - Показать сводку и список выбранных файлов
            |  clear-sources           - Очистить список файлов
            |  media                   - Показать список носителей и их порядок
            |  add-media <имя/id>      - Добавить носитель из каталога в порядок использования
            |  remove-media <номер>    - Убрать носитель из порядка
            |  move-media <от> <куда>  - Изменить позицию носителя (например: move-media 2 1)
            |  limit-media <ном> <МБ>  - Ограничить доступный объём носителя вручную (в МБ)
            |  plan                    - Построить план распределения без записи
            |  write                   - Начать запись по плану
            |  resume                  - Продолжить прерванную запись
            |  verify                  - Проверить записанный набор по манифесту
            |  restore <папка>         - Восстановить файлы в указанную папку
            |  manifest                - Просмотреть информацию из манифеста
            |  reload                  - Перезагрузить config.json и drives.json
            |  demo-files              - Автоматически создать тестовую папку с файлами для быстрой проверки
            |  exit                    - Завершить работу""".stripMargin
        )

      case "create-set" =>
        if (arg.isEmpty) println("Укажите имя набора: create-set <имя>")
        else {
          currentSetName = arg
          sources = List.empty
          currentPlan = None
          println(s"Создан новый набор: $currentSetName")
          logEvent(settings, s"Создан новый набор: $currentSetName")
        }

      case "demo-files" =>
        // Тестовые файлы (~16 МБ)
        val demoDir = os.pwd / "demo_source"
        os.makeDir.all(demoDir / "Documents" / "Tables")
        os.makeDir.all(demoDir / "Photos")
        os.makeDir.all(demoDir / "EmptyFolder")
        val rnd = new scala.util.Random(42)
        os.write.over(demoDir / "Documents" / "report.docx", rnd.nextBytes(3 * 1024 * 1024))
        os.write.over(demoDir / "Documents" / "Tables" / "budget.xlsx", rnd.nextBytes(4 * 1024 * 1024))
        os.write.over(demoDir / "Photos" / "vacationVideo.mp4", rnd.nextBytes(9 * 1024 * 1024))
        println(s"Тестовые данные созданы в $demoDir (~16 МБ).")
        println("Выполните: add-directory demo_source")

      case "add-file" =>
        if (arg.isEmpty) println("Использование: add-file <путь>")
        else {
          val p = if (java.nio.file.Paths.get(arg).isAbsolute) os.Path(arg) else os.pwd / os.RelPath(arg)
          sources = SourceManager.addFile(sources, p, settings)
        }

      case "add-directory" =>
        if (arg.isEmpty) println("Использование: add-directory <путь>")
        else {
          val p = if (java.nio.file.Paths.get(arg).isAbsolute) os.Path(arg) else os.pwd / os.RelPath(arg)
          sources = SourceManager.addDirectory(sources, p, settings)
        }

      case "sources" =>
        SourceManager.printSummary(currentSetName, sources)

      case "clear-sources" =>
        sources = List.empty
        currentPlan = None
        println("Список файлов очищен.")

      case "media" =>
        println("\nТекущий порядок носителей:")
        selectedDrives.zipWithIndex.foreach { case (d, i) =>
          val phys = getPhysicalFreeSpace(mediaRoot, d)
          val allowed = getAllowedFreeSpace(mediaRoot, d, settings)
          println(s"${i + 1}. ${d.mark} (папка: ${d.path}, всего: ${formatSize(d.volume)}, свободно: ${formatSize(phys)}, доступно с резервом: ${formatSize(allowed)})")
        }

      case "add-media" =>
        if (arg.isEmpty) println("Использование: add-media <имя носителя>")
        else {
          catalogue.drives.find(_.mark.equalsIgnoreCase(arg)) match {
            case Some(drive) =>
              if (selectedDrives.exists(_.id == drive.id)) println("Носитель уже в списке.")
              else {
                selectedDrives = selectedDrives :+ drive
                println(s"Носитель ${drive.mark} добавлен.")
              }
            case None => println(s"Носитель '$arg' не найден в drives.json.")
          }
        }

      case "remove-media" =>
        arg.toIntOption match {
          case Some(idx) if idx >= 1 && idx <= selectedDrives.size =>
            val rem = selectedDrives(idx - 1)
            selectedDrives = selectedDrives.patch(idx - 1, Nil, 1)
            println(s"Носитель ${rem.mark} удалён.")
          case _ => println("Неверный номер носителя.")
        }

      case "move-media" =>
        val nums = arg.split("\\s+").flatMap(_.toIntOption)
        if (nums.length != 2 || nums.exists(n => n < 1 || n > selectedDrives.size)) {
          println("Использование: move-media <откуда_номер> <куда_номер>")
        } else {
          val fromIdx = nums(0) - 1
          val toIdx = nums(1) - 1
          val item = selectedDrives(fromIdx)
          selectedDrives = selectedDrives.patch(fromIdx, Nil, 1).patch(toIdx, List(item), 0)
          println(s"Носитель ${item.mark} перемещён на позицию ${toIdx + 1}.")
        }

      case "limit-media" =>
        val tokens = arg.split("\\s+")
        if (tokens.length != 2 || tokens(0).toIntOption.isEmpty || tokens(1).toLongOption.isEmpty) {
          println("Использование: limit-media <номер_носителя> <лимит_в_МБ>")
        } else {
          val idx = tokens(0).toInt - 1
          val mb = tokens(1).toLong * 1024L * 1024L
          if (idx >= 0 && idx < selectedDrives.size) {
            val d = selectedDrives(idx)
            selectedDrives = selectedDrives.updated(idx, d.copy(maxAllowedOverride = mb))
            println(s"Лимит для ${d.mark} установлен: ${formatSize(mb)}")
          }
        }

      case "plan" =>
        currentPlan = Planner.buildPlan(currentSetName, sources, selectedDrives, mediaRoot, settings)

      case "write" =>
        val planToUse = currentPlan.orElse(Planner.buildPlan(currentSetName, sources, selectedDrives, mediaRoot, settings))
        planToUse match {
          case Some(plan) =>
            print("Начать запись? (y/n): ")
            val ans = readLine().trim.toLowerCase
            if (ans == "y" || ans == "yes" || ans == "д") {
              currentPlan = DataWriter.executeWrite(plan, mediaRoot, settings)
            }
          case None => ()
        }

      case "resume" =>
        val sFile = os.pwd / settings.workDirectory / "transfer-state.json"
        if (!os.exists(sFile)) {
          println("Незавершённых операций записи не обнаружено.")
        } else {
          try {
            val savedState = read[TransferState](os.read(sFile))
            println(s"Продолжение записи набора ${savedState.manifest.name}...")
            currentPlan = DataWriter.executeWrite(savedState.manifest, mediaRoot, settings)
          } catch {
            case e: Exception => println(s"Ошибка чтения состояния: ${e.getMessage}")
          }
        }

      case "manifest" =>
        val mPath = mediaRoot / selectedDrives.head.path / settings.manifestFile
        if (!os.exists(mPath)) println(s"Манифест не найден: $mPath")
        else {
          try {
            val m = read[ManifestData](os.read(mPath))
            println(s"\n=== Манифест: ${m.name} (ID: ${m.setId}) ===")
            println(s"Статус: ${if (m.completed) "Завершён" else "Не завершён"}")
            println(s"Носителей: ${m.mediaCount}")
            println(s"Частей: ${m.parts.size}")
            m.parts.foreach { p =>
              println(s"  ${p.fileName} | Носитель: ${p.mediaMark} | Размер: ${formatSize(p.size)} | SHA-256: ${p.checksum.take(10)}...")
            }
          } catch {
            case _: Exception => println("Ошибка чтения манифеста!")
          }
        }

      case "verify" =>
        val mPath = mediaRoot / selectedDrives.head.path / settings.manifestFile
        if (!os.exists(mPath)) println("Манифест на первом носителе не найден!")
        else {
          try {
            val m = read[ManifestData](os.read(mPath))
            Restorer.verifySet(m, mediaRoot, settings)
          } catch {
            case _: Exception => println("Ошибка чтения манифеста!")
          }
        }

      case "restore" =>
        if (arg.isEmpty) println("Использование: restore <каталог_восстановления>")
        else {
          val targetDir = if (java.nio.file.Paths.get(arg).isAbsolute) os.Path(arg) else os.pwd / os.RelPath(arg)
          val mPath = mediaRoot / selectedDrives.head.path / settings.manifestFile
          if (!os.exists(mPath)) println("Манифест на первом носителе не найден!")
          else {
            try {
              val m = read[ManifestData](os.read(mPath))
              Restorer.restoreSet(m, targetDir, mediaRoot, settings)
            } catch {
              case _: Exception => println("Ошибка чтения манифеста!")
            }
          }
        }

      case "reload" =>
        settings = loadOrCreateSettings(configPath)
        catalogue = loadOrCreateCatalogue(mediaRoot)
        selectedDrives = catalogue.drives
        println("Конфигурация и список носителей перезагружены.")
        logEvent(settings, "Перезагружена конфигурация приложения")

      case "exit" | "quit" =>
        logEvent(settings, "Работа завершена")
        println("До свидания!")
        running = false

      case "" => ()

      case unknown =>
        println(s"Неизвестная команда: '$unknown'. Введите 'help' для справки.")
    }
  }
}
