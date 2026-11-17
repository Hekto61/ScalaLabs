import MyJson.*
import scala.io.StdIn.readLine
import adt._


object Repl {
  def run(st: AppState): Unit = {
    val registry = new CommandRegistry
    logEvent(st.settings, "Приложение запущено")
    printBanner(st)
    warnUnfinished(st)

    while (st.running) {
      print(s"\n[${st.currentSetName}]> ")
      val raw = Option(readLine()).getOrElse("exit").trim

      if (raw.nonEmpty) {
        val parts = raw.split("\\s+").toList
        val cmdStr = parts.head.toLowerCase
        val args = parts.tail

        registry.find(cmdStr) match {
          case None =>
            println(s"Неизвестная команда: '$cmdStr'. Введите 'help'.")

          case Some(spec) =>
            spec.parser(args) match {
              case Left(err) => println(formatError(err))
              case Right(CmdHelp) => registry.printHelp()
              case Right(CmdWrite) => handleWrite(st)
              case Right(CmdResume) => handleResume(st)
              case Right(cmd) =>
                val onConflict: os.Path => Boolean = { path =>
                  print(s"Файл $path уже существует. Перезаписать? (y/n): ")
                  Set("y", "yes", "д").contains(readLine().trim.toLowerCase)
                }
                CommandExecutor.execute(cmd, st, onConflict) match {
                  case Right(msg) => if (msg.nonEmpty) println(msg)
                  case Left(err)  => println(formatError(err))
                }
            }
        }
      }
    }
  }

  private def handleWrite(st: AppState): Unit = {
    val planEither = st.currentPlan match {
      case Some(plan) => Right(plan)
      case None       => Planner.buildPlan(st.currentSetName, st.sources, st.selectedDrives, st.mediaRoot, st.settings)
    }

    planEither match {
      case Left(err) => println(formatError(err))
      case Right(manifest) =>
        DataWriter.validateSources(manifest, st.settings) match {
          case Left(err) => println(formatError(err))
          case Right(_) =>
            print("Начать запись? (y/n): ")
            if (Set("y", "yes", "д").contains(readLine().trim.toLowerCase)) {
              executeWriteLoop(manifest, st)
            }
        }
    }
  }

  private def executeWriteLoop(initialManifest: ManifestData, st: AppState): Unit = {
    var manifest = initialManifest
    val unwritten = manifest.parts.filter(!_.writtenAndVerified).map(_.mediaNumber).distinct.sorted
    val workDir = os.pwd / st.settings.workDirectory
    os.makeDir.all(workDir)
    var paused = false
    val it = unwritten.iterator

    while (it.hasNext && !paused) {
      val mediaNum = it.next()
      val drive = manifest.mediaOrder(mediaNum - 1)
      println(s"\nПодготовьте носитель $mediaNum: ${drive.mark} (${drive.path})")
      print("Нажмите Enter для продолжения (или 'pause'): ")

      if (readLine().trim.toLowerCase == "pause") {
        println("Запись приостановлена.")
        st.currentPlan = Some(manifest)
        paused = true
      } else {
        DataWriter.prepareMedia(mediaNum, manifest, st.mediaRoot, st.settings) match {
          case Left(err) => println(formatError(err)); paused = true // сделать отдельно
          case Right(_) =>
            val partsToWrite = manifest.parts.filter(p => p.mediaNumber == mediaNum && !p.writtenAndVerified)
            val sourceMap = manifest.sources.map(s => s.relativePath -> s).toMap
            val driveFolder = st.mediaRoot / drive.path

            val partIt = partsToWrite.iterator
            while (partIt.hasNext && !paused) {
              val part = partIt.next()
              DataWriter.writePart(part, sourceMap, driveFolder, st.settings) match {
                case Left(err) => println(formatError(err)); paused = true
                case Right(updatedPart) =>
                  manifest = manifest.copy(parts = manifest.parts.map(p => if (p.number == part.number) updatedPart else p))
                  val state = TransferState(manifest, part.number)
                  os.write.over(workDir / "transfer-state.json", write(state, indent = 2))
                  println(s"Часть ${part.fileName} записана.")
              }
            }
            if (!paused) {
              val allMediaPartNumbers = manifest.parts.filter(_.mediaNumber == mediaNum).map(_.number)
              val descriptor = MediaDescriptor(
                setName = manifest.name,
                setId = manifest.setId,
                mediaNumber = mediaNum,
                totalMediaCount = manifest.mediaCount,
                partNumbers = allMediaPartNumbers,
                completed = true
              )
              os.write.over(driveFolder / st.settings.mediaDescriptorFile, write(descriptor, indent = 2))
              println(s"Носитель $mediaNum успешно записан.")
            }
        }
      }
    }

    if (!paused) {
      manifest = manifest.copy(completed = true)
      st.currentPlan = Some(manifest)
      val firstDrive = st.mediaRoot / manifest.mediaOrder.head.path
      os.write.over(firstDrive / st.settings.manifestFile, write(manifest, indent = 2))
      if (os.exists(workDir / "transfer-state.json")) {
        os.remove(workDir / "transfer-state.json")
      }
      println("Набор успешно завершён!")
      logEvent(st.settings, s"Набор ${manifest.name} успешно создан")
    }
  }

  private def handleResume(st: AppState): Unit = {
    val f = os.pwd / st.settings.workDirectory / "transfer-state.json"
    if (!os.exists(f)) println("Прерванных операций нет.")
    else {
      try {
        val saved = read[TransferState](os.read(f))
        println(s"Продолжение записи набора ${saved.manifest.name}...")
        executeWriteLoop(saved.manifest, st)
      } catch { case e: Exception => println(s"Ошибка состояния: ${e.getMessage}") }
    }
  }

  private def printBanner(st: AppState): Unit = {
    val code = 9760
    println("==================================================================")
    println(f"    Распределение файлов по нескольким носителям (${code.toChar}%c Дэмо ${code.toChar}%c)")
    println("==================================================================")
    println(s"Загружено виртуальных носителей: ${st.catalogue.drives.size} (папка: ${st.mediaRoot})")
    println("Введите 'help' для просмотра всех доступных команд.")
  }

  private def warnUnfinished(st: AppState): Unit = {
    val f = os.pwd / st.settings.workDirectory / "transfer-state.json"
    if (os.exists(f)) try {
      val s = read[TransferState](os.read(f))
      println(s"\n[!] Обнаружена незавершённая операция:")
      println(s"    Набор: ${s.manifest.name} (ID: ${s.manifest.setId})")
      println(s"    Последняя проверенная часть: ${s.lastVerifiedPart} из ${s.manifest.parts.size}")
      println(s"    Введите 'resume', чтобы продолжить запись!")
    } catch { case _: Exception => () }
  }

  def formatError(err: AppError): String = err match {
    case FileNotFound(p) => s"Ошибка: файл не найден ($p)"
    case DirNotFound(p) => s"Ошибка: каталог не найден ($p)"
    case PathIsDir(p) => s"Ошибка: путь является каталогом ($p)"
    case PathConflict(p) => s"Ошибка: конфликт путей ('$p' уже есть в наборе)"
    case EmptySources => "Ошибка: список файлов пуст."
    case EmptyDrives => "Ошибка: не выбрано ни одного носителя."
    case NotEnoughSpace(req, av) => s"Ошибка: не хватает места (требуется ${formatSize(req)}, доступно ${formatSize(av)})."
    case DriveNotFound(m) => s"Ошибка: носитель '$m' не найден."
    case DriveProtected(m) => s"Ошибка: носитель '$m' защищён."
    case ForeignSetFound(m, id) => s"Ошибка: на носителе '$m' уже есть другой набор (ID: $id)."
    case ManifestNotFound(p) => s"Ошибка: манифест не найден ($p)."
    case ManifestNotCompleted => "Ошибка: набор не завершён."
    case SourcesChanged => "Ошибка: исходные файлы изменились после планирования. Перестройте план."
    case VerificationFailed(msg) => s"Ошибка проверки: $msg"
    case ParseError(msg) => s"Ошибка ввода: $msg"
    case IoError(msg) => s"Системная ошибка: $msg"
  }
}