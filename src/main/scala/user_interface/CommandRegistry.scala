import adt._

class CommandRegistry {
  private def noArg(name: String, aliases: List[String], desc: String, cmd: AppCommand) =
    CommandSpec(name, aliases, desc, _ => Right(cmd))

  private def oneArg(name: String, aliases: List[String], desc: String, f: String => AppCommand) =
    CommandSpec(name, aliases, desc, args => 
      if (args.nonEmpty) Right(f(args.mkString(" "))) 
      else Left(ParseError(s"Ожидался аргумент для $name"))
    )

  val commands: List[CommandSpec] = List(
    noArg("help",           List("menu"), "Справка",                      CmdHelp),
    oneArg("create-set",    Nil,          "Создать набор: <имя>",         CmdCreateSet.apply),
    oneArg("add-file",      Nil,          "Добавить файл: <путь>",        CmdAddFile.apply),
    oneArg("add-directory", Nil,          "Добавить каталог: <путь>",     CmdAddDir.apply),
    noArg("sources",        Nil,          "Сводка набора",                CmdShowSources),
    noArg("clear-sources",  Nil,          "Очистить список",              CmdClearSources),
    noArg("media",          Nil,          "Список носителей",             CmdShowMedia),
    oneArg("add-media",     Nil,          "Добавить носитель: <имя>",     CmdAddMedia.apply),
    
    CommandSpec("remove-media", Nil, "Убрать носитель: <номер>", args =>
      args.headOption.flatMap(_.toIntOption).map(CmdRemoveMedia)
        .toRight(ParseError("Ожидался номер носителя"))
    ),
    
    CommandSpec("move-media", Nil, "Переместить: <от> <куда>", {
      case from :: to :: Nil =>
        for {
          f <- from.toIntOption.toRight(ParseError("Ожидался номер"))
          t <- to.toIntOption.toRight(ParseError("Ожидался номер"))
        } yield CmdMoveMedia(f, t)
      case _ => Left(ParseError("Формат: move-media <от> <куда>"))
    }),
    
    CommandSpec("limit-media", Nil, "Лимит: <ном> <МБ>", {
      case idx :: mb :: Nil =>
        for {
          i <- idx.toIntOption.toRight(ParseError("Ожидался номер"))
          m <- mb.toLongOption.toRight(ParseError("Ожидался размер"))
        } yield CmdLimitMedia(i, m)
      case _ => Left(ParseError("Формат: limit-media <ном> <МБ>"))
    }),

    noArg("plan",           Nil,          "План записи",                  CmdPlan),
    noArg("write",          Nil,          "Начать запись",                CmdWrite),
    noArg("resume",         Nil,          "Продолжить запись",            CmdResume),
    noArg("verify",         Nil,          "Проверить набор",              CmdVerify),
    oneArg("restore",       Nil,          "Восстановить: <папка>",        CmdRestore.apply),
    noArg("manifest",       Nil,          "Показать манифест",            CmdManifest),
    noArg("reload",         Nil,          "Перезагрузить конфиг",         CmdReload),
    noArg("demo-files",     Nil,          "Создать демо-файлы",           CmdDemoFiles),
    noArg("exit",           List("quit"), "Выход",                        CmdExit)
  )

  def printHelp(): Unit = {
    println("Доступные команды:")
    commands.zipWithIndex.foreach { case (c, i) =>
      println(f"  ${i + 1}%2d. ${c.name}%-16s — ${c.description}")
    }
    println("  (также можно ввести номер команды)")
  }

  def find(cmdStr: String): Option[CommandSpec] =
    commands.find(c => c.name == cmdStr || c.aliases.contains(cmdStr))
      .orElse(cmdStr.toIntOption.filter(n => n >= 1 && n <= commands.size).map(n => commands(n - 1)))
}