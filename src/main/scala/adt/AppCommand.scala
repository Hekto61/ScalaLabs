package adt
sealed trait AppCommand

// System
case class CmdCreateSet(name: String) extends AppCommand
case object CmdHelp extends AppCommand
case object CmdReload extends AppCommand
case object CmdExit extends AppCommand

// Sources
case class CmdAddFile(path: String) extends AppCommand
case class CmdAddDir(path: String) extends AppCommand
case object CmdShowSources extends AppCommand
case object CmdClearSources extends AppCommand
case object CmdDemoFiles extends AppCommand

// Media
case object CmdShowMedia extends AppCommand
case class CmdAddMedia(mark: String) extends AppCommand
case class CmdRemoveMedia(idx: Int) extends AppCommand
case class CmdMoveMedia(from: Int, to: Int) extends AppCommand
case class CmdLimitMedia(idx: Int, mb: Long) extends AppCommand

// Transfer & Restore
case object CmdPlan extends AppCommand
case object CmdWrite extends AppCommand
case object CmdResume extends AppCommand
case object CmdVerify extends AppCommand
case class CmdRestore(dir: String) extends AppCommand
case object CmdManifest extends AppCommand