import adt.AppCommand

case class CommandSpec(
  name: String,
  aliases: List[String],
  description: String,
  parser: List[String] => Either[AppError, AppCommand]
)