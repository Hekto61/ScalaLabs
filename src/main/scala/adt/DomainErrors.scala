sealed trait AppError

case class FileNotFound(path: String) extends AppError
case class DirNotFound(path: String) extends AppError
case class PathIsDir(path: String) extends AppError
case class PathConflict(path: String) extends AppError
case object EmptySources extends AppError
case object EmptyDrives extends AppError
case class NotEnoughSpace(required: Long, available: Long) extends AppError
case class DriveNotFound(mark: String) extends AppError
case class DriveProtected(mark: String) extends AppError
case class ForeignSetFound(mark: String, setId: String) extends AppError
case class ManifestNotFound(path: String) extends AppError
case object ManifestNotCompleted extends AppError
case object SourcesChanged extends AppError
case class VerificationFailed(msg: String) extends AppError
case class ParseError(msg: String) extends AppError
case class IoError(msg: String) extends AppError