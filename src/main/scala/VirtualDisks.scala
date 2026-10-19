object MyJson extends upickle.AttributeTagged {
  override def serializeDefaults: Boolean = true
}
import MyJson.*

//import upickle.default.*
//import os.*

case class Drive (
                   id: Int,
                   mark: String = "BLUE-USB",
                   path: String,
                   volume: Long = 2L * 1024 * 1024 * 1024, //2 гигабайта
                   isProtected: Boolean = false,
                   maxAllowedOverride: Long = -1L
                 )derives ReadWriter

case class Catalogue(
                      drives: List[Drive]
                )derives ReadWriter


val drives = List(
  Drive(id = 1, mark = "BLUE-USB", path = "usb1", volume = 2L * 1024 * 1024 * 1024),
  Drive(id = 2, mark = "OLD-FLASH", path = "usb2", volume = 1L * 1024 * 1024 * 1024),
  Drive(id = 3, mark = "BACKUP-SD", path = "usb3", volume = 4L * 1024 * 1024 * 1024),
  Drive(id = 4, mark = "RED-USB", path = "usb4", volume = 16L * 1024 * 1024 * 1024)
)

//drives.foreach { drive =>
//  val folderPath =  os.pwd / "src" / "main" / "scala" / "virtual_media" / drive.path
//  os.makeDir.all(folderPath)
//  println(s"Папка для носителя ${drive.mark} готово: $folderPath")
//}

case class Archive(
                    enabled: Boolean = false,
                    command: String = "7z",
                    format: String = "7z",
                    compressionLevel: Int = 0
                  )derives ReadWriter

case class AppSettings(
                      workDirectory: String = "transfer-work",
                      logFile: String = "transfer-manager.log",
                      defaultPartSize: Long = 2L * 1024 * 1024 * 1024, //2 гигабайта
                      minimumPartSize: Long = 64L * 1024 * 1024, // 64 Mbit
                      reservedSpacePerMedia: Long = 500L * 1024 * 1024, //500 Mbit
                      verifyAfterWrite: Boolean = true,
                      checkSumAlgorithm: String = "SHA-256",
                      overrideMode: String = "ask",
                      preserveEmptyDirectories: Boolean = true,
                      archive: Archive = Archive(),
                      fileNameTemplate: String = "{setName}.part{number}",
                      mediaDescriptorFile: String = "media-info.json",
                      manifestFile: String = "manifest.json"
                      )derives ReadWriter



//кусочки файликов
case class SourceItem(
                       originalPath: String,
                       relativePath: String,
                       size: Long,
                       lastModified: Long,
                       readable: Boolean,
                       isDirectory: Boolean = false,
                       checksum: String = ""
                     )derives ReadWriter


case class FileChunkRef(
                         relativePath: String, // Какой это файл (относительный путь)
                         fileOffset: Long,     // С какого байта внутри исходного файла начинается кусок
                         length: Long          // Сколько байт этого файла лежит в данной части
                       ) derives ReadWriter


case class PartInfo(
                     number: Int,
                     fileName: String,
                     mediaNumber: Int,     // Порядковый номер носителя
                     mediaMark: String,    // Имя
                     mediaPath: String,    // Папка носителя (путь)
                     size: Long,           // Размер части в байтах
                     checksum: String = "",
                     chunks: List[FileChunkRef] = List.empty,
                     writtenAndVerified: Boolean = false
                   ) derives ReadWriter

// медиа инфо
case class MediaDescriptor(
                            setName: String,
                            setId: String,
                            mediaNumber: Int,
                            totalMediaCount: Int,
                            partNumbers: List[Int],
                            completed: Boolean
                          ) derives ReadWriter

// манифест
case class ManifestData(
                         setId: String,
                         name: String,
                         createdAt: String,
                         mediaCount: Int,
                         sources: List[SourceItem],
                         parts: List[PartInfo],
                         mediaOrder: List[Drive],
                         checksumAlgorithm: String,
                         preserveEmptyDirectories: Boolean,
                         completed: Boolean
                       ) derives ReadWriter

case class TransferState(
                          manifest: ManifestData,
                          lastVerifiedPart: Int
                        ) derives ReadWriter


//функции
def validateSettings(settings: AppSettings): Boolean = {
  if(settings.minimumPartSize <= 0 || settings.minimumPartSize >= settings.defaultPartSize){
    println("Ошибка! Минимальный размер указан меньше нуля или он равен обычному размеру!")
    false
  } else if (settings.reservedSpacePerMedia < 0){
    println("Ошибка! Минимальный размер медиа указан меньше нуля!")
    false
  } else if (settings.workDirectory.isEmpty){
    println("Ошибка! Путь пустой!")
    false
  } else {
    println("Всё гуд!")
    true
  }
}

def loadOrCreateSettings(path: os.Path): AppSettings = {
  if (!os.exists(path)) {
    val defaultApp = AppSettings()
    os.write.over(path, write(defaultApp, indent = 2))
    defaultApp
  } else {
    try {
      val json = os.read(path)
      val loaded = read[AppSettings](json)
      if (validateSettings(loaded)){
        loaded
      } else {
        AppSettings()
      }
    } catch {
      case e: Exception =>
        println("Ошибка! Будет использован файл по умолчанию")
        AppSettings()
    }
  }
}


def loadOrCreateCatalogue(mediaRoot: os.Path): Catalogue = {
  val jsonPath = mediaRoot / "drives.json"
  val catalogue = if (!os.exists(jsonPath)) {
    val defaultCatalogue = Catalogue(drives)
    os.write.over(jsonPath, write(defaultCatalogue, indent = 2), createFolders = true)
    defaultCatalogue
  } else {
    try {
      val json = os.read(jsonPath)
      read[Catalogue](json)
    } catch {
      case e: Exception =>
        println("Ошибка чтения drives.json! Используется список носителей по умолчанию.")
        Catalogue(drives)
    }
  }
  catalogue.drives.foreach { drive =>
    val folderPath = mediaRoot / drive.path
    os.makeDir.all(folderPath)
  }
  catalogue
}