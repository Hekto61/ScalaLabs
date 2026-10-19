import java.security.MessageDigest
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

def logEvent(settings: AppSettings, message: String): Unit = {
  val logPath = os.pwd / settings.logFile
  val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
  val logLine = s"[$timestamp] $message\n"

  os.write.append(logPath, logLine, createFolders = true)
}

def calculateChecksum(bytes: Array[Byte], algorithm: String = "SHA-256"): String = {
  val digest = MessageDigest.getInstance(algorithm)
  val hashBytes = digest.digest(bytes)
  hashBytes.map("%02X".format(_)).mkString
}


def calculateFileChecksum(path: os.Path, algorithm: String = "SHA-256"): String = {
  val bytes = os.read.bytes(path)
  calculateChecksum(bytes, algorithm)
}

def getUsedSpace(mediaRoot: os.Path, drive: Drive): Long = {
  val folderPath = mediaRoot / drive.path
  if (!os.exists(folderPath)) {
    0L
  } else {
     os.walk(folderPath)
     .filter(os.isFile)
     .map(os.size)
     .sum
    os.walk(folderPath).filter(os.isFile).map(os.size).sum
  }
}

def getPhysicalFreeSpace(mediaRoot: os.Path, drive: Drive): Long = {
  val used = getUsedSpace(mediaRoot, drive)
  Math.max(0L, drive.volume - used)
}

def getAllowedFreeSpace(mediaRoot: os.Path, drive: Drive, settings: AppSettings): Long = {
  val physicalFree = getPhysicalFreeSpace(mediaRoot, drive)
  val withReserve = Math.max(0L, physicalFree - settings.reservedSpacePerMedia)
  if (drive.maxAllowedOverride > 0) Math.min(withReserve, drive.maxAllowedOverride)
  else withReserve
}

def formatSize(bytes: Long): String = {
  if (bytes >= 1024L * 1024 * 1024) f"${bytes.toDouble / (1024 * 1024 * 1024)}%.2f ГБ"
  else if (bytes >= 1024L * 1024) f"${bytes.toDouble / (1024 * 1024)}%.2f МБ"
  else if (bytes >= 1024L) f"${bytes.toDouble / 1024}%.2f КБ"
  else s"$bytes Б"
}

def formatPartName(template: String, setName: String, number: Int): String = {
  val numStr = f"$number%03d"
  template.replace("{setName}", setName).replace("{number}", numStr)
}