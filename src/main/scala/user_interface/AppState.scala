class AppState(val configPath: os.Path, val mediaRoot: os.Path) {
  var settings: AppSettings = loadOrCreateSettings(configPath).getOrElse(AppSettings())
  var catalogue: Catalogue = loadOrCreateCatalogue(mediaRoot).getOrElse(Catalogue(drives))
  var currentSetName: String = "archive"
  var sources: List[SourceItem] = List.empty
  var selectedDrives: List[Drive] = catalogue.drives
  var currentPlan: Option[ManifestData] = None
  var running: Boolean = true

  def reload(): Either[AppError, Unit] = {
    for {
      s <- loadOrCreateSettings(configPath)
      c <- loadOrCreateCatalogue(mediaRoot)
    } yield {
      settings = s
      catalogue = c
      selectedDrives = c.drives
    }
  }

  def manifestPath: os.Path = mediaRoot / selectedDrives.head.path / settings.manifestFile
}