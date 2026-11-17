@main
def main(): Unit = {
  val configPath = os.pwd / "src" / "main" / "scala" / "config.json"
  val mediaRoot  = os.pwd / "src" / "main" / "scala" / "virtual_media"

  val state = new AppState(configPath, mediaRoot)
  Repl.run(state)
}