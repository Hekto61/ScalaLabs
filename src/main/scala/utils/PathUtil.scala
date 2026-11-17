object PathUtil {
  def resolve(arg: String): os.Path =
    if (java.nio.file.Paths.get(arg).isAbsolute) os.Path(arg)
    else os.pwd / os.RelPath(arg)
}