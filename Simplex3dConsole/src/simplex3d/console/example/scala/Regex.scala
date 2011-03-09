package simplex3d.console.example.scala

object Regex extends Application {

  val line = "  abc  "
  val Trim = """\s*([^\s]*)\s*""".r
  val Trim(text) = line
  println("|" + text + "|")

}
