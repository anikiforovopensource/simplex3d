package simplex3d.console.example.scala

object Version extends Application {

  println("Scala version: " + scala.util.Properties.versionString)

  try {
    println("Java version: " + System.getProperty("java.version"))
  }
  catch {
    case _ => println("Unable to read Java version from the sandbox mode.")
  }

}
