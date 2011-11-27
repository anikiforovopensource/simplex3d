package example.scala


/**
 * @author Aleksey Nikiforov (lex)
 */
object Version extends App {

  println("Scala version: " + scala.util.Properties.versionString)

  try {
    println("Java version: " + System.getProperty("java.version"))
  }
  catch {
    case _ => println("Unable to read Java version from the sandbox mode.")
  }

}
