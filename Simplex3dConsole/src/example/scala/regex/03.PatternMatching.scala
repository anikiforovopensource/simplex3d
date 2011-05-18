package example.scala.regex


/**
 * @author Aleksey Nikiforov (lex)
 */
object PatternMatching extends App {

  val string = "aaabbb"

  // Match or error.
  val ThreeA = """(aaa).*""".r
  val ThreeA(matched) = string
  println("|" + matched + "|")

  // Detect when there are no matches.
  string match {
    case ThreeA(matched) => println("|" + matched + "|")
    case _ => println("no matches")
  }

}
