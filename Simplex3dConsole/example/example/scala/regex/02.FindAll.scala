package example.scala.regex


/**
 * @author Aleksey Nikiforov (lex)
 */
object FindAll extends App {

  val string = "aaabbb"

  val regex = "a".r
  for (matched <- regex.findAllIn(string)) {
    println(matched)
  }
}
