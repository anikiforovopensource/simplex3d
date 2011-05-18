package example.scala.regex


/**
 * @author Aleksey Nikiforov (lex)
 */
object Simple extends App {

  val string = "aaabbb"

  val usingJava = string.replace("a", "x")
  println(usingJava)

  val regex = "a".r
  val usingScala = regex.replaceAllIn(string, "x")
  println(usingScala)

}
