package example.scala.regex


/**
 * @author Aleksey Nikiforov (lex)
 */
object UsingMatch extends Application {

  import scala.util.matching.Regex.Match

  val string = "aabbcc"
  val regex = "[abc]".r

  val replaceAll = regex.replaceAllIn(string, _ match {
    case Match("a") => "x"
    case Match("b") => "y"
    case _ => "?"
  })
  println(replaceAll)

  val replaceSome = regex.replaceSomeIn(string, {
    case Match("a") => Some("x")
    case Match("b") => Some("y")
    case _ => None
  })
  println(replaceSome)
}
