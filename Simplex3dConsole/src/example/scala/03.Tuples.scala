package example.scala


/**
 * @author Aleksey Nikiforov (lex)
 */
object Tuples extends Application {

  // Return multiple values using tuples.
  def multipleReturn() :(String, Int) = {
    ("SomeValue", 1)
  }

  // Extract the results with ease using pattern matching.
  val (name, x) = multipleReturn()

  // The type information is preserved!
  println(name.toLowerCase + " " + (x + 1))

  // Extracting components of a tuple.
  val t = ("a", "b", "c")
  println(t._3 + t._2 + t._1)

}
