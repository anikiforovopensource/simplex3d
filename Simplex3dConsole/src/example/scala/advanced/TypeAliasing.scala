package example.scala.advanced


/**
 * @author Aleksey Nikiforov (lex)
 */
object TypeAliasing extends Application {

  // A trait/class with an abstract type.
  trait Trait { type AbstractType }

  // A cluttered declaration.
  def fooClutter[R](arg: Trait { type AbstractType = R }) {}

  // Type alias: moving abstract types into generics.
  type T[R] = Trait { type AbstractType = R }

  // Short declaration with no clutter, equivalent to fooClutter.
  def foo[R](arg: T[R]) {}

}
