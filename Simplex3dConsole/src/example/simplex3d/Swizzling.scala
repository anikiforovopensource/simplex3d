package example.simplex3d

import simplex3d.math.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Swizzling extends App {

  val u = Vec3(1, 2, 3)
  val v = Vec2(5, 6)
  u.xz += Vec2(2) + v.yx
  println(u)
  
}
