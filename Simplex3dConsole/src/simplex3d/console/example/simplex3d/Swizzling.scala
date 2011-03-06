package simplex3d.console.example.simplex3d

import simplex3d.math.double._

object Swizzling extends Application {

  val u = Vec3(1, 2, 3)
  val v = Vec2(5, 6)
  u.xz += Vec2(2) + v.yx
  println(u)
  
}
