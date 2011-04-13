package example.simplex3d.tutorials

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Vectors extends Application {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-using-vectors/

  
  // Simple factories:
  val u = Vec3(1, 2, 3)
  val v = Vec3(2)
  val a = Vec2(1, 2)
  val b = Vec4(1)

  // Composition factories:
  Vec3(1, a)
  Vec4(2, a, 2)
  Vec4(u, 1)
  Vec4(a, a)

  // The above code is equivalent to:
  Vec3(1, a.x, a.y)
  Vec4(2, a.x, a.y, 2)
  Vec4(u.x, u.y, u.z, 1)
  Vec4(a.x, a.y, a.x, a.y)

  // Accessors:
  u == Vec3(u.r, u.g, u.b)
  u == Vec3(u.s, u.t, u.p)
  u == Vec3(u(0), u(1), u(2))

  // Vector operators:
  val add = u + v
  val sub = u - v
  val negation = -u
  val mult1 = u*2
  val mult2 = 2*u

  // Component vise mult and div:
  val componentMult = u*v
  val componentDiv = u/v

  // Compound assignment operators:
  u += v
  u -= v
  u *= 2
  u *= v
  u /= v

  // Functions:
  length(u)
  normalize(u)
  dot(u, v)
  cross(u, v)

  // Component-wise functions:
  abs(u)
  radians(u)
  sin(u)
  sqrt(u)
  min(u, v)
  clamp(u, 0, 1)


  // Swizzling read:
  val rearranged1 = u.xy
  val rearranged2 = u.yx
  val rearranged3 = u.xx
  val rearranged4 = b.wxz

  // Swizzling write:
  u.zx = a
  u.xy = u.zy
  b.xyw = u

  // Compound assignent with swizzling:
  u.zx += a
  u.xy *= u.zy
  b.xyw /= u

  // Constant (immutable) vectors:
  val c1 = ConstVec3(1, 2, 3)
  val c2 = ConstVec3(u)
  val c3: ConstVec3 = u


  // Integer vectors:
  val i2 = Vec2i(1, 2)
  val i3 = Vec3i(1)
  val i4 = Vec4i(2)

  // Promotion:
  val doubleRes1 = u + i3
  val doubleRes2 = normalize(i2)

  // Boolean vectors:
  val eq = equal(u, v)
  val lt = lessThan(u, v)
  val ge = greaterThanEqual(u, v)

  // Functions on boolean vectors:
  not(eq) == notEqual(u, v)
  val boolean1 = all(lt)
  val boolean2 = any(ge)

}
