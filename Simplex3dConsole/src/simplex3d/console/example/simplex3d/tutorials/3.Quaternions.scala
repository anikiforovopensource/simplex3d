package simplex3d.console.example.simplex3d.tutorials

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Quaternions extends Application {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-using-quaternions/

  
  // The identity quaternion:
  val identity = Quat4(1, 0, 0, 0)
  val p = Quat4(identity)
  val q: Quat4 = Quat4.Identity

  // Accessors:
  q == Quat4(q.a, q.b, q.c, q.d)

  // Converting vectors:
  val u = Vec4(1, 2, 3, 4)
  Quat4(u) == Quat4(u.w, u.x, u.y, u.z)

  // Operators:
  val add = q + p
  val sub = q - p
  val negation = -q
  val mult1 = q*2
  val mult2 = 2*q

  // Rotating a vector
  val pos = Vec3(1, 2, 3)
  q.rotateVector(pos)

  // Quaternion rotation:
  val pq = q*p
  pq == p.rotate(q)

  // Axis-wise rotation.
  val r = q rotateY(radians(10)) rotateX(0.3) rotateZ(Pi/2)

  // Constructing rotations
  val r1 = Quat4.Identity rotateX(radians(45)) rotateY(radians(30))
  val r2 = Quat4 rotateX(radians(45)) rotateY(radians(30))
  r1 == r2

  // Functions:
  norm(q)
  normalize(q)
  conjugate(q)
  inverse(q)

  // Compound assignment operators:
  q += p
  q -= p
  q *= 2
  q *= p

  // Constant (immutable) vectors:
  val c1 = ConstQuat4(1, 2, 3, 4)
  val c2 = ConstQuat4(q)
  val c3: ConstQuat4 = q

}
