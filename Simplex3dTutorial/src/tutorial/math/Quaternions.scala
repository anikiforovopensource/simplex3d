package tutorial.math

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Quaternions {

  def main(args: Array[String]): Unit = {
    // The identity quaternion:
    val p = Quat4(1, 0, 0, 0)
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

    // Compound assignment operators:
    q += p
    q -= p
    q *= 2
    q *= p

    // Functions:
    norm(q)
    normalize(q)
    conjugate(q)
    inverse(q)

    // Constructing rotations
    val r1 = q rotate(p) rotateX(radians(10))
    val r2 = Quat4 rotateZ(Pi/2) rotateY(0.3)

    // Rotating a vector
    q.rotateVector(Vec3(1, 2, 3))

    // Constant (immutable) vectors:
    val c1 = ConstQuat4(1, 2, 3, 4)
    val c2 = ConstQuat4(q)
    val c3: ConstQuat4 = q
  }
}
