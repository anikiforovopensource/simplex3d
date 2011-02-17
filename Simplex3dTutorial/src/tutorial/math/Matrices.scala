package tutorial.math

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Matrices {

  def main(args: Array[String]): Unit = {
    // Factories:
    val m2 = Mat2(1, 2, 3, 4)
    val m3 = Mat3(Vec3(1), Vec3(2), Vec3(3))
    val m4 = Mat4(2)

    // Converting matrices:
    var n2 = Mat2(m3)
    var n4 = Mat4(m2)

    // Constructors above is equivalent to:
    n2 = Mat2(
      m3.m00, m3.m10,
      m3.m01, m3.m11
    )
    
    n4 = Mat4(
      m2.m00, m2.m10, 0, 0,
      m2.m01, m2.m11, 0, 0,
      0,      0,      1, 0,
      0,      0,      0, 1
    )

    // Accessors:
    m2 == Mat2(m2.m00, m2.m10, m2.m01, m2.m11)
    m2 == Mat2(m2(0, 0), m2(1, 0), m2(0, 1), m2(1,1))
    m2 == Mat2(m2(0), m2(1))

    // Matrix operators:
    val add = m2 + n2
    val sub = m2 - n2
    val negation = -m2
    val mult1 = m2 * 5
    val mult2 = 5 * m2

    // Linear algebra multiplication:
    val matrix_x_matrix = m2*n2
    val matrix_x_vector = m2*Vec2(1, 2)

    // Compound assignment operators:
    m2 += n2
    m2 -= n2
    m2 *= 5
    m2 *= n2

    // Functions:
    matrixCompMult(m2, n2)
    transpose(m2)
    determinant(m2)
    inverse(m2)

    // Constant (immutable) matrices:
    val c2 = ConstMat2(1, 2, 3, 4)
    val c3 = ConstMat3(m3)
    val c4: ConstMat4 = m4
  }
}
