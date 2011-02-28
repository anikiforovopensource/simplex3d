package tutorial.math

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Transformations {

  def main(args: Array[String]): Unit = {
    // Transformation classes.
    var t2d = Mat2x3 scale(2) rotate(radians(30)) translate(Vec2(1, 2))
    var t3d = Mat3x4 scale(2) rotateZ(radians(30)) translate(Vec3(1, 2, 0))

    // The above code is equivalent to.
    t2d = Mat2x3.Identity scale(2) rotate(radians(30)) translate(Vec2(1, 2))
    t3d = Mat3x4.Identity scale(2) rotateZ(radians(30)) translate(Vec3(1, 2, 0))

    // Example.
    val p = Vec3(1, 0, 0)
    val t1 = Mat3x4 rotateY(radians(-90)) scale(Vec3(1, 1, 2)) translate(Vec3(2, 0, 0))
    println(t1.transformPoint(p))

    // Order-dependent.
    val t2 = Mat3x4 scale(Vec3(1, 1, 2)) translate(Vec3(2, 0, 0)) rotateY(radians(-90))
    println(t2.transformPoint(p))

    // Apply.
    val t3 = Mat3x4(1)
    t3.applyScale(Vec3(1, 1, 2))
    t3.applyTranslation(Vec3(2, 0, 0))
    t3.applyRotationY(radians(-90))
    val p3 = t3.transformPoint(p)
    println(p3)

    // Inverse
    val invt3 = inverse(t3)
    println(invt3.transformPoint(p3))

    // 2d Transformations
    val scale2d = Mat2x3 scale(2) scale(Vec2(2))
    val rotation2d = Mat2x3 rotate(radians(10))
    val translation2d = Mat2x3 translate(Vec2(1))
    val concatenation2d = Mat2x3 concat(t2d)

    // 3d Transformations
    val q = Quat4 rotateX(0.1) rotateZ(0.2)
    val scale3d = Mat3x4 scale(2) scale(Vec3(2))
    val rotation3d = Mat3x4 rotate(q) rotateX(radians(10)) rotateY(radians(15)) rotateZ(radians(20))
    val translation3d = Mat3x4 translate(Vec3(1))
    val concatenation3d = Mat3x4 concat(t3d)
  }
}
