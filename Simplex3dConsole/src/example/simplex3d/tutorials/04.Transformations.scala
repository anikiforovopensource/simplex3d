package example.simplex3d.tutorials

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Transformations extends Application {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-using-transformations/


  // Transformation classes.
  var t2d = Mat2x3 scale(2) rotate(radians(30)) translate(Vec2(1, 2))
  var t3d = Mat3x4 scale(2) rotateZ(radians(30)) translate(Vec3(1, 2, 0))

  // The above code is equivalent to.
  t2d = Mat2x3.Identity scale(2) rotate(radians(30)) translate(Vec2(1, 2))
  t3d = Mat3x4.Identity scale(2) rotateZ(radians(30)) translate(Vec3(1, 2, 0))

  // Example.
  val p = Vec3(1, 0, 0)
  val t1 = Mat3x4 rotateY(-Pi/2) scale(Vec3(1, 1, 2)) translate(Vec3(2, 0, 0))
  println("order1: " + t1.transformPoint(p))

  // Order-dependent.
  val t2 = Mat3x4 scale(Vec3(1, 1, 2)) translate(Vec3(2, 0, 0)) rotateY(-Pi/2)
  println("order2: " + t2.transformPoint(p))

  // Apply.
  val t3 = Mat3x4(1)
  t3.applyScale(Vec3(1, 1, 2))
  t3.applyTranslation(Vec3(2, 0, 0))
  t3.applyRotationY(-Pi/2)
  val p3 = t3.transformPoint(p)
  println("original: " + p + ", after transformation: " + p3)

  // Inverse.
  val invt3 = inverse(t3)
  println("transformed: " + p3 + ", after inverse: " + invt3.transformPoint(p3))

  // 2D Transformations.
  val scale2d = Mat2x3 scale(2) scale(Vec2(2))
  val rotation2d = Mat2x3 rotate(radians(10))
  val translation2d = Mat2x3 translate(Vec2(1))
  val concatenation2d = Mat2x3 concat(t2d)

  // 3D Transformations.
  val q = Quat4 rotateX(0.1) rotateZ(0.2)
  val scale3d = Mat3x4 scale(2) scale(Vec3(2))
  val rotation3d = Mat3x4 rotate(q) rotateX(Pi) rotateY(Pi/4) rotateZ(Pi/6)
  val translation3d = Mat3x4 translate(Vec3(1))
  val concatenation3d = Mat3x4 concat(t3d)

  // 2D apply.
  val a2 = Mat2x3(1)
  a2.applyScale(2)
  a2.applyScale(Vec2(2))
  a2.applyRotation(radians(10))
  a2.applyTranslation(Vec2(1))
  a2.applyTransform(t2d)

  // 3D apply.
  val a3 = Mat3x4(1)
  a3.applyScale(2)
  a3.applyScale(Vec3(2))
  a3.applyRotation(q)
  a3.applyRotationX(Pi)
  a3.applyRotationY(Pi/4)
  a3.applyRotationZ(Pi/6)
  a3.applyTranslation(Vec3(1))
  a3.applyTransform(t3d)

}
