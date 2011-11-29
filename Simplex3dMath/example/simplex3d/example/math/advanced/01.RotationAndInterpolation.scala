package simplex3d.example.math.advanced

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object RotationAndInterpolation extends App {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-rotation-and-interpolation/


  // Quaternion rotation.
  val q = Quat4 rotateX(radians(10)) rotateZ(Pi/2)
  val p = Vec3(1, 0, 0)
  println("Quaternion rotation: " + q.rotateVector(p))

  // Matrix rotation.
  val t = Mat3x4 rotateX(radians(10)) rotateZ(Pi/2)
  println("Using transformations: " + t.transformVector(p))
  val m = Mat3(t)
  println("Using rotation matrix: " + m*p)

  // Angle-axis rotation.
  val axis = Vec3(0)
  var angle = angleAxis(q, axis)
  println("Angle-axis from quaternion: " + angle + " radians, " + axis)
  angle = angleAxis(m, axis)
  println("Angle-axis from matrix: " + angle + " radians, " + axis)

  // Converting rotations.
  val qm = quaternion(m)
  val qaa = quaternion(angle, axis)
  Quat4.rotateY(Pi/4) == quaternion(Pi/4, Vec3.UnitY)

  val mq = rotationMat(q)
  val maa = rotationMat(angle, axis)
  Mat3(Mat3x4 rotateY(Pi/4)) == rotationMat(Pi/4, Vec3.UnitY)

  // Skipping quaternion normalization.
  val msafe = t rotate(q) // will perform quaternion normalization
  val munsafe = t concat(rotationMat(q))
  val rsafe = q.rotateVector(p) // will perform quaternion normalization
  val runsafe = rotateVector(p, q)

  // LookAt
  val direction = Vec3(1)
  val up = Vec3.UnitY
  var objectRotation = lookAt(direction, up)
  var cameraRotation = lookAt(-direction, up)

  val pos = Vec3(3, 5, 6)
  val point = Vec3(-2, 1, 4)
  objectRotation = lookAt(point - pos, up)
  cameraRotation = lookAt(pos - point, up)

  val lookAtQuat = quaternion(lookAt(point - pos, up))

  // Linear interpolation.
  val iv = mix(pos, point, 0.3)
  val im = lerp(objectRotation, m, 0.9)
  println("Rotation mat interpolation: " + im*p)

  // Spherical interpolation.
  val sq = slerp(lookAtQuat, q, 0.9)
  println("Spherical linear interpolation: " + q.rotateVector(p))

}
