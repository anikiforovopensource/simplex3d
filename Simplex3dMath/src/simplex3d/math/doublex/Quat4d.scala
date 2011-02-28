/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math
package doublex

import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadQuat4d extends ProtectedQuat4d[Double]
{
  private[math] final def fa: Float = a.toFloat
  private[math] final def fb: Float = b.toFloat
  private[math] final def fc: Float = c.toFloat
  private[math] final def fd: Float = d.toFloat

  private[math] final def da: Double = a
  private[math] final def db: Double = b
  private[math] final def dc: Double = c
  private[math] final def dd: Double = d


  final def a = pa
  final def b = pb
  final def c = pc
  final def d = pd


  protected def a_=(s: Double) { throw new UnsupportedOperationException }
  protected def b_=(s: Double) { throw new UnsupportedOperationException }
  protected def c_=(s: Double) { throw new UnsupportedOperationException }
  protected def d_=(s: Double) { throw new UnsupportedOperationException }


  final def apply(i: Int) :Double = {
    i match {
      case 0 => a
      case 1 => b
      case 2 => c
      case 3 => d
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  final def unary_+() :ReadQuat4d = this
  /**
   * This methods negates every term of this rotationQuat.
   * Negating the rotationQuat produces another rotationQuat which represent
   * the same rotation. That is both q and -q represent exactly the
   * same rotation.
   */
  final def unary_-() = new Quat4d(-a, -b, -c, -d)
  final def *(s: Double) = new Quat4d(a * s, b * s, c * s, d * s)
  final def /(s: Double) = { val inv = 1/s;
    new Quat4d(a * inv, b * inv, c * inv, d * inv)
  }
  final def +(s: Double) = new Quat4d(a + s, b + s, c + s, d + s)
  final def -(s: Double) = new Quat4d(a - s, b - s, c - s, d - s)
  
  private[math] final def divideByComponent(s: Double) = {
    new Quat4d(s / a, s / b, s / c, s / d)
  }

  final def +(q: inQuat4d) = new Quat4d(a + q.a, b + q.b, c + q.c, d + q.d)
  final def -(q: inQuat4d) = new Quat4d(a - q.a, b - q.b, c - q.c, d - q.d)
  final def *(q: inQuat4d) = new Quat4d(
    a*q.a - b*q.b - c*q.c - d*q.d,
    a*q.b + b*q.a + c*q.d - d*q.c,
    a*q.c - b*q.d + c*q.a + d*q.b,
    a*q.d + b*q.c - c*q.b + d*q.a
  )

  final def rotate(q: inQuat4d) :Quat4d = q*this
  final def rotateX(angle: Double) :Quat4d = {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qb = sin(halfAngle)

    new Quat4d(qa*a - qb*b, qa*b + qb*a, qa*c - qb*d, qa*d + qb*c)
  }
  final def rotateY(angle: Double) :Quat4d = {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qc = sin(halfAngle)

    new Quat4d(qa*a - qc*c, qa*b + qc*d, qa*c + qc*a, qa*d - qc*b)
  }
  final def rotateZ(angle: Double) :Quat4d = {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qd = sin(halfAngle)

    new Quat4d(qa*a - qd*d, qa*b - qd*c, qa*c + qd*b, qa*d + qd*a)
  }

  final def rotateVector(u: inVec3d) :Vec3d =
    functions.rotateVector(u, normalize(this))


  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case q: AnyQuat4[_] => da == q.da && db == q.db && dc == q.dc && dd == q.dd
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    ) + d.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Quat4" + "(" + a + ", " + b + ", " + c + ", " + d + ")"
  }
}


@serializable @SerialVersionUID(8104346712419693669L)
final class ConstQuat4d private[math] (
  ca: Double, cb: Double, cc: Double, cd: Double
) extends ReadQuat4d with Immutable {
  pa = ca; pb = cb; pc = cc; pd = cd

  override def clone() = this
}

object ConstQuat4d {
  /*main factory*/ def apply(a: Double, b: Double, c: Double, d: Double) =
    new ConstQuat4d(a, b, c, d)

  def apply(u: AnyQuat4[_]) = new ConstQuat4d(u.da, u.db, u.dc, u.dd)
  def apply(u: AnyVec4[_]) = new ConstQuat4d(u.dw, u.dx, u.dy, u.dz)

  implicit def toConst(u: ReadQuat4d) = new ConstQuat4d(u.a, u.b, u.c, u.d)
}


@serializable @SerialVersionUID(8104346712419693669L)
final class Quat4d private[math] (
  ca: Double, cb: Double, cc: Double, cd: Double
) extends ReadQuat4d with Implicits[On] with Composite
{
  type Read = ReadQuat4d
  type Const = ConstQuat4d
  type Component = RDouble

  pa = ca; pb = cb; pc = cc; pd = cd

  override def a_=(s: Double) { pa = s }
  override def b_=(s: Double) { pb = s }
  override def c_=(s: Double) { pc = s }
  override def d_=(s: Double) { pd = s }

  def *=(s: Double) { a *= s; b *= s; c *= s; d *= s }
  def /=(s: Double) { val inv = 1/s; a *= inv; b *= inv; c *= inv; d *= inv }

  def +=(s: Double) { a += s; b += s; c += s; d += s }
  def -=(s: Double) { a -= s; b -= s; c -= s; d -= s }

  def +=(q: inQuat4d) { a += q.a; b += q.b; c += q.c; d += q.d }
  def -=(q: inQuat4d) { a -= q.a; b -= q.b; c -= q.c; d -= q.d }
  def *=(q: inQuat4d) {
    val na = a*q.a - b*q.b - c*q.c - d*q.d
    val nb = a*q.b + b*q.a + c*q.d - d*q.c
    val nc = a*q.c - b*q.d + c*q.a + d*q.b
         d = a*q.d + b*q.c - c*q.b + d*q.a

    a = na; b = nb; c = nc
  }

  final def applyRotation(q: inQuat4d) {
    val na = q.a*a - q.b*b - q.c*c - q.d*d
    val nb = q.a*b + q.b*a + q.c*d - q.d*c
    val nc = q.a*c - q.b*d + q.c*a + q.d*b
         d = q.a*d + q.b*c - q.c*b + q.d*a

    a = na; b = nb; c = nc
  }
  final def applyRotationX(angle: Double) {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qb = sin(halfAngle)

    val na = qa*a - qb*b
         b = qa*b + qb*a
    val nc = qa*c - qb*d
         d = qa*d + qb*c

    a = na; c = nc
  }
  final def applyRotationY(angle: Double) {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qc = sin(halfAngle)

    val na = qa*a - qc*c
    val nb = qa*b + qc*d
         c = qa*c + qc*a
         d = qa*d - qc*b

    a = na; b = nb
  }
  final def applyRotationZ(angle: Double) {
    val halfAngle = angle*0.5
    val qa = cos(halfAngle)
    val qd = sin(halfAngle)

    val na = qa*a - qd*d
    val nb = qa*b - qd*c
         c = qa*c + qd*b
         d = qa*d + qd*a

    a = na; b = nb
  }


  override def clone() = Quat4d(this)
  def :=(q: inQuat4d) { a = q.a; b = q.b; c = q.c; d = q.d }

  def update(i: Int, s: Double) {
    i match {
      case 0 => a = s
      case 1 => b = s
      case 2 => c = s
      case 3 => d = s
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }
}

object Quat4d {
  final val Identity = new ConstQuat4d(1, 0, 0, 0)

  final val Manifest = classType[Quat4d](classOf[Quat4d])
  final val ConstManifest = classType[ConstQuat4d](classOf[ConstQuat4d])
  final val ReadManifest = classType[ReadQuat4d](classOf[ReadQuat4d])

  /*main factory*/ def apply(a: Double, b: Double, c: Double, d: Double) =
    new Quat4d(a, b, c, d)

  def apply(q: AnyQuat4[_]) = new Quat4d(q.da, q.db, q.dc, q.dd)
  def apply(u: AnyVec4[_]) = new Quat4d(u.dw, u.dx, u.dy, u.dz)

  def unapply(q: ReadQuat4d) = Some((q.a, q.b, q.c, q.d))

  def rotate(q: inQuat4d) :Quat4d = Quat4d(q)
  def rotateX(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitX)
  }
  def rotateY(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitY)
  }
  def rotateZ(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitZ)
  }

  implicit def toMutable(u: ReadQuat4d) = new Quat4d(u.a, u.b, u.c, u.d)
  implicit def castFloat(q: AnyQuat4[Float]) = new Quat4d(q.da, q.db, q.dc, q.dd)
}
