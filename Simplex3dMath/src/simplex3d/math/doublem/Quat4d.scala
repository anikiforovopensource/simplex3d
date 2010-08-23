/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.doublem

import scala.reflect.Manifest._
import simplex3d.math.integration.buffer._
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadQuat4d extends ProtectedQuat4d[Double]
{
  private[math] final def fa: Float = float(a)
  private[math] final def fb: Float = float(b)
  private[math] final def fc: Float = float(c)
  private[math] final def fd: Float = float(d)

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
  final def rotate(angle: Double, axis: inVec3d) :Quat4d = {
    quaternion(angle, normalize(axis))*this
  }
  final def rotateX(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitX)*this
  }
  final def rotateY(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitY)*this
  }
  final def rotateZ(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitZ)*this
  }

  final def rotateVector(u: inVec3d) :Vec3d =
    DoubleMath.rotateVector(u, normalize(this))


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
    this.getClass.getSimpleName + "(" + a + ", " + b + ", " + c + ", " + d + ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
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


@serializable @SerialVersionUID(5359695191257934190L)
final class Quat4d private[math] (
  ca: Double, cb: Double, cc: Double, cd: Double
) extends ReadQuat4d with Implicits[On] with Composite
{
  type Element = ReadQuat4d
  type Component = Double1

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
  final val Manifest = classType[ReadQuat4d](classOf[ReadQuat4d])
  
  /*main factory*/ def apply(a: Double, b: Double, c: Double, d: Double) =
    new Quat4d(a, b, c, d)

  def apply(q: AnyQuat4[_]) = new Quat4d(q.da, q.db, q.dc, q.dd)
  def apply(u: AnyVec4[_]) = new Quat4d(u.dw, u.dx, u.dy, u.dz)

  def unapply(q: ReadQuat4d) = Some((q.a, q.b, q.c, q.d))

  def rotate(q: inQuat4d) :Quat4d = Quat4d(q)
  def rotate(angle: Double, axis: inVec3d) :Quat4d = {
    quaternion(angle, normalize(axis))
  }
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
