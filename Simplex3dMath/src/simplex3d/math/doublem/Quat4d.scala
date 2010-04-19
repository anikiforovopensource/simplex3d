/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyQuat4d extends ReadQ[Double] {

  private[math] def fa: Float = float(a)
  private[math] def fb: Float = float(b)
  private[math] def fc: Float = float(c)
  private[math] def fd: Float = float(d)

  private[math] def da: Double = a
  private[math] def db: Double = b
  private[math] def dc: Double = c
  private[math] def dd: Double = d

  def a: Double
  def b: Double
  def c: Double
  def d: Double

  def apply(i: Int) :Double = {
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

  def unary_+() :this.type = this
  /**
   * This methods negates every term of this quaternion.
   * Negating the quaternion produces another quaternion which represent
   * the same rotation. That is both q and -q represent exactly the
   * same rotation.
   */
  def unary_-() = new Quat4d(-a, -b, -c, -d)
  def *(s: Double) = new Quat4d(a * s, b * s, c * s, d * s)
  def /(s: Double) = { val inv = 1/s;
    new Quat4d(a * inv, b * inv, c * inv, d * inv)
  }
  def +(s: Double) = new Quat4d(a + s, b + s, c + s, d + s)
  def -(s: Double) = new Quat4d(a - s, b - s, c - s, d - s)
  
  private[math] def divideByComponent(s: Double) = {
    new Quat4d(s / a, s / b, s / c, s / d)
  }

  def +(q: inQuat4d) = new Quat4d(a + q.a, b + q.b, c + q.c, d + q.d)
  def -(q: inQuat4d) = new Quat4d(a - q.a, b - q.b, c - q.c, d - q.d)
  def *(q: inQuat4d) = new Quat4d(
    a*q.a - b*q.b - c*q.c - d*q.d,
    a*q.b + b*q.a + c*q.d - d*q.c,
    a*q.c - b*q.d + c*q.a + d*q.b,
    a*q.d + b*q.c - c*q.b + d*q.a
  )

  def rotate(q: inQuat4d) :Quat4d = q*this
  def rotate(angle: Double, axis: inVec3d) :Quat4d = {
    quaternion(angle, normalize(axis))*this
  }
  def rotateX(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitX)*this
  }
  def rotateY(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitY)*this
  }
  def rotateZ(angle: Double) :Quat4d = {
    quaternion(angle, Vec3d.UnitZ)*this
  }
  def invert() :Quat4d = inverse(this)

  def rotateVector(u: inVec3d) :Vec3d = DoubleMath.rotate(u, normalize(this))


  def ==(q: inQuat4d) :Boolean = {
    if (q eq null) false
    else a == q.a && b == q.b && c == q.c && d == q.d
  }

  def !=(q: inQuat4d) :Boolean = !(this == q)

  private[math] def hasErrors: Boolean = {
    import java.lang.Double._
    (
      isNaN(a) || isInfinite(a) ||
      isNaN(b) || isInfinite(b) ||
      isNaN(c) || isInfinite(c) ||
      isNaN(d) || isInfinite(d)
    )
  }

  override def equals(other: Any) :Boolean = {
    other match {
      case q: inQuat4d => this == q
      case _ => false
    }
  }

  override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + a.hashCode
        ) + b.hashCode
      ) + c.hashCode
    ) + d.hashCode
  }

  override def toString() :String = {
    this.getClass.getSimpleName + "(" + a + ", " + b + ", " + c + ", " + d + ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstQuat4d private[math] (
  val a: Double, val b: Double, val c: Double, val d: Double
) extends AnyQuat4d with Immutable

object ConstQuat4d {
  /* @inline */ def apply(a: Double, b: Double, c: Double, d: Double) =
    new ConstQuat4d(a, b, c, d)

  def apply(u: ReadQ[_]) = new ConstQuat4d(u.da, u.db, u.dc, u.dd)

  implicit def toConst(u: AnyQuat4d) = new ConstQuat4d(u.a, u.b, u.c, u.d)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Quat4d private[math] (
  var a: Double, var b: Double, var c: Double, var d: Double
) extends AnyQuat4d with Mutable with Implicits[On]
{
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
  
  def :=(q: inQuat4d) { a = q.a; b = q.b; c = q.c; d = q.d }
  def set(a: Double, b: Double, c: Double, d: Double) {
    this.a = a; this.b = b; this.c = c; this.d = d
  }

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
  val Identity = new ConstQuat4d(1, 0, 0, 0)
  
  /* @inline */ def apply(a: Double, b: Double, c: Double, d: Double) =
    new Quat4d(a, b, c, d)

  def apply(q: ReadQ[_]) = new Quat4d(q.da, q.db, q.dc, q.dd)
  def apply(u: Read4[_]) = new Quat4d(u.dw, u.dx, u.dy, u.dz)

  def unapply(q: AnyQuat4d) = Some((q.a, q.b, q.c, q.d))

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

  implicit def toMutable(u: AnyQuat4d) = new Quat4d(u.a, u.b, u.c, u.d)
}
