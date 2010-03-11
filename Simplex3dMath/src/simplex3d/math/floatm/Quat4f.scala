/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyQuat4f extends ReadQ[Float] {

  private[math] def fa: Float = a
  private[math] def fb: Float = b
  private[math] def fc: Float = c
  private[math] def fd: Float = d

  private[math] def da: Double = a
  private[math] def db: Double = b
  private[math] def dc: Double = c
  private[math] def dd: Double = d

  def a: Float
  def b: Float
  def c: Float
  def d: Float

  def apply(i: Int) :Float = {
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
  def unary_-() = new Quat4f(-a, -b, -c, -d)
  def *(s: Float) = new Quat4f(a * s, b * s, c * s, d * s)
  def /(s: Float) = { val inv = 1/s;
    new Quat4f(a * inv, b * inv, c * inv, d * inv)
  }

  def +(s: Float) = new Quat4f(a + s, b + s, c + s, d + s)
  def -(s: Float) = new Quat4f(a - s, b - s, c - s, d - s)

  private[math] def divideByComponent(s: Float) = {
    new Quat4f(s / a, s / b, s / c, s / d)
  }

  def +(q: AnyQuat4f) = new Quat4f(a + q.a, b + q.b, c + q.c, d + q.d)
  def -(q: AnyQuat4f) = new Quat4f(a - q.a, b - q.b, c - q.c, d - q.d)
  def *(q: AnyQuat4f) = new Quat4f(
    a*q.a - b*q.b - c*q.c - d*q.d,
    a*q.b + b*q.a + c*q.d - d*q.c,
    a*q.c - b*q.d + c*q.a + d*q.b,
    a*q.d + b*q.c - c*q.b + d*q.a
  )

  def rotate(q: AnyQuat4f) :Quat4f = q*this
  def rotate(angle: Float, axis: AnyVec3f) :Quat4f = {
    quaternion(angle, axis)*this
  }
  def rotateX(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitX)*this
  }
  def rotateY(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitY)*this
  }
  def rotateZ(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitZ)*this
  }
  def invert() :Quat4f = {
    inverse(this)
  }

  def ==(q: AnyQuat4f) :Boolean = {
    if (q eq null) false
    else a == q.a && b == q.b && c == q.c && d == q.d
  }

  def !=(q: AnyQuat4f) :Boolean = !(this == q)

  private[math] def hasErrors: Boolean = {
    import java.lang.Float._
    (
      isNaN(a) || isInfinite(a) ||
      isNaN(b) || isInfinite(b) ||
      isNaN(c) || isInfinite(c) ||
      isNaN(d) || isInfinite(d)
    )
  }

  override def equals(other: Any) :Boolean = {
    other match {
      case q: AnyQuat4f => this == q
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


final class ConstQuat4f private[math] (
  val a: Float, val b: Float, val c: Float, val d: Float
) extends AnyQuat4f with ConstQuat[Float]

object ConstQuat4f {
  def apply(a: Float, b: Float, c: Float, d: Float) = {
    new ConstQuat4f(a, b, c, d)
  }
  def apply(u: ReadQ[_]) = new ConstQuat4f(u.fa, u.fb, u.fc, u.fd)

  implicit def toConst(u: AnyQuat4f) = new ConstQuat4f(u.a, u.b, u.c, u.d)
}


final class Quat4f private[math] (
  var a: Float, var b: Float, var c: Float, var d: Float
) extends AnyQuat4f with Quat[Float]
{
  def *=(s: Float) { a *= s; b *= s; c *= s; d *= s }
  def /=(s: Float) { val inv = 1/s; a *= inv; b *= inv; c *= inv; d *= inv }

  def +=(s: Float) { a += s; b += s; c += s; d += s }
  def -=(s: Float) { a -= s; b -= s; c -= s; d -= s }

  def +=(q: Quat4f) { a += q.a; b += q.b; c += q.c; d += q.d }
  def -=(q: Quat4f) { a -= q.a; b -= q.b; c -= q.c; d -= q.d }
  def *=(q: Quat4f) {
    val na = a*q.a - b*q.b - c*q.c - d*q.d
    val nb = a*q.b + b*q.a + c*q.d - d*q.c
    val nc = a*q.c - b*q.d + c*q.a + d*q.b
    d = a*q.d + b*q.c - c*q.b + d*q.a

    a = na; b = nb; c = nc
  }
  
  def :=(q: AnyQuat4f) { a = q.a; b = q.b; c = q.c; d = q.d }
  def set(a: Float, b: Float, c: Float, d: Float) {
    this.a = a; this.b = b; this.c = c; this.d = d
  }

  def update(i: Int, s: Float) {
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

object Quat4f {
  val Identity = new ConstQuat4f(1, 0, 0, 0)

  def apply(a: Float, b: Float, c: Float, d: Float) = new Quat4f(a, b, c, d)
  def apply(q: ReadQ[_]) = new Quat4f(q.fa, q.fb, q.fc, q.fd)
  def apply(u: Read4[_]) = new Quat4f(u.fw, u.fx, u.fy, u.fz)

  def unapply(q: AnyQuat4f) = Some((q.a, q.b, q.c, q.d))

  def rotate(angle: Float, axis: AnyVec3f) :Quat4f = {
    quaternion(angle, axis)
  }
  def rotateX(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitX)
  }
  def rotateY(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitY)
  }
  def rotateZ(angle: Float) :Quat4f = {
    quaternion(angle, Vec3f.UnitZ)
  }

  implicit def toMutable(u: AnyQuat4f) = new Quat4f(u.a, u.b, u.c, u.d)
}
