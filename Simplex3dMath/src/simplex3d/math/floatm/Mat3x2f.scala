/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009 Simplex3d Team
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
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat3x2f extends Read3x2[ConstVec3f]
{
  // Column major order.
  def m00: Float; def m10: Float; def m20: Float // column
  def m01: Float; def m11: Float; def m21: Float // column

  private[math] override def f00 = m00
  private[math] override def f10 = m10
  private[math] override def f20 = m20

  private[math] override def f01 = m01
  private[math] override def f11 = m11
  private[math] override def f21 = m21


  private[math] override def d00 = m00
  private[math] override def d10 = m10
  private[math] override def d20 = m20

  private[math] override def d01 = m01
  private[math] override def d11 = m11
  private[math] override def d21 = m21


  def apply(c: Int) :ConstVec3f = {
    c match {
      case 0 => new ConstVec3f(m00, m10, m20)
      case 1 => new ConstVec3f(m01, m11, m21)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  def apply(c: Int, r: Int) :Float = {
    def error() :Float = throw new IndexOutOfBoundsException(
      "Trying to read index (" + c + ", " + r + ") in " +
      this.getClass.getSimpleName
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00
          case 1 => m10
          case 2 => m20
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case 2 => m21
          case _ => error
        }
      case _ => error
    }
  }

  def unary_+() :this.type = this
  def unary_-() = new Mat3x2f(
    -m00, -m10, -m20,
    -m01, -m11, -m21
  )
  def *(s: Float) = new Mat3x2f(
    s*m00, s*m10, s*m20,
    s*m01, s*m11, s*m21
  )
  def /(s: Float) = { val inv = 1/s; new Mat3x2f(
    inv*m00, inv*m10, inv*m20,
    inv*m01, inv*m11, inv*m21
  )}

  def +(s: Float) = new Mat3x2f(
    m00 + s, m10 + s, m20 + s,
    m01 + s, m11 + s, m21 + s
  )
  def -(s: Float) = new Mat3x2f(
    m00 - s, m10 - s, m20 - s,
    m01 - s, m11 - s, m21 - s
  )

  def +(m: inMat3x2f) = new Mat3x2f(
    m00 + m.m00, m10 + m.m10, m20 + m.m20,
    m01 + m.m01, m11 + m.m11, m21 + m.m21
  )
  def -(m: inMat3x2f) = new Mat3x2f(
    m00 - m.m00, m10 - m.m10, m20 - m.m20,
    m01 - m.m01, m11 - m.m11, m21 - m.m21
  )

  /**
   * Component-wise devision.
   */
  def /(m: inMat3x2f) = new Mat3x2f(
    m00/m.m00, m10/m.m10, m20/m.m20,
    m01/m.m01, m11/m.m11, m21/m.m21
  )
  private[math] def divideByComponent(s: Float) = new Mat3x2f(
    s/m00, s/m10, s/m20,
    s/m01, s/m11, s/m21
  )

  def *(m: inMat2f) = new Mat3x2f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11
  )
  def *(m: inMat2x3f) = new Mat3f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12
  )
  def *(m: inMat2x4f) = new Mat3x4f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12,

    m00*m.m03 + m01*m.m13,
    m10*m.m03 + m11*m.m13,
    m20*m.m03 + m21*m.m13
  )

  def *(u: inVec2f) = new Vec3f(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y
  )
  private[math] def transposeMul(u: inVec3f) = new Vec2f(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z
  )

  def ==(m: inMat3x2f) :Boolean = {
    if (m eq null) false
    else
      m00 == m.m00 && m10 == m.m10 && m20 == m.m20 &&
      m01 == m.m01 && m11 == m.m11 && m21 == m.m21
  }

  def !=(m: inMat3x2f) :Boolean = !(this == m)

  private[math] def hasErrors: Boolean = {
    import java.lang.Float._

    (
      isNaN(m00) || isInfinite(m00) ||
      isNaN(m10) || isInfinite(m10) ||
      isNaN(m20) || isInfinite(m20) ||

      isNaN(m01) || isInfinite(m01) ||
      isNaN(m11) || isInfinite(m11) ||
      isNaN(m21) || isInfinite(m21)
    )
  }

  override def equals(other: Any) :Boolean = {
    other match {
      case m: inMat3x2f => this == m
      case _ => false
    }
  }

  override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 * (
            41 * (
              41 + m00.hashCode
            ) + m10.hashCode
          ) + m20.hashCode
        ) + m01.hashCode
      ) + m11.hashCode
    ) + m21.hashCode
  }

  override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + ", " + m20 + "; " + 
      m01 + ", " + m11 + ", " + m21 +
    ")"
  }
}

final class ConstMat3x2f private[math] (
  val m00: Float, val m10: Float, val m20: Float,
  val m01: Float, val m11: Float, val m21: Float
) extends AnyMat3x2f with Immutable

object ConstMat3x2f {

  def apply(s: Float) = new ConstMat3x2f(
    s, 0, 0,
    0, s, 0
  )

  @inline def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float
  ) = new ConstMat3x2f(
    m00, m10, m20,
    m01, m11, m21
  )

  def apply(c0: Read3[_], c1: Read3[_]) = 
  new ConstMat3x2f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: ReadMat[_]) = new ConstMat3x2f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21
  )

  implicit def toConst(m: AnyMat3x2f) = ConstMat3x2f(m)
}


final class Mat3x2f private[math] (
  var m00: Float, var m10: Float, var m20: Float,
  var m01: Float, var m11: Float, var m21: Float
) extends AnyMat3x2f with Mutable with Implicits[On]
{
  def *=(s: Float) {
    m00 *= s; m10 *= s; m20 *= s;
    m01 *= s; m11 *= s; m21 *= s
  }
  def /=(s: Float) { val inv = 1/s;
    m00 *= inv; m10 *= inv; m20 *= inv;
    m01 *= inv; m11 *= inv; m21 *= inv
  }

  def +=(s: Float) {
    m00 += s; m10 += s; m20 += s
    m01 += s; m11 += s; m21 += s
  }
  def -=(s: Float) {
    m00 -= s; m10 -= s; m20 -= s
    m01 -= s; m11 -= s; m21 -= s
  }

  def +=(m: inMat3x2f) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20;
    m01 += m.m01; m11 += m.m11; m21 += m.m21
  }
  def -=(m: inMat3x2f) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21
  }

  def *=(m: inMat2f) {
    val a00 = m00*m.m00 + m01*m.m10
    val a10 = m10*m.m00 + m11*m.m10
    val a20 = m20*m.m00 + m21*m.m10

    val a01 = m00*m.m01 + m01*m.m11
    val a11 = m10*m.m01 + m11*m.m11
    val a21 = m20*m.m01 + m21*m.m11

    m00 = a00; m10 = a10; m20 = a20
    m01 = a01; m11 = a11; m21 = a21
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat3x2f) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21
  }

  def :=(m: inMat3x2f) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20;
    m01 = m.m01; m11 = m.m11; m21 = m.m21
  }

  def set(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float
  ) {
    this.m00 = m00; this.m10 = m10; this.m20 = m20;
    this.m01 = m01; this.m11 = m11; this.m21 = m21
  }

  def update(c: Int, r: Int, s: Float) {
    def error() = throw new IndexOutOfBoundsException(
      "Trying to update index (" + c + ", " + r + ") in " +
      this.getClass.getSimpleName
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00 = s
          case 1 => m10 = s
          case 2 => m20 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01 = s
          case 1 => m11 = s
          case 2 => m21 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y
      case 1 => m01 = v.x; m11 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  def update(c: Int, v: inVec3f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }
}

object Mat3x2f {

  val Zero = ConstMat3x2f(0)
  val Identity = ConstMat3x2f(1)

  def apply(s: Float) = new Mat3x2f(
    s, 0, 0,
    0, s, 0
  )

  @inline def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float
  ) = new Mat3x2f(
    m00, m10, m20,
    m01, m11, m21
  )

  def apply(c0: Read3[_], c1: Read3[_]) = 
  new Mat3x2f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: ReadMat[_]) = new Mat3x2f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21
  )

  def unapply(m: AnyMat3x2f) = Some((m(0), m(1)))

  implicit def toMutable(m: AnyMat3x2f) = Mat3x2f(m)
}
