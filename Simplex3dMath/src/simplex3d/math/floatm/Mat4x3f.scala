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
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat4x3f extends Read4x3[ConstVec4f]
{
  // Column major order.
  def m00: Float; def m10: Float; def m20: Float; def m30: Float // column
  def m01: Float; def m11: Float; def m21: Float; def m31: Float // column
  def m02: Float; def m12: Float; def m22: Float; def m32: Float // column

  private[math] final override def f00 = m00
  private[math] final override def f10 = m10
  private[math] final override def f20 = m20
  private[math] final override def f30 = m30

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11
  private[math] final override def f21 = m21
  private[math] final override def f31 = m31

  private[math] final override def f02 = m02
  private[math] final override def f12 = m12
  private[math] final override def f22 = m22
  private[math] final override def f32 = m32


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20
  private[math] final override def d30 = m30

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21
  private[math] final override def d31 = m31

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12
  private[math] final override def d22 = m22
  private[math] final override def d32 = m32


  final def apply(c: Int) :ConstVec4f = {
    c match {
      case 0 => new ConstVec4f(m00, m10, m20, m30)
      case 1 => new ConstVec4f(m01, m11, m21, m31)
      case 2 => new ConstVec4f(m02, m12, m22, m32)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  final def apply(c: Int, r: Int) :Float = {
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
          case 3 => m30
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case 2 => m21
          case 3 => m31
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02
          case 1 => m12
          case 2 => m22
          case 3 => m32
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :AnyMat4x3f = this
  final def unary_-() = new Mat4x3f(
    -m00, -m10, -m20, -m30,
    -m01, -m11, -m21, -m31,
    -m02, -m12, -m22, -m32
  )
  final def *(s: Float) = new Mat4x3f(
    s*m00, s*m10, s*m20, s*m30,
    s*m01, s*m11, s*m21, s*m31,
    s*m02, s*m12, s*m22, s*m32
  )
  final def /(s: Float) = { val inv = 1/s; new Mat4x3f(
    inv*m00, inv*m10, inv*m20, inv*m30,
    inv*m01, inv*m11, inv*m21, inv*m31,
    inv*m02, inv*m12, inv*m22, inv*m32
  )}

  final def +(s: Float) = new Mat4x3f(
    m00 + s, m10 + s, m20 + s, m30 + s,
    m01 + s, m11 + s, m21 + s, m31 + s,
    m02 + s, m12 + s, m22 + s, m32 + s
  )
  final def -(s: Float) = new Mat4x3f(
    m00 - s, m10 - s, m20 - s, m30 - s,
    m01 - s, m11 - s, m21 - s, m31 - s,
    m02 - s, m12 - s, m22 - s, m32 - s
  )

  final def +(m: inMat4x3f) = new Mat4x3f(
    m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
    m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31,
    m02 + m.m02, m12 + m.m12, m22 + m.m22, m32 + m.m32
  )
  final def -(m: inMat4x3f) = new Mat4x3f(
    m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
    m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31,
    m02 - m.m02, m12 - m.m12, m22 - m.m22, m32 - m.m32
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat4x3f) = new Mat4x3f(
    m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
    m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31,
    m02/m.m02, m12/m.m12, m22/m.m22, m32/m.m32
  )
  private[math] final def divideByComponent(s: Float) = new Mat4x3f(
    s/m00, s/m10, s/m20, s/m30,
    s/m01, s/m11, s/m21, s/m31,
    s/m02, s/m12, s/m22, s/m32
  )

  final def *(m: inMat3x2f) = new Mat4x2f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,
    m20*m.m00 + m21*m.m10 + m22*m.m20,
    m30*m.m00 + m31*m.m10 + m32*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,
    m20*m.m01 + m21*m.m11 + m22*m.m21,
    m30*m.m01 + m31*m.m11 + m32*m.m21
  )
  final def *(m: inMat3f) = new Mat4x3f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,
    m20*m.m00 + m21*m.m10 + m22*m.m20,
    m30*m.m00 + m31*m.m10 + m32*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,
    m20*m.m01 + m21*m.m11 + m22*m.m21,
    m30*m.m01 + m31*m.m11 + m32*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22,
    m20*m.m02 + m21*m.m12 + m22*m.m22,
    m30*m.m02 + m31*m.m12 + m32*m.m22
  )
  final def *(m: inMat3x4f) = new Mat4f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,
    m20*m.m00 + m21*m.m10 + m22*m.m20,
    m30*m.m00 + m31*m.m10 + m32*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,
    m20*m.m01 + m21*m.m11 + m22*m.m21,
    m30*m.m01 + m31*m.m11 + m32*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22,
    m20*m.m02 + m21*m.m12 + m22*m.m22,
    m30*m.m02 + m31*m.m12 + m32*m.m22,

    m00*m.m03 + m01*m.m13 + m02*m.m23,
    m10*m.m03 + m11*m.m13 + m12*m.m23,
    m20*m.m03 + m21*m.m13 + m22*m.m23,
    m30*m.m03 + m31*m.m13 + m32*m.m23
  )

  final def *(u: inVec3f) = new Vec4f(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z,
    m20*u.x + m21*u.y + m22*u.z,
    m30*u.x + m31*u.y + m32*u.z
  )
  private[math] final def transposeMul(u: inVec4f) = new Vec3f(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w,
    m02*u.x + m12*u.y + m22*u.z + m32*u.w
  )

  final def ==(m: inMat4x3f) :Boolean = {
    if (m eq null) false
    else
      m00 == m.m00 && m10 == m.m10 && m20 == m.m20 && m30 == m.m30 &&
      m01 == m.m01 && m11 == m.m11 && m21 == m.m21 && m31 == m.m31 &&
      m02 == m.m02 && m12 == m.m12 && m22 == m.m22 && m32 == m.m32
  }

  final def !=(m: inMat4x3f) :Boolean = !(this == m)

  private[math] final def hasErrors: Boolean = {
    import java.lang.Float._

    (
      isNaN(m00) || isInfinite(m00) ||
      isNaN(m10) || isInfinite(m10) ||
      isNaN(m20) || isInfinite(m20) ||
      isNaN(m30) || isInfinite(m30) ||

      isNaN(m01) || isInfinite(m01) ||
      isNaN(m11) || isInfinite(m11) ||
      isNaN(m21) || isInfinite(m21) ||
      isNaN(m31) || isInfinite(m31) ||

      isNaN(m02) || isInfinite(m02) ||
      isNaN(m12) || isInfinite(m12) ||
      isNaN(m22) || isInfinite(m22) ||
      isNaN(m32) || isInfinite(m32)
    )
  }

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: inMat4x3f => this == m
      case _ => false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 * (
            41 * (
              41 * (
                41 * (
                  41 * (
                    41 * (
                      41 * (
                        41 * (
                          41 + m00.hashCode
                        ) + m10.hashCode
                      ) + m20.hashCode
                    ) + m30.hashCode
                  ) + m01.hashCode
                ) + m11.hashCode
              ) + m21.hashCode
            ) + m31.hashCode
          ) + m02.hashCode
        ) + m12.hashCode
      ) + m22.hashCode
    ) + m32.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
      m01 + ", " + m11 + ", " + m21 + ", " + m31 + "; " + 
      m02 + ", " + m12 + ", " + m22 + ", " + m32 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat4x3f private[math] (
  val m00: Float, val m10: Float, val m20: Float, val m30: Float,
  val m01: Float, val m11: Float, val m21: Float, val m31: Float,
  val m02: Float, val m12: Float, val m22: Float, val m32: Float
) extends AnyMat4x3f with Immutable

object ConstMat4x3f {

  def apply(s: Float) = new ConstMat4x3f(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0
  )

  /* @inline */ def apply(
    m00: Float, m10: Float, m20: Float, m30: Float,
    m01: Float, m11: Float, m21: Float, m31: Float,
    m02: Float, m12: Float, m22: Float, m32: Float
  ) = new ConstMat4x3f(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32
  )

  def apply(c0: Read4[_], c1: Read4[_], c2: Read4[_]) = 
  new ConstMat4x3f(
    c0.fx, c0.fy, c0.fz, c0.fw,
    c1.fx, c1.fy, c1.fz, c1.fw,
    c2.fx, c2.fy, c2.fz, c2.fw
  )

  def apply(m: ReadMat[_]) = new ConstMat4x3f(
    m.f00, m.f10, m.f20, m.f30,
    m.f01, m.f11, m.f21, m.f31,
    m.f02, m.f12, m.f22, m.f32
  )

  implicit def toConst(m: AnyMat4x3f) = ConstMat4x3f(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat4x3f private[math] (
  var m00: Float, var m10: Float, var m20: Float, var m30: Float,
  var m01: Float, var m11: Float, var m21: Float, var m31: Float,
  var m02: Float, var m12: Float, var m22: Float, var m32: Float
) extends AnyMat4x3f with Mutable with Implicits[On] with Composite
{
  type Element = AnyMat4x3f
  type Component = Float1

  def *=(s: Float) {
    m00 *= s; m10 *= s; m20 *= s; m30 *= s;
    m01 *= s; m11 *= s; m21 *= s; m31 *= s;
    m02 *= s; m12 *= s; m22 *= s; m32 *= s
  }
  def /=(s: Float) { val inv = 1/s;
    m00 *= inv; m10 *= inv; m20 *= inv; m30 *= inv;
    m01 *= inv; m11 *= inv; m21 *= inv; m31 *= inv;
    m02 *= inv; m12 *= inv; m22 *= inv; m32 *= inv
  }

  def +=(s: Float) {
    m00 += s; m10 += s; m20 += s; m30 += s
    m01 += s; m11 += s; m21 += s; m31 += s
    m02 += s; m12 += s; m22 += s; m32 += s
  }
  def -=(s: Float) {
    m00 -= s; m10 -= s; m20 -= s; m30 -= s
    m01 -= s; m11 -= s; m21 -= s; m31 -= s
    m02 -= s; m12 -= s; m22 -= s; m32 -= s
  }

  def +=(m: inMat4x3f) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
    m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31;
    m02 += m.m02; m12 += m.m12; m22 += m.m22; m32 += m.m32
  }
  def -=(m: inMat4x3f) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31;
    m02 -= m.m02; m12 -= m.m12; m22 -= m.m22; m32 -= m.m32
  }

  def *=(m: inMat3f) {
    val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
    val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
    val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20
    val a30 = m30*m.m00 + m31*m.m10 + m32*m.m20

    val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
    val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
    val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21
    val a31 = m30*m.m01 + m31*m.m11 + m32*m.m21

    val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
    val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22
    val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22
    val a32 = m30*m.m02 + m31*m.m12 + m32*m.m22

    m00 = a00; m10 = a10; m20 = a20; m30 = a30
    m01 = a01; m11 = a11; m21 = a21; m31 = a31
    m02 = a02; m12 = a12; m22 = a22; m32 = a32
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat4x3f) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20; m30 /= m.m30
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21; m31 /= m.m31
    m02 /= m.m02; m12 /= m.m12; m22 /= m.m22; m32 /= m.m32
  }

  def :=(m: inMat4x3f) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
    m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31;
    m02 = m.m02; m12 = m.m12; m22 = m.m22; m32 = m.m32
  }

  def set(
    m00: Float, m10: Float, m20: Float, m30: Float,
    m01: Float, m11: Float, m21: Float, m31: Float,
    m02: Float, m12: Float, m22: Float, m32: Float
  ) {
    this.m00 = m00; this.m10 = m10; this.m20 = m20; this.m30 = m30;
    this.m01 = m01; this.m11 = m11; this.m21 = m21; this.m31 = m31;
    this.m02 = m02; this.m12 = m12; this.m22 = m22; this.m32 = m32
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
          case 3 => m30 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01 = s
          case 1 => m11 = s
          case 2 => m21 = s
          case 3 => m31 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02 = s
          case 1 => m12 = s
          case 2 => m22 = s
          case 3 => m32 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y
      case 1 => m01 = v.x; m11 = v.y
      case 2 => m02 = v.x; m12 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  def update(c: Int, v: inVec3f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z
      case 2 => m02 = v.x; m12 = v.y; m22 = v.z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }

  def update(c: Int, v: inVec4f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
      case 2 => m02 = v.x; m12 = v.y; m22 = v.z; m32 = v.w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }
}

object Mat4x3f {

  val Zero = ConstMat4x3f(0)
  val Identity = ConstMat4x3f(1)

  def apply(s: Float) = new Mat4x3f(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0
  )

  /* @inline */ def apply(
    m00: Float, m10: Float, m20: Float, m30: Float,
    m01: Float, m11: Float, m21: Float, m31: Float,
    m02: Float, m12: Float, m22: Float, m32: Float
  ) = new Mat4x3f(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32
  )

  def apply(c0: Read4[_], c1: Read4[_], c2: Read4[_]) = 
  new Mat4x3f(
    c0.fx, c0.fy, c0.fz, c0.fw,
    c1.fx, c1.fy, c1.fz, c1.fw,
    c2.fx, c2.fy, c2.fz, c2.fw
  )

  def apply(m: ReadMat[_]) = new Mat4x3f(
    m.f00, m.f10, m.f20, m.f30,
    m.f01, m.f11, m.f21, m.f31,
    m.f02, m.f12, m.f22, m.f32
  )

  def unapply(m: AnyMat4x3f) = Some((m(0), m(1), m(2)))

  implicit def toMutable(m: AnyMat4x3f) = Mat4x3f(m)
}
