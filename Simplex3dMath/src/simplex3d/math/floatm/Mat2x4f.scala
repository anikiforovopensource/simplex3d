/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat2x4f extends Read2x4[Float]
{
  // Column major order.
  def m00: Float; def m10: Float // column
  def m01: Float; def m11: Float // column
  def m02: Float; def m12: Float // column
  def m03: Float; def m13: Float // column

  private[math] final override def f00 = m00
  private[math] final override def f10 = m10

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11

  private[math] final override def f02 = m02
  private[math] final override def f12 = m12

  private[math] final override def f03 = m03
  private[math] final override def f13 = m13


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12

  private[math] final override def d03 = m03
  private[math] final override def d13 = m13


  final def apply(c: Int) :ConstVec2f = {
    c match {
      case 0 => new ConstVec2f(m00, m10)
      case 1 => new ConstVec2f(m01, m11)
      case 2 => new ConstVec2f(m02, m12)
      case 3 => new ConstVec2f(m03, m13)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02
          case 1 => m12
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03
          case 1 => m13
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :AnyMat2x4f = this
  final def unary_-() = new Mat2x4f(
    -m00, -m10,
    -m01, -m11,
    -m02, -m12,
    -m03, -m13
  )
  final def *(s: Float) = new Mat2x4f(
    s*m00, s*m10,
    s*m01, s*m11,
    s*m02, s*m12,
    s*m03, s*m13
  )
  final def /(s: Float) = { val inv = 1/s; new Mat2x4f(
    inv*m00, inv*m10,
    inv*m01, inv*m11,
    inv*m02, inv*m12,
    inv*m03, inv*m13
  )}

  final def +(s: Float) = new Mat2x4f(
    m00 + s, m10 + s,
    m01 + s, m11 + s,
    m02 + s, m12 + s,
    m03 + s, m13 + s
  )
  final def -(s: Float) = new Mat2x4f(
    m00 - s, m10 - s,
    m01 - s, m11 - s,
    m02 - s, m12 - s,
    m03 - s, m13 - s
  )

  final def +(m: inMat2x4f) = new Mat2x4f(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11,
    m02 + m.m02, m12 + m.m12,
    m03 + m.m03, m13 + m.m13
  )
  final def -(m: inMat2x4f) = new Mat2x4f(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11,
    m02 - m.m02, m12 - m.m12,
    m03 - m.m03, m13 - m.m13
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2x4f) = new Mat2x4f(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11,
    m02/m.m02, m12/m.m12,
    m03/m.m03, m13/m.m13
  )
  private[math] final def divideByComponent(s: Float) = new Mat2x4f(
    s/m00, s/m10,
    s/m01, s/m11,
    s/m02, s/m12,
    s/m03, s/m13
  )

  final def *(m: inMat4x2f) = new Mat2f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
  )
  final def *(m: inMat4x3f) = new Mat2x3f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
  )
  final def *(m: inMat4f) = new Mat2x4f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,

    m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
    m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
  )

  final def *(u: inVec4f) = new Vec2f(
    m00*u.x + m01*u.y + m02*u.z + m03*u.w,
    m10*u.x + m11*u.y + m12*u.z + m13*u.w
  )
  private[math] final def transposeMul(u: inVec2f) = new Vec4f(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y,
    m02*u.x + m12*u.y,
    m03*u.x + m13*u.y
  )

  private[math] final def hasErrors: Boolean = {
    import java.lang.Float._

    (
      isNaN(m00) || isInfinite(m00) ||
      isNaN(m10) || isInfinite(m10) ||

      isNaN(m01) || isInfinite(m01) ||
      isNaN(m11) || isInfinite(m11) ||

      isNaN(m02) || isInfinite(m02) ||
      isNaN(m12) || isInfinite(m12) ||

      isNaN(m03) || isInfinite(m03) ||
      isNaN(m13) || isInfinite(m13)
    )
  }

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: Read2x4[_] =>
        d00 == m.d00 && d10 == m.d10 &&
        d01 == m.d01 && d11 == m.d11 &&
        d02 == m.d02 && d12 == m.d12 &&
        d03 == m.d03 && d13 == m.d13
      case _ =>
        false
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
                  41 + m00.hashCode
                ) + m10.hashCode
              ) + m01.hashCode
            ) + m11.hashCode
          ) + m02.hashCode
        ) + m12.hashCode
      ) + m03.hashCode
    ) + m13.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + "; " + 
      m01 + ", " + m11 + "; " + 
      m02 + ", " + m12 + "; " + 
      m03 + ", " + m13 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat2x4f private[math] (
  val m00: Float, val m10: Float,
  val m01: Float, val m11: Float,
  val m02: Float, val m12: Float,
  val m03: Float, val m13: Float
) extends AnyMat2x4f with Immutable

object ConstMat2x4f {

  def apply(s: Float) = new ConstMat2x4f(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /* main factory */ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float,
    m02: Float, m12: Float,
    m03: Float, m13: Float
  ) = new ConstMat2x4f(
    m00, m10,
    m01, m11,
    m02, m12,
    m03, m13
  )

  def apply(c0: Read2[_], c1: Read2[_], c2: Read2[_], c3: Read2[_]) = 
  new ConstMat2x4f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy,
    c3.fx, c3.fy
  )

  def apply(m: ReadMat[_]) = new ConstMat2x4f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12,
    m.f03, m.f13
  )

  implicit def toConst(m: AnyMat2x4f) = ConstMat2x4f(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2x4f private[math] (
  var m00: Float, var m10: Float,
  var m01: Float, var m11: Float,
  var m02: Float, var m12: Float,
  var m03: Float, var m13: Float
) extends AnyMat2x4f with Mutable with Implicits[On] with Composite
{
  type Element = AnyMat2x4f
  type Component = Float1

  def *=(s: Float) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s;
    m02 *= s; m12 *= s;
    m03 *= s; m13 *= s
  }
  def /=(s: Float) { val inv = 1/s;
    m00 *= inv; m10 *= inv;
    m01 *= inv; m11 *= inv;
    m02 *= inv; m12 *= inv;
    m03 *= inv; m13 *= inv
  }

  def +=(s: Float) {
    m00 += s; m10 += s
    m01 += s; m11 += s
    m02 += s; m12 += s
    m03 += s; m13 += s
  }
  def -=(s: Float) {
    m00 -= s; m10 -= s
    m01 -= s; m11 -= s
    m02 -= s; m12 -= s
    m03 -= s; m13 -= s
  }

  def +=(m: inMat2x4f) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11;
    m02 += m.m02; m12 += m.m12;
    m03 += m.m03; m13 += m.m13
  }
  def -=(m: inMat2x4f) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11;
    m02 -= m.m02; m12 -= m.m12;
    m03 -= m.m03; m13 -= m.m13
  }

  def *=(m: inMat4f) {
    val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
    val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30

    val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
    val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31

    val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
    val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32

    val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
    val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33

    m00 = a00; m10 = a10
    m01 = a01; m11 = a11
    m02 = a02; m12 = a12
    m03 = a03; m13 = a13
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat2x4f) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
    m02 /= m.m02; m12 /= m.m12
    m03 /= m.m03; m13 /= m.m13
  }

  def :=(m: inMat2x4f) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11;
    m02 = m.m02; m12 = m.m12;
    m03 = m.m03; m13 = m.m13
  }

  def set(
    m00: Float, m10: Float,
    m01: Float, m11: Float,
    m02: Float, m12: Float,
    m03: Float, m13: Float
  ) {
    this.m00 = m00; this.m10 = m10;
    this.m01 = m01; this.m11 = m11;
    this.m02 = m02; this.m12 = m12;
    this.m03 = m03; this.m13 = m13
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01 = s
          case 1 => m11 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02 = s
          case 1 => m12 = s
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03 = s
          case 1 => m13 = s
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
      case 3 => m03 = v.x; m13 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }
}

object Mat2x4f {

  val Zero = ConstMat2x4f(0)
  val Identity = ConstMat2x4f(1)

  def apply(s: Float) = new Mat2x4f(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /* main factory */ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float,
    m02: Float, m12: Float,
    m03: Float, m13: Float
  ) = new Mat2x4f(
    m00, m10,
    m01, m11,
    m02, m12,
    m03, m13
  )

  def apply(c0: Read2[_], c1: Read2[_], c2: Read2[_], c3: Read2[_]) = 
  new Mat2x4f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy,
    c3.fx, c3.fy
  )

  def apply(m: ReadMat[_]) = new Mat2x4f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12,
    m.f03, m.f13
  )

  def unapply(m: AnyMat2x4f) = Some((m(0), m(1), m(2), m(3)))

  implicit def toMutable(m: AnyMat2x4f) = Mat2x4f(m)
}
