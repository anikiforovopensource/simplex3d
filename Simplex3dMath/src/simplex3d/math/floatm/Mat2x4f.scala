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

import scala.reflect.Manifest._
import simplex3d.math.integration._
import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadMat2x4f
extends ProtectedMat2x4f[Float, ReadMat2x4f]
{
  // Column major order.
  final def m00= p00; final def m10= p10
  final def m01= p01; final def m11= p11
  final def m02= p02; final def m12= p12
  final def m03= p03; final def m13= p13


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }

  protected def m02_=(s: Float) { throw new UnsupportedOperationException }
  protected def m12_=(s: Float) { throw new UnsupportedOperationException }

  protected def m03_=(s: Float) { throw new UnsupportedOperationException }
  protected def m13_=(s: Float) { throw new UnsupportedOperationException }


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

  final def unary_+() :ReadMat2x4f = this
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
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat2x4f(
    m00 + s, m10 + s,
    m01 + s, m11 + s,
    m02 + s, m12 + s,
    m03 + s, m13 + s
  )
  final def -(s: Float) = this + (-s)

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

  final def copyAsMutable() = Mat2x4f(this)
  final def copyAsImmutable() = ConstMat2x4f(this)

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2x4[_, _] =>
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
  c00: Float, c10: Float,
  c01: Float, c11: Float,
  c02: Float, c12: Float,
  c03: Float, c13: Float
) extends ReadMat2x4f with Immutable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12
  p03 = c03; p13 = c13
}

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

  def apply(c0: AnyVec2[_, _], c1: AnyVec2[_, _], c2: AnyVec2[_, _], c3: AnyVec2[_, _]) = 
  new ConstMat2x4f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy,
    c3.fx, c3.fy
  )

  def apply(m: AnyMat[_, _]) = new ConstMat2x4f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12,
    m.f03, m.f13
  )

  implicit def toConst(m: ReadMat2x4f) = ConstMat2x4f(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2x4f private[math] (
  c00: Float, c10: Float,
  c01: Float, c11: Float,
  c02: Float, c12: Float,
  c03: Float, c13: Float
) extends ReadMat2x4f
  with MutableObject[ReadMat2x4f] with Implicits[On] with Composite
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12
  p03 = c03; p13 = c13

  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }

  override def m02_=(s: Float) { p02 = s }
  override def m12_=(s: Float) { p12 = s }

  override def m03_=(s: Float) { p03 = s }
  override def m13_=(s: Float) { p13 = s }

  type Element = ReadMat2x4f
  type Component = Float1

  def *=(s: Float) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s;
    m02 *= s; m12 *= s;
    m03 *= s; m13 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s
    m01 += s; m11 += s
    m02 += s; m12 += s
    m03 += s; m13 += s
  }
  def -=(s: Float) { this += (-s) }

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

  override def :=(m: inMat2x4f) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11;
    m02 = m.m02; m12 = m.m12;
    m03 = m.m03; m13 = m.m13
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
  final val Zero = ConstMat2x4f(0)
  final val Identity = ConstMat2x4f(1)
  final val Manifest = classType[ReadMat2x4f](classOf[ReadMat2x4f])

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

  def apply(c0: AnyVec2[_, _], c1: AnyVec2[_, _], c2: AnyVec2[_, _], c3: AnyVec2[_, _]) = 
  new Mat2x4f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy,
    c3.fx, c3.fy
  )

  def apply(m: AnyMat[_, _]) = new Mat2x4f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12,
    m.f03, m.f13
  )

  def unapply(m: ReadMat2x4f) = Some((m(0), m(1), m(2), m(3)))

  implicit def toMutable(m: ReadMat2x4f) = Mat2x4f(m)
}
