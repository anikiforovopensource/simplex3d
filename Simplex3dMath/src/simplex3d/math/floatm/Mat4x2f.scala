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
import simplex3d.math.integration.buffer._
import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadMat4x2f
extends ProtectedMat4x2f[Float]
{
  // Column major order.
  final def m00= p00; final def m10= p10; final def m20= p20; final def m30= p30
  final def m01= p01; final def m11= p11; final def m21= p21; final def m31= p31


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m20_=(s: Float) { throw new UnsupportedOperationException }
  protected def m30_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }
  protected def m21_=(s: Float) { throw new UnsupportedOperationException }
  protected def m31_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f10 = m10
  private[math] final override def f20 = m20
  private[math] final override def f30 = m30

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11
  private[math] final override def f21 = m21
  private[math] final override def f31 = m31


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20
  private[math] final override def d30 = m30

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21
  private[math] final override def d31 = m31


  final def apply(c: Int) :ConstVec4f = {
    c match {
      case 0 => new ConstVec4f(m00, m10, m20, m30)
      case 1 => new ConstVec4f(m01, m11, m21, m31)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
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
      case _ => error
    }
  }

  final def unary_+() :ReadMat4x2f = this
  final def unary_-() = new Mat4x2f(
    -m00, -m10, -m20, -m30,
    -m01, -m11, -m21, -m31
  )
  final def *(s: Float) = new Mat4x2f(
    s*m00, s*m10, s*m20, s*m30,
    s*m01, s*m11, s*m21, s*m31
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat4x2f(
    m00 + s, m10 + s, m20 + s, m30 + s,
    m01 + s, m11 + s, m21 + s, m31 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat4x2f) = new Mat4x2f(
    m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
    m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31
  )
  final def -(m: inMat4x2f) = new Mat4x2f(
    m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
    m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat4x2f) = new Mat4x2f(
    m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
    m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31
  )
  private[math] final def divideByComponent(s: Float) = new Mat4x2f(
    s/m00, s/m10, s/m20, s/m30,
    s/m01, s/m11, s/m21, s/m31
  )

  final def *(m: inMat2f) = new Mat4x2f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11
  )
  final def *(m: inMat2x3f) = new Mat4x3f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12,
    m30*m.m02 + m31*m.m12
  )
  final def *(m: inMat2x4f) = new Mat4f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12,
    m30*m.m02 + m31*m.m12,

    m00*m.m03 + m01*m.m13,
    m10*m.m03 + m11*m.m13,
    m20*m.m03 + m21*m.m13,
    m30*m.m03 + m31*m.m13
  )

  final def *(u: inVec2f) = new Vec4f(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y,
    m30*u.x + m31*u.y
  )
  private[math] final def transposeMul(u: inVec4f) = new Vec2f(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w
  )


  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat4x2[_] =>
        d00 == m.d00 && d10 == m.d10 && d20 == m.d20 && d30 == m.d30 &&
        d01 == m.d01 && d11 == m.d11 && d21 == m.d21 && d31 == m.d31
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
              ) + m20.hashCode
            ) + m30.hashCode
          ) + m01.hashCode
        ) + m11.hashCode
      ) + m21.hashCode
    ) + m31.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
      m01 + ", " + m11 + ", " + m21 + ", " + m31 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat4x2f private[math] (
  c00: Float, c10: Float, c20: Float, c30: Float,
  c01: Float, c11: Float, c21: Float, c31: Float
) extends ReadMat4x2f with Immutable
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31

  override def clone() = this
}

object ConstMat4x2f {
  def apply(s: Float) = new ConstMat4x2f(
    s, 0, 0, 0,
    0, s, 0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float, m30: Float,
    m01: Float, m11: Float, m21: Float, m31: Float
  ) = new ConstMat4x2f(
    m00, m10, m20, m30,
    m01, m11, m21, m31
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_]) = 
  new ConstMat4x2f(
    c0.fx, c0.fy, c0.fz, c0.fw,
    c1.fx, c1.fy, c1.fz, c1.fw
  )

  def apply(m: AnyMat[_]) = new ConstMat4x2f(
    m.f00, m.f10, m.f20, m.f30,
    m.f01, m.f11, m.f21, m.f31
  )

  implicit def toConst(m: ReadMat4x2f) = ConstMat4x2f(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat4x2f private[math] (
  c00: Float, c10: Float, c20: Float, c30: Float,
  c01: Float, c11: Float, c21: Float, c31: Float
) extends ReadMat4x2f with Implicits[On] with Composite
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31

  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }
  override def m20_=(s: Float) { p20 = s }
  override def m30_=(s: Float) { p30 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }
  override def m21_=(s: Float) { p21 = s }
  override def m31_=(s: Float) { p31 = s }

  type Element = ReadMat4x2f
  type Immutable = ConstMat4x2f
  type Component = Float1

  def *=(s: Float) {
    m00 *= s; m10 *= s; m20 *= s; m30 *= s;
    m01 *= s; m11 *= s; m21 *= s; m31 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s; m20 += s; m30 += s
    m01 += s; m11 += s; m21 += s; m31 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat4x2f) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
    m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31
  }
  def -=(m: inMat4x2f) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31
  }

  def *=(m: inMat2f) {
    val a00 = m00*m.m00 + m01*m.m10
    val a10 = m10*m.m00 + m11*m.m10
    val a20 = m20*m.m00 + m21*m.m10
    val a30 = m30*m.m00 + m31*m.m10

    val a01 = m00*m.m01 + m01*m.m11
    val a11 = m10*m.m01 + m11*m.m11
    val a21 = m20*m.m01 + m21*m.m11
    val a31 = m30*m.m01 + m31*m.m11

    m00 = a00; m10 = a10; m20 = a20; m30 = a30
    m01 = a01; m11 = a11; m21 = a21; m31 = a31
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat4x2f) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20; m30 /= m.m30
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21; m31 /= m.m31
  }

  override def clone() = Mat4x2f(this)
  
  def :=(m: inMat4x2f) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
    m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31
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

  def update(c: Int, v: inVec4f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }
}

object Mat4x2f {
  final val Zero = ConstMat4x2f(0)
  final val Identity = ConstMat4x2f(1)

  final val Manifest = classType[Mat4x2f](classOf[Mat4x2f])
  final val ConstManifest = classType[ConstMat4x2f](classOf[ConstMat4x2f])
  final val ReadManifest = classType[ReadMat4x2f](classOf[ReadMat4x2f])

  def apply(s: Float) = new Mat4x2f(
    s, 0, 0, 0,
    0, s, 0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float, m30: Float,
    m01: Float, m11: Float, m21: Float, m31: Float
  ) = new Mat4x2f(
    m00, m10, m20, m30,
    m01, m11, m21, m31
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_]) = 
  new Mat4x2f(
    c0.fx, c0.fy, c0.fz, c0.fw,
    c1.fx, c1.fy, c1.fz, c1.fw
  )

  def apply(m: AnyMat[_]) = new Mat4x2f(
    m.f00, m.f10, m.f20, m.f30,
    m.f01, m.f11, m.f21, m.f31
  )

  def unapply(m: ReadMat4x2f) = Some((m(0), m(1)))

  implicit def toMutable(m: ReadMat4x2f) = Mat4x2f(m)
}
