/*
 * Simplex3dMath - Float Module
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
package floatx

import scala.language.implicitConversions
import scala.reflect._
import simplex3d.math.integration._
import simplex3d.math.types._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat3f extends ProtectedMat3f[Float]
with Protected with Serializable
{

  type Clone <: ReadMat3f
  def toConst :ConstMat3f
  
  type Read = ReadMat3f
  type Mutable = Mat3f
  final def mutableCopy = Mat3f(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01; final def m02 = p02
  final def m10 = p10; final def m11 = p11; final def m12 = p12
  final def m20 = p20; final def m21 = p21; final def m22 = p22


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m02_=(s: Float) { throw new UnsupportedOperationException }

  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }
  protected def m12_=(s: Float) { throw new UnsupportedOperationException }

  protected def m20_=(s: Float) { throw new UnsupportedOperationException }
  protected def m21_=(s: Float) { throw new UnsupportedOperationException }
  protected def m22_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f01 = m01
  private[math] final override def f02 = m02

  private[math] final override def f10 = m10
  private[math] final override def f11 = m11
  private[math] final override def f12 = m12

  private[math] final override def f20 = m20
  private[math] final override def f21 = m21
  private[math] final override def f22 = m22


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01
  private[math] final override def d02 = m02

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11
  private[math] final override def d12 = m12

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21
  private[math] final override def d22 = m22


  final def apply(c: Int) :ConstVec3f = {
    c match {
      case 0 => new ConstVec3f(m00, m01, m02)
      case 1 => new ConstVec3f(m10, m11, m12)
      case 2 => new ConstVec3f(m20, m21, m22)
      case j => throw new IndexOutOfBoundsException(
          "Trying to read column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def apply(c: Int, r: Int) :Float = {
    def error() :Float = throw new IndexOutOfBoundsException(
      "Trying to read index (" + c + ", " + r + ") in " + this.getClass.getSimpleName + "."
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00
          case 1 => m01
          case 2 => m02
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10
          case 1 => m11
          case 2 => m12
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20
          case 1 => m21
          case 2 => m22
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat3f = this
  final def unary_-() = new Mat3f(
    -m00, -m01, -m02,
    -m10, -m11, -m12,
    -m20, -m21, -m22
  )
  final def *(s: Float) = new Mat3f(
    s*m00, s*m01, s*m02,
    s*m10, s*m11, s*m12,
    s*m20, s*m21, s*m22
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat3f(
    m00 + s, m01 + s, m02 + s,
    m10 + s, m11 + s, m12 + s,
    m20 + s, m21 + s, m22 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat3f) = new Mat3f(
    m00 + m.m00, m01 + m.m01, m02 + m.m02,
    m10 + m.m10, m11 + m.m11, m12 + m.m12,
    m20 + m.m20, m21 + m.m21, m22 + m.m22
  )
  final def -(m: inMat3f) = new Mat3f(
    m00 - m.m00, m01 - m.m01, m02 - m.m02,
    m10 - m.m10, m11 - m.m11, m12 - m.m12,
    m20 - m.m20, m21 - m.m21, m22 - m.m22
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat3f) = new Mat3f(
    m00/m.m00, m01/m.m01, m02/m.m02,
    m10/m.m10, m11/m.m11, m12/m.m12,
    m20/m.m20, m21/m.m21, m22/m.m22
  )
  private[math] final def divByComp(s: Float) = new Mat3f(
    s/m00, s/m01, s/m02,
    s/m10, s/m11, s/m12,
    s/m20, s/m21, s/m22
  )

  final def *(m: inMat2x3f) = new Mat2x3f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12
  )
  final def *(m: inMat3f) = new Mat3f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,
    m02*m.m20 + m12*m.m21 + m22*m.m22
  )
  final def *(m: inMat4x3f) = new Mat4x3f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,
    m02*m.m20 + m12*m.m21 + m22*m.m22,

    m00*m.m30 + m10*m.m31 + m20*m.m32,
    m01*m.m30 + m11*m.m31 + m21*m.m32,
    m02*m.m30 + m12*m.m31 + m22*m.m32
  )

  final def *(u: inVec3f) = new Vec3f(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z,
    m02*u.x + m12*u.y + m22*u.z
  )
  private[math] final def transposeMult(u: inVec3f) = new Vec3f(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z,
    m20*u.x + m21*u.y + m22*u.z
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat3[_] =>
        d00 == m.d00 && d01 == m.d01 && d02 == m.d02 &&
        d10 == m.d10 && d11 == m.d11 && d12 == m.d12 &&
        d20 == m.d20 && d21 == m.d21 && d22 == m.d22
      case _ =>
        false
    }
  }

  final override def hashCode :Int = {
    41 * (
      41 * (
        41 * (
          41 * (
            41 * (
              41 * (
                41 * (
                  41 * (
                    41 + simplex3d.math.floatHashCode(m00)
                  ) + simplex3d.math.floatHashCode(m01)
                ) + simplex3d.math.floatHashCode(m02)
              ) + simplex3d.math.floatHashCode(m10)
            ) + simplex3d.math.floatHashCode(m11)
          ) + simplex3d.math.floatHashCode(m12)
        ) + simplex3d.math.floatHashCode(m20)
      ) + simplex3d.math.floatHashCode(m21)
    ) + simplex3d.math.floatHashCode(m22)
  }

  final override def toString :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat3" +
    "(" +
      m00 + "f, " + m01 + "f, " + m02 + "f,   " + 
      m10 + "f, " + m11 + "f, " + m12 + "f,   " + 
      m20 + "f, " + m21 + "f, " + m22 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3f private[math] (
  c00: Float, c01: Float, c02: Float,
  c10: Float, c11: Float, c12: Float,
  c20: Float, c21: Float, c22: Float
) extends ReadMat3f with Immutable with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12
  p20 = c20; p21 = c21; p22 = c22


  type Clone = ConstMat3f
  override def clone = this
  def toConst = this
}

object ConstMat3f {
  def apply(s: Float) = new ConstMat3f(
    s, 0, 0,
    0, s, 0,
    0, 0, s
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float, m02: Float,
    m10: Float, m11: Float, m12: Float,
    m20: Float, m21: Float, m22: Float
  ) = new ConstMat3f(
    m00, m01, m02,
    m10, m11, m12,
    m20, m21, m22
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_]) = 
  new ConstMat3f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz,
    c2.fx, c2.fy, c2.fz
  )

  def apply(m: AnyMat[_]) = new ConstMat3f(
    m.f00, m.f01, m.f02,
    m.f10, m.f11, m.f12,
    m.f20, m.f21, m.f22
  )

  implicit def toConst(m: ReadMat3f) = ConstMat3f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3f private[math] (
  c00: Float, c01: Float, c02: Float,
  c10: Float, c11: Float, c12: Float,
  c20: Float, c21: Float, c22: Float
)
extends ReadMat3f with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12
  p20 = c20; p21 = c21; p22 = c22

  private[math] def this() = this(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  )
  
  type Clone = Mat3f
  type Const = ConstMat3f

  type Accessor = Mat3f
  type Component = RFloat

  override def clone = Mat3f(this)
  def toConst = ConstMat3f(this)

  def :=(m: inMat3f) {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
    m20 = m.m20; m21 = m.m21; m22 = m.m22
  }


  override def m00_=(s: Float) { p00 = s }
  override def m01_=(s: Float) { p01 = s }
  override def m02_=(s: Float) { p02 = s }

  override def m10_=(s: Float) { p10 = s }
  override def m11_=(s: Float) { p11 = s }
  override def m12_=(s: Float) { p12 = s }

  override def m20_=(s: Float) { p20 = s }
  override def m21_=(s: Float) { p21 = s }
  override def m22_=(s: Float) { p22 = s }


  def *=(s: Float) {
    m00 *= s; m01 *= s; m02 *= s;
    m10 *= s; m11 *= s; m12 *= s;
    m20 *= s; m21 *= s; m22 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m01 += s; m02 += s
    m10 += s; m11 += s; m12 += s
    m20 += s; m21 += s; m22 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat3f) {
    m00 += m.m00; m01 += m.m01; m02 += m.m02;
    m10 += m.m10; m11 += m.m11; m12 += m.m12;
    m20 += m.m20; m21 += m.m21; m22 += m.m22
  }
  def -=(m: inMat3f) {
    m00 -= m.m00; m01 -= m.m01; m02 -= m.m02;
    m10 -= m.m10; m11 -= m.m11; m12 -= m.m12;
    m20 -= m.m20; m21 -= m.m21; m22 -= m.m22
  }

  def *=(m: inMat3f) {
    val t00 = m00*m.m00 + m10*m.m01 + m20*m.m02
    val t01 = m01*m.m00 + m11*m.m01 + m21*m.m02
    val t02 = m02*m.m00 + m12*m.m01 + m22*m.m02

    val t10 = m00*m.m10 + m10*m.m11 + m20*m.m12
    val t11 = m01*m.m10 + m11*m.m11 + m21*m.m12
    val t12 = m02*m.m10 + m12*m.m11 + m22*m.m12

    val t20 = m00*m.m20 + m10*m.m21 + m20*m.m22
    val t21 = m01*m.m20 + m11*m.m21 + m21*m.m22
        m22 = m02*m.m20 + m12*m.m21 + m22*m.m22

    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11; m12 = t12
    m20 = t20; m21 = t21
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat3f) {
    m00 /= m.m00; m01 /= m.m01; m02 /= m.m02
    m10 /= m.m10; m11 /= m.m11; m12 /= m.m12
    m20 /= m.m20; m21 /= m.m21; m22 /= m.m22
  }


  def update(c: Int, r: Int, s: Float) {
    def error() = throw new IndexOutOfBoundsException(
      "Trying to update index (" + c + ", " + r + ") in " + this.getClass.getSimpleName + "."
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00 = s
          case 1 => m01 = s
          case 2 => m02 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10 = s
          case 1 => m11 = s
          case 2 => m12 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20 = s
          case 1 => m21 = s
          case 2 => m22 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m01 = v.y
      case 1 => m10 = v.x; m11 = v.y
      case 2 => m20 = v.x; m21 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def update(c: Int, v: inVec3f) {
    c match {
      case 0 => m00 = v.x; m01 = v.y; m02 = v.z
      case 1 => m10 = v.x; m11 = v.y; m12 = v.z
      case 2 => m20 = v.x; m21 = v.y; m22 = v.z
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat3f {
  final val Zero = ConstMat3f(0)
  final val Identity = ConstMat3f(1)

  final val Tag = classTag[Mat3f]
  final val ConstTag = classTag[ConstMat3f]
  final val ReadTag = classTag[ReadMat3f]

  def apply(s: Float) = new Mat3f(
    s, 0, 0,
    0, s, 0,
    0, 0, s
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float, m02: Float,
    m10: Float, m11: Float, m12: Float,
    m20: Float, m21: Float, m22: Float
  ) = new Mat3f(
    m00, m01, m02,
    m10, m11, m12,
    m20, m21, m22
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_]) = 
  new Mat3f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz,
    c2.fx, c2.fy, c2.fz
  )

  def apply(m: AnyMat[_]) = new Mat3f(
    m.f00, m.f01, m.f02,
    m.f10, m.f11, m.f12,
    m.f20, m.f21, m.f22
  )

  def unapply(m: ReadMat3f) = Some((m(0), m(1), m(2)))
}
