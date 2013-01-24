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
sealed abstract class ReadMat2x3f extends ProtectedMat2x3f[Float]
with Protected with Serializable
{

  type Clone <: ReadMat2x3f
  def toConst :ConstMat2x3f
  
  type Read = ReadMat2x3f
  type Mutable = Mat2x3f
  final def mutableCopy = Mat2x3f(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01; final def m02 = p02
  final def m10 = p10; final def m11 = p11; final def m12 = p12


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m02_=(s: Float) { throw new UnsupportedOperationException }

  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }
  protected def m12_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f01 = m01
  private[math] final override def f02 = m02

  private[math] final override def f10 = m10
  private[math] final override def f11 = m11
  private[math] final override def f12 = m12


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01
  private[math] final override def d02 = m02

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11
  private[math] final override def d12 = m12


  final def apply(c: Int) :ConstVec3f = {
    c match {
      case 0 => new ConstVec3f(m00, m01, m02)
      case 1 => new ConstVec3f(m10, m11, m12)
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
      case _ => error
    }
  }

  final def unary_+() :ReadMat2x3f = this
  final def unary_-() = new Mat2x3f(
    -m00, -m01, -m02,
    -m10, -m11, -m12
  )
  final def *(s: Float) = new Mat2x3f(
    s*m00, s*m01, s*m02,
    s*m10, s*m11, s*m12
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat2x3f(
    m00 + s, m01 + s, m02 + s,
    m10 + s, m11 + s, m12 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat2x3f) = new Mat2x3f(
    m00 + m.m00, m01 + m.m01, m02 + m.m02,
    m10 + m.m10, m11 + m.m11, m12 + m.m12
  )
  final def -(m: inMat2x3f) = new Mat2x3f(
    m00 - m.m00, m01 - m.m01, m02 - m.m02,
    m10 - m.m10, m11 - m.m11, m12 - m.m12
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat2x3f) = new Mat2x3f(
    m00/m.m00, m01/m.m01, m02/m.m02,
    m10/m.m10, m11/m.m11, m12/m.m12
  )
  private[math] final def divByComp(s: Float) = new Mat2x3f(
    s/m00, s/m01, s/m02,
    s/m10, s/m11, s/m12
  )

  final def *(m: inMat2f) = new Mat2x3f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,
    m02*m.m00 + m12*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,
    m02*m.m10 + m12*m.m11
  )
  final def *(m: inMat3x2f) = new Mat3f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,
    m02*m.m00 + m12*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,
    m02*m.m10 + m12*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21,
    m02*m.m20 + m12*m.m21
  )
  final def *(m: inMat4x2f) = new Mat4x3f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,
    m02*m.m00 + m12*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,
    m02*m.m10 + m12*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21,
    m02*m.m20 + m12*m.m21,

    m00*m.m30 + m10*m.m31,
    m01*m.m30 + m11*m.m31,
    m02*m.m30 + m12*m.m31
  )

  final def *(u: inVec2f) = new Vec3f(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y,
    m02*u.x + m12*u.y
  )
  private[math] final def transposeMult(u: inVec3f) = new Vec2f(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2x3[_] =>
        d00 == m.d00 && d01 == m.d01 && d02 == m.d02 &&
        d10 == m.d10 && d11 == m.d11 && d12 == m.d12
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
              41 + simplex3d.math.floatHashCode(m00)
            ) + simplex3d.math.floatHashCode(m01)
          ) + simplex3d.math.floatHashCode(m02)
        ) + simplex3d.math.floatHashCode(m10)
      ) + simplex3d.math.floatHashCode(m11)
    ) + simplex3d.math.floatHashCode(m12)
  }

  final override def toString :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2x3" +
    "(" +
      m00 + "f, " + m01 + "f, " + m02 + "f,   " + 
      m10 + "f, " + m11 + "f, " + m12 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat2x3f private[math] (
  c00: Float, c01: Float, c02: Float,
  c10: Float, c11: Float, c12: Float
) extends ReadMat2x3f with Immutable with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12


  type Clone = ConstMat2x3f
  override def clone = this
  def toConst = this
}

object ConstMat2x3f {
  def apply(s: Float) = new ConstMat2x3f(
    s, 0, 0,
    0, s, 0
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float, m02: Float,
    m10: Float, m11: Float, m12: Float
  ) = new ConstMat2x3f(
    m00, m01, m02,
    m10, m11, m12
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_]) = 
  new ConstMat2x3f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: AnyMat[_]) = new ConstMat2x3f(
    m.f00, m.f01, m.f02,
    m.f10, m.f11, m.f12
  )

  implicit def toConst(m: ReadMat2x3f) = ConstMat2x3f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat2x3f private[math] (
  c00: Float, c01: Float, c02: Float,
  c10: Float, c11: Float, c12: Float
)
extends ReadMat2x3f with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12

  private[math] def this() = this(
    1, 0, 0,
    0, 1, 0
  )
  
  type Clone = Mat2x3f
  type Const = ConstMat2x3f

  type Accessor = Mat2x3f
  type Component = RFloat

  override def clone = Mat2x3f(this)
  def toConst = ConstMat2x3f(this)

  def :=(m: inMat2x3f) {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
  }


  override def m00_=(s: Float) { p00 = s }
  override def m01_=(s: Float) { p01 = s }
  override def m02_=(s: Float) { p02 = s }

  override def m10_=(s: Float) { p10 = s }
  override def m11_=(s: Float) { p11 = s }
  override def m12_=(s: Float) { p12 = s }


  def *=(s: Float) {
    m00 *= s; m01 *= s; m02 *= s;
    m10 *= s; m11 *= s; m12 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m01 += s; m02 += s
    m10 += s; m11 += s; m12 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat2x3f) {
    m00 += m.m00; m01 += m.m01; m02 += m.m02;
    m10 += m.m10; m11 += m.m11; m12 += m.m12
  }
  def -=(m: inMat2x3f) {
    m00 -= m.m00; m01 -= m.m01; m02 -= m.m02;
    m10 -= m.m10; m11 -= m.m11; m12 -= m.m12
  }

  def *=(m: inMat2f) {
    val t00 = m00*m.m00 + m10*m.m01
    val t01 = m01*m.m00 + m11*m.m01
    val t02 = m02*m.m00 + m12*m.m01

    val t10 = m00*m.m10 + m10*m.m11
    val t11 = m01*m.m10 + m11*m.m11
        m12 = m02*m.m10 + m12*m.m11

    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat2x3f) {
    m00 /= m.m00; m01 /= m.m01; m02 /= m.m02
    m10 /= m.m10; m11 /= m.m11; m12 /= m.m12
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
      case _ => error
    }
  }

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m01 = v.y
      case 1 => m10 = v.x; m11 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def update(c: Int, v: inVec3f) {
    c match {
      case 0 => m00 = v.x; m01 = v.y; m02 = v.z
      case 1 => m10 = v.x; m11 = v.y; m12 = v.z
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat2x3f {
  final val Zero = ConstMat2x3f(0)
  final val Identity = ConstMat2x3f(1)

  final val Tag = classTag[Mat2x3f]
  final val ConstTag = classTag[ConstMat2x3f]
  final val ReadTag = classTag[ReadMat2x3f]

  def apply(s: Float) = new Mat2x3f(
    s, 0, 0,
    0, s, 0
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float, m02: Float,
    m10: Float, m11: Float, m12: Float
  ) = new Mat2x3f(
    m00, m01, m02,
    m10, m11, m12
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_]) = 
  new Mat2x3f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: AnyMat[_]) = new Mat2x3f(
    m.f00, m.f01, m.f02,
    m.f10, m.f11, m.f12
  )

  def unapply(m: ReadMat2x3f) = Some((m(0), m(1)))
}
