/*
 * Simplex3dMath - Double Module
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
package doublex

import scala.language.implicitConversions
import scala.reflect._
import simplex3d.math.integration._
import simplex3d.math.types._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat2d extends ProtectedMat2d[Double]
with Protected with Serializable
{

  type Clone <: ReadMat2d
  def toConst :ConstMat2d
  
  type Read = ReadMat2d
  type Mutable = Mat2d
  final def mutableCopy = Mat2d(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01
  final def m10 = p10; final def m11 = p11


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m01_=(s: Double) { throw new UnsupportedOperationException }

  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f01 = m01.toFloat

  private[math] final override def f10 = m10.toFloat
  private[math] final override def f11 = m11.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m01)
      case 1 => new ConstVec2d(m10, m11)
      case j => throw new IndexOutOfBoundsException(
          "Trying to read column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  final def apply(c: Int, r: Int) :Double = {
    def error() :Double = throw new IndexOutOfBoundsException(
      "Trying to read index (" + c + ", " + r + ") in " + this.getClass.getSimpleName + "."
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00
          case 1 => m01
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10
          case 1 => m11
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat2d = this
  final def unary_-() = new Mat2d(
    -m00, -m01,
    -m10, -m11
  )
  final def *(s: Double) = new Mat2d(
    s*m00, s*m01,
    s*m10, s*m11
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat2d(
    m00 + s, m01 + s,
    m10 + s, m11 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat2d) = new Mat2d(
    m00 + m.m00, m01 + m.m01,
    m10 + m.m10, m11 + m.m11
  )
  final def -(m: inMat2d) = new Mat2d(
    m00 - m.m00, m01 - m.m01,
    m10 - m.m10, m11 - m.m11
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat2d) = new Mat2d(
    m00/m.m00, m01/m.m01,
    m10/m.m10, m11/m.m11
  )
  private[math] final def divByComp(s: Double) = new Mat2d(
    s/m00, s/m01,
    s/m10, s/m11
  )

  final def *(m: inMat2d) = new Mat2d(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11
  )
  final def *(m: inMat3x2d) = new Mat3x2d(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21
  )
  final def *(m: inMat4x2d) = new Mat4x2d(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21,

    m00*m.m30 + m10*m.m31,
    m01*m.m30 + m11*m.m31
  )

  final def *(u: inVec2d) = new Vec2d(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y
  )
  private[math] final def transposeMult(u: inVec2d) = new Vec2d(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2[_] =>
        d00 == m.d00 && d01 == m.d01 &&
        d10 == m.d10 && d11 == m.d11
      case _ =>
        false
    }
  }

  final override def hashCode :Int = {
    41 * (
      41 * (
        41 * (
          41 + simplex3d.math.doubleHashCode(m00)
        ) + simplex3d.math.doubleHashCode(m01)
      ) + simplex3d.math.doubleHashCode(m10)
    ) + simplex3d.math.doubleHashCode(m11)
  }

  final override def toString :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2" +
    "(" +
      m00 + ", " + m01 + ",   " + 
      m10 + ", " + m11 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double
) extends ReadMat2d with Immutable with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11


  type Clone = ConstMat2d
  override def clone = this
  def toConst = this
}

object ConstMat2d {
  def apply(s: Double) = new ConstMat2d(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double
  ) = new ConstMat2d(
    m00, m01,
    m10, m11
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_]) = 
  new ConstMat2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy
  )

  def apply(u: AnyVec4[_]) = new ConstMat2d(
    u.dx, u.dy,
    u.dz, u.dw
  )

  def apply(m: AnyMat[_]) = new ConstMat2d(
    m.d00, m.d01,
    m.d10, m.d11
  )

  implicit def toConst(m: ReadMat2d) = ConstMat2d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double
)
extends ReadMat2d with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11

  private[math] def this() = this(
    1, 0,
    0, 1
  )
  
  type Clone = Mat2d
  type Const = ConstMat2d

  type Accessor = Mat2d
  type Component = RDouble

  override def clone = Mat2d(this)
  def toConst = ConstMat2d(this)

  def :=(m: inMat2d) {
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
  }


  override def m00_=(s: Double) { p00 = s }
  override def m01_=(s: Double) { p01 = s }

  override def m10_=(s: Double) { p10 = s }
  override def m11_=(s: Double) { p11 = s }


  def *=(s: Double) {
    m00 *= s; m01 *= s;
    m10 *= s; m11 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m01 += s
    m10 += s; m11 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat2d) {
    m00 += m.m00; m01 += m.m01;
    m10 += m.m10; m11 += m.m11
  }
  def -=(m: inMat2d) {
    m00 -= m.m00; m01 -= m.m01;
    m10 -= m.m10; m11 -= m.m11
  }

  def *=(m: inMat2d) {
    val t00 = m00*m.m00 + m10*m.m01
    val t01 = m01*m.m00 + m11*m.m01

    val t10 = m00*m.m10 + m10*m.m11
        m11 = m01*m.m10 + m11*m.m11

    m00 = t00; m01 = t01
    m10 = t10
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat2d) {
    m00 /= m.m00; m01 /= m.m01
    m10 /= m.m10; m11 /= m.m11
  }


  def update(c: Int, r: Int, s: Double) {
    def error() = throw new IndexOutOfBoundsException(
      "Trying to update index (" + c + ", " + r + ") in " + this.getClass.getSimpleName + "."
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00 = s
          case 1 => m01 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10 = s
          case 1 => m11 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2d) {
    c match {
      case 0 => m00 = v.x; m01 = v.y
      case 1 => m10 = v.x; m11 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat2d {
  final val Zero = ConstMat2d(0)
  final val Identity = ConstMat2d(1)

  final val Tag = classTag[Mat2d]
  final val ConstTag = classTag[ConstMat2d]
  final val ReadTag = classTag[ReadMat2d]

  def apply(s: Double) = new Mat2d(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double
  ) = new Mat2d(
    m00, m01,
    m10, m11
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_]) = 
  new Mat2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy
  )

  def apply(u: AnyVec4[_]) = new Mat2d(
    u.dx, u.dy,
    u.dz, u.dw
  )

  def apply(m: AnyMat[_]) = new Mat2d(
    m.d00, m.d01,
    m.d10, m.d11
  )

  def unapply(m: ReadMat2d) = Some((m(0), m(1)))
}
