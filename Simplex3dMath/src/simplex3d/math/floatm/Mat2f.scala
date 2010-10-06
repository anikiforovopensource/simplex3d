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
sealed abstract class ReadMat2f
extends ProtectedMat2f[Float]
{
  // Column major order.
  final def m00= p00; final def m10= p10
  final def m01= p01; final def m11= p11


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f10 = m10

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11


  final def apply(c: Int) :ConstVec2f = {
    c match {
      case 0 => new ConstVec2f(m00, m10)
      case 1 => new ConstVec2f(m01, m11)
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat2f = this
  final def unary_-() = new Mat2f(
    -m00, -m10,
    -m01, -m11
  )
  final def *(s: Float) = new Mat2f(
    s*m00, s*m10,
    s*m01, s*m11
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat2f(
    m00 + s, m10 + s,
    m01 + s, m11 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat2f) = new Mat2f(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11
  )
  final def -(m: inMat2f) = new Mat2f(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2f) = new Mat2f(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11
  )
  private[math] final def divideByComponent(s: Float) = new Mat2f(
    s/m00, s/m10,
    s/m01, s/m11
  )

  final def *(m: inMat2f) = new Mat2f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11
  )
  final def *(m: inMat2x3f) = new Mat2x3f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12
  )
  final def *(m: inMat2x4f) = new Mat2x4f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,

    m00*m.m03 + m01*m.m13,
    m10*m.m03 + m11*m.m13
  )

  final def *(u: inVec2f) = new Vec2f(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y
  )
  private[math] final def transposeMul(u: inVec2f) = new Vec2f(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y
  )


  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2x2[_] =>
        d00 == m.d00 && d10 == m.d10 &&
        d01 == m.d01 && d11 == m.d11
      case _ =>
        false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + m00.hashCode
        ) + m10.hashCode
      ) + m01.hashCode
    ) + m11.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + "; " + 
      m01 + ", " + m11 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat2f private[math] (
  c00: Float, c10: Float,
  c01: Float, c11: Float
) extends ReadMat2f with Immutable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11

  override def clone() = this
}

object ConstMat2f {
  def apply(s: Float) = new ConstMat2f(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float
  ) = new ConstMat2f(
    m00, m10,
    m01, m11
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_]) = 
  new ConstMat2f(
    c0.fx, c0.fy,
    c1.fx, c1.fy
  )

  def apply(u: AnyVec4[_]) = new ConstMat2f(
    u.fx, u.fy,
    u.fz, u.fw
  )

  def apply(m: AnyMat[_]) = new ConstMat2f(
    m.f00, m.f10,
    m.f01, m.f11
  )

  implicit def toConst(m: ReadMat2f) = ConstMat2f(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2f private[math] (
  c00: Float, c10: Float,
  c01: Float, c11: Float
) extends ReadMat2f with Implicits[On] with Composite
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11

  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }

  type Element = ReadMat2f
  type Immutable = ConstMat2f
  type Component = Float1

  def *=(s: Float) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s
    m01 += s; m11 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat2f) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11
  }
  def -=(m: inMat2f) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11
  }

  def *=(m: inMat2f) {
    val a00 = m00*m.m00 + m01*m.m10
    val a10 = m10*m.m00 + m11*m.m10

    val a01 = m00*m.m01 + m01*m.m11
    val a11 = m10*m.m01 + m11*m.m11

    m00 = a00; m10 = a10
    m01 = a01; m11 = a11
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat2f) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
  }

  override def clone() = Mat2f(this)
  
  def :=(m: inMat2f) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11
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
}

object Mat2f {
  final val Zero = ConstMat2f(0)
  final val Identity = ConstMat2f(1)
  final val Manifest = classType[ReadMat2f](classOf[ReadMat2f])

  def apply(s: Float) = new Mat2f(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float
  ) = new Mat2f(
    m00, m10,
    m01, m11
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_]) = 
  new Mat2f(
    c0.fx, c0.fy,
    c1.fx, c1.fy
  )

  def apply(u: AnyVec4[_]) = new Mat2f(
    u.fx, u.fy,
    u.fz, u.fw
  )

  def apply(m: AnyMat[_]) = new Mat2f(
    m.f00, m.f10,
    m.f01, m.f11
  )

  def unapply(m: ReadMat2f) = Some((m(0), m(1)))

  implicit def toMutable(m: ReadMat2f) = Mat2f(m)
}
