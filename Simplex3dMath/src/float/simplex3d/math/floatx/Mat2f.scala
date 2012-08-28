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
                      
import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.types._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat2f extends ProtectedMat2f[Float]
with Protected with Serializable
{

  type Clone <: ReadMat2f
  def toConst() :ConstMat2f
  
  type Read = ReadMat2f
  type Mutable = Mat2f
  final def readType: Class[Read] = classOf[ReadMat2f]
  final def mutableCopy() = Mat2f(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01
  final def m10 = p10; final def m11 = p11


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m01_=(s: Float) { throw new UnsupportedOperationException }

  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f01 = m01

  private[math] final override def f10 = m10
  private[math] final override def f11 = m11


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11


  final def apply(c: Int) :ConstVec2f = {
    c match {
      case 0 => new ConstVec2f(m00, m01)
      case 1 => new ConstVec2f(m10, m11)
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

  final def unary_+() :ReadMat2f = this
  final def unary_-() = new Mat2f(
    -m00, -m01,
    -m10, -m11
  )
  final def *(s: Float) = new Mat2f(
    s*m00, s*m01,
    s*m10, s*m11
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat2f(
    m00 + s, m01 + s,
    m10 + s, m11 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat2f) = new Mat2f(
    m00 + m.m00, m01 + m.m01,
    m10 + m.m10, m11 + m.m11
  )
  final def -(m: inMat2f) = new Mat2f(
    m00 - m.m00, m01 - m.m01,
    m10 - m.m10, m11 - m.m11
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat2f) = new Mat2f(
    m00/m.m00, m01/m.m01,
    m10/m.m10, m11/m.m11
  )
  private[math] final def divByComp(s: Float) = new Mat2f(
    s/m00, s/m01,
    s/m10, s/m11
  )

  final def *(m: inMat2f) = new Mat2f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11
  )
  final def *(m: inMat3x2f) = new Mat3x2f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21
  )
  final def *(m: inMat4x2f) = new Mat4x2f(
    m00*m.m00 + m10*m.m01,
    m01*m.m00 + m11*m.m01,

    m00*m.m10 + m10*m.m11,
    m01*m.m10 + m11*m.m11,

    m00*m.m20 + m10*m.m21,
    m01*m.m20 + m11*m.m21,

    m00*m.m30 + m10*m.m31,
    m01*m.m30 + m11*m.m31
  )

  final def *(u: inVec2f) = new Vec2f(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y
  )
  private[math] final def transposeMult(u: inVec2f) = new Vec2f(
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

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + m00.hashCode
        ) + m01.hashCode
      ) + m10.hashCode
    ) + m11.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2" +
    "(" +
      m00 + "f, " + m01 + "f,   " + 
      m10 + "f, " + m11 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat2f private[math] (
  c00: Float, c01: Float,
  c10: Float, c11: Float
) extends ReadMat2f with Immutable with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11


  type Clone = ConstMat2f
  override def clone() = this
  def toConst() = this
}

object ConstMat2f {
  def apply(s: Float) = new ConstMat2f(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float,
    m10: Float, m11: Float
  ) = new ConstMat2f(
    m00, m01,
    m10, m11
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
    m.f00, m.f01,
    m.f10, m.f11
  )

  implicit def toConst(m: ReadMat2f) = ConstMat2f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat2f private[math] (
  c00: Float, c01: Float,
  c10: Float, c11: Float
)
extends ReadMat2f with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11

  private[math] def this() = this(
    1, 0,
    0, 1
  )
  
  type Clone = Mat2f
  type Const = ConstMat2f

  type Accessor = Mat2f
  type Component = RFloat

  override def clone() = Mat2f(this)
  def toConst() = ConstMat2f(this)
  def :=(m: inMat2f) {
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
  }

  
  override def m00_=(s: Float) { p00 = s }
  override def m01_=(s: Float) { p01 = s }

  override def m10_=(s: Float) { p10 = s }
  override def m11_=(s: Float) { p11 = s }


  def *=(s: Float) {
    m00 *= s; m01 *= s;
    m10 *= s; m11 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m01 += s
    m10 += s; m11 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat2f) {
    m00 += m.m00; m01 += m.m01;
    m10 += m.m10; m11 += m.m11
  }
  def -=(m: inMat2f) {
    m00 -= m.m00; m01 -= m.m01;
    m10 -= m.m10; m11 -= m.m11
  }

  def *=(m: inMat2f) {
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
  def /=(m: inMat2f) {
    m00 /= m.m00; m01 /= m.m01
    m10 /= m.m10; m11 /= m.m11
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

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m01 = v.y
      case 1 => m10 = v.x; m11 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat2f {
  final val Zero = ConstMat2f(0)
  final val Identity = ConstMat2f(1)

  final val Manifest = classType[Mat2f](classOf[Mat2f])
  final val ConstManifest = classType[ConstMat2f](classOf[ConstMat2f])
  final val ReadManifest = classType[ReadMat2f](classOf[ReadMat2f])

  def apply(s: Float) = new Mat2f(
    s, 0,
    0, s
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float,
    m10: Float, m11: Float
  ) = new Mat2f(
    m00, m01,
    m10, m11
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
    m.f00, m.f01,
    m.f10, m.f11
  )

  def unapply(m: ReadMat2f) = Some((m(0), m(1)))
}
