/*
 * Simplex3d, FloatMath module
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
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat3x2f extends ProtectedMat3x2f[Float]
with ReadPropertyRef[ReadMat3x2f] with Serializable
{

  type Clone <: ReadMat3x2f
  type Const = ConstMat3x2f
  type Mutable = Mat3x2f
  def toConst() :ConstMat3x2f
  final def toMutable() = Mat3x2f(this)

  // Column major order.
  final def m00 = p00; final def m10 = p10; final def m20 = p20
  final def m01 = p01; final def m11 = p11; final def m21 = p21


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m20_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }
  protected def m21_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f10 = m10
  private[math] final override def f20 = m20

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11
  private[math] final override def f21 = m21


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21


  final def apply(c: Int) :ConstVec3f = {
    c match {
      case 0 => new ConstVec3f(m00, m10, m20)
      case 1 => new ConstVec3f(m01, m11, m21)
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 1, got " + j + "."
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

  final def unary_+() :ReadMat3x2f = this
  final def unary_-() = new Mat3x2f(
    -m00, -m10, -m20,
    -m01, -m11, -m21
  )
  final def *(s: Float) = new Mat3x2f(
    s*m00, s*m10, s*m20,
    s*m01, s*m11, s*m21
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat3x2f(
    m00 + s, m10 + s, m20 + s,
    m01 + s, m11 + s, m21 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat3x2f) = new Mat3x2f(
    m00 + m.m00, m10 + m.m10, m20 + m.m20,
    m01 + m.m01, m11 + m.m11, m21 + m.m21
  )
  final def -(m: inMat3x2f) = new Mat3x2f(
    m00 - m.m00, m10 - m.m10, m20 - m.m20,
    m01 - m.m01, m11 - m.m11, m21 - m.m21
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat3x2f) = new Mat3x2f(
    m00/m.m00, m10/m.m10, m20/m.m20,
    m01/m.m01, m11/m.m11, m21/m.m21
  )
  private[math] final def divByComp(s: Float) = new Mat3x2f(
    s/m00, s/m10, s/m20,
    s/m01, s/m11, s/m21
  )

  final def *(m: inMat2f) = new Mat3x2f(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11
  )
  final def *(m: inMat2x3f) = new Mat3f(
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
  final def *(m: inMat2x4f) = new Mat3x4f(
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

  final def *(u: inVec2f) = new Vec3f(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y
  )
  private[math] final def transposeMult(u: inVec3f) = new Vec2f(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat3x2[_] =>
        d00 == m.d00 && d10 == m.d10 && d20 == m.d20 &&
        d01 == m.d01 && d11 == m.d11 && d21 == m.d21
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
              41 + m00.hashCode
            ) + m10.hashCode
          ) + m20.hashCode
        ) + m01.hashCode
      ) + m11.hashCode
    ) + m21.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat3x2" +
    "(" +
      m00 + "f, " + m10 + "f, " + m20 + "f,   " + 
      m01 + "f, " + m11 + "f, " + m21 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3x2f private[math] (
  c00: Float, c10: Float, c20: Float,
  c01: Float, c11: Float, c21: Float
) extends ReadMat3x2f with Immutable with Serializable
{
  p00 = c00; p10 = c10; p20 = c20
  p01 = c01; p11 = c11; p21 = c21

  type Clone = ConstMat3x2f
  override def clone() = this
  def toConst() = this
}

object ConstMat3x2f {
  def apply(s: Float) = new ConstMat3x2f(
    s, 0, 0,
    0, s, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float
  ) = new ConstMat3x2f(
    m00, m10, m20,
    m01, m11, m21
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_]) = 
  new ConstMat3x2f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: AnyMat[_]) = new ConstMat3x2f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21
  )

  implicit def toConst(m: ReadMat3x2f) = ConstMat3x2f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3x2f private[math] (
  c00: Float, c10: Float, c20: Float,
  c01: Float, c11: Float, c21: Float
)
extends ReadMat3x2f with Meta with CompositeFormat
with PropertyRef[ReadMat3x2f] with Serializable
{
  p00 = c00; p10 = c10; p20 = c20
  p01 = c01; p11 = c11; p21 = c21

  type Read = ReadMat3x2f

  type Meta = Mat3x2f
  type Component = RFloat

  type Clone = Mat3x2f
  override def clone() = Mat3x2f(this)
  def toConst() = ConstMat3x2f(this)
  def :=(u: ConstMat3x2f) { this := u.asInstanceOf[inMat3x2f] }
  
  def :=(m: inMat3x2f) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20;
    m01 = m.m01; m11 = m.m11; m21 = m.m21
  }

  
  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }
  override def m20_=(s: Float) { p20 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }
  override def m21_=(s: Float) { p21 = s }


  def *=(s: Float) {
    m00 *= s; m10 *= s; m20 *= s;
    m01 *= s; m11 *= s; m21 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s; m20 += s
    m01 += s; m11 += s; m21 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat3x2f) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20;
    m01 += m.m01; m11 += m.m11; m21 += m.m21
  }
  def -=(m: inMat3x2f) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21
  }

  def *=(m: inMat2f) {
    val t00 = m00*m.m00 + m01*m.m10
    val t10 = m10*m.m00 + m11*m.m10
    val t20 = m20*m.m00 + m21*m.m10

    val t01 = m00*m.m01 + m01*m.m11
    val t11 = m10*m.m01 + m11*m.m11
        m21 = m20*m.m01 + m21*m.m11

    m00 = t00; m10 = t10; m20 = t20
    m01 = t01; m11 = t11
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat3x2f) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21
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
  final val Zero = ConstMat3x2f(0)
  final val Identity = ConstMat3x2f(1)

  final val Manifest = classType[Mat3x2f](classOf[Mat3x2f])
  final val ConstManifest = classType[ConstMat3x2f](classOf[ConstMat3x2f])
  final val ReadManifest = classType[ReadMat3x2f](classOf[ReadMat3x2f])

  def apply(s: Float) = new Mat3x2f(
    s, 0, 0,
    0, s, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float
  ) = new Mat3x2f(
    m00, m10, m20,
    m01, m11, m21
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_]) = 
  new Mat3x2f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz
  )

  def apply(m: AnyMat[_]) = new Mat3x2f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21
  )

  def unapply(m: ReadMat3x2f) = Some((m(0), m(1)))
}
