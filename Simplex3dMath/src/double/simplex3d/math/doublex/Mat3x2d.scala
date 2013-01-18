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
sealed abstract class ReadMat3x2d extends ProtectedMat3x2d[Double]
with Protected with Serializable
{

  type Clone <: ReadMat3x2d
  def toConst() :ConstMat3x2d
  
  type Read = ReadMat3x2d
  type Mutable = Mat3x2d
  final def mutableCopy() = Mat3x2d(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01
  final def m10 = p10; final def m11 = p11
  final def m20 = p20; final def m21 = p21


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m01_=(s: Double) { throw new UnsupportedOperationException }

  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }

  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f01 = m01.toFloat

  private[math] final override def f10 = m10.toFloat
  private[math] final override def f11 = m11.toFloat

  private[math] final override def f20 = m20.toFloat
  private[math] final override def f21 = m21.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m01)
      case 1 => new ConstVec2d(m10, m11)
      case 2 => new ConstVec2d(m20, m21)
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
      case 2 =>
        r match {
          case 0 => m20
          case 1 => m21
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat3x2d = this
  final def unary_-() = new Mat3x2d(
    -m00, -m01,
    -m10, -m11,
    -m20, -m21
  )
  final def *(s: Double) = new Mat3x2d(
    s*m00, s*m01,
    s*m10, s*m11,
    s*m20, s*m21
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat3x2d(
    m00 + s, m01 + s,
    m10 + s, m11 + s,
    m20 + s, m21 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat3x2d) = new Mat3x2d(
    m00 + m.m00, m01 + m.m01,
    m10 + m.m10, m11 + m.m11,
    m20 + m.m20, m21 + m.m21
  )
  final def -(m: inMat3x2d) = new Mat3x2d(
    m00 - m.m00, m01 - m.m01,
    m10 - m.m10, m11 - m.m11,
    m20 - m.m20, m21 - m.m21
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat3x2d) = new Mat3x2d(
    m00/m.m00, m01/m.m01,
    m10/m.m10, m11/m.m11,
    m20/m.m20, m21/m.m21
  )
  private[math] final def divByComp(s: Double) = new Mat3x2d(
    s/m00, s/m01,
    s/m10, s/m11,
    s/m20, s/m21
  )

  final def *(m: inMat2x3d) = new Mat2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12
  )
  final def *(m: inMat3d) = new Mat3x2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22
  )
  final def *(m: inMat4x3d) = new Mat4x2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,

    m00*m.m30 + m10*m.m31 + m20*m.m32,
    m01*m.m30 + m11*m.m31 + m21*m.m32
  )

  final def *(u: inVec3d) = new Vec2d(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z
  )
  private[math] final def transposeMult(u: inVec2d) = new Vec3d(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y
  )

  final def scale(s: Double) :Mat3x2d = this*s
  final def scale(s: inVec2d) :Mat3x2d = new Mat3x2d(
    m00*s.x, m01*s.y,
    m10*s.x, m11*s.y,
    m20*s.x, m21*s.y
  )

  final def rotate(angle: Double) :Mat3x2d = {
    val cosA = cos(angle)
    val sinA = sin(angle)
  
    new Mat3x2d(
      cosA*m00 - sinA*m01, sinA*m00 + cosA*m01,
      cosA*m10 - sinA*m11, sinA*m10 + cosA*m11,
      cosA*m20 - sinA*m21, sinA*m20 + cosA*m21
    )
  }

  final def translate(u: inVec2d) :Mat3x2d = new Mat3x2d(
    m00, m01,
    m10, m11,
    m20 + u.x, m21 + u.y
  )

  final def concat(m: inMat3x2d) :Mat3x2d = new Mat3x2d(
    m.m00*m00 + m.m10*m01,
    m.m01*m00 + m.m11*m01,

    m.m00*m10 + m.m10*m11,
    m.m01*m10 + m.m11*m11,

    m.m00*m20 + m.m10*m21 + m.m20,
    m.m01*m20 + m.m11*m21 + m.m21
  )
  final def concat(m: inMat2d) :Mat3x2d = m*this

  final def transformPoint(p: inVec2d) :Vec2d = new Vec2d(
    m00*p.x + m10*p.y + m20,
    m01*p.x + m11*p.y + m21
  )
  final def transformVector(v: inVec2d) :Vec2d = new Vec2d(
    m00*v.x + m10*v.y,
    m01*v.x + m11*v.y
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat3x2[_] =>
        d00 == m.d00 && d01 == m.d01 &&
        d10 == m.d10 && d11 == m.d11 &&
        d20 == m.d20 && d21 == m.d21
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
              41 + simplex3d.math.doubleHashCode(m00)
            ) + simplex3d.math.doubleHashCode(m01)
          ) + simplex3d.math.doubleHashCode(m10)
        ) + simplex3d.math.doubleHashCode(m11)
      ) + simplex3d.math.doubleHashCode(m20)
    ) + simplex3d.math.doubleHashCode(m21)
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat3x2" +
    "(" +
      m00 + ", " + m01 + ",   " + 
      m10 + ", " + m11 + ",   " + 
      m20 + ", " + m21 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3x2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double,
  c20: Double, c21: Double
) extends ReadMat3x2d with Immutable with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21


  type Clone = ConstMat3x2d
  override def clone() = this
  def toConst() = this
}

object ConstMat3x2d {
  def apply(s: Double) = new ConstMat3x2d(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double,
    m20: Double, m21: Double
  ) = new ConstMat3x2d(
    m00, m01,
    m10, m11,
    m20, m21
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new ConstMat3x2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy
  )

  def apply(m: AnyMat[_]) = new ConstMat3x2d(
    m.d00, m.d01,
    m.d10, m.d11,
    m.d20, m.d21
  )

  implicit def toConst(m: ReadMat3x2d) = ConstMat3x2d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3x2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double,
  c20: Double, c21: Double
)
extends ReadMat3x2d with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21

  private[math] def this() = this(
    1, 0,
    0, 1,
    0, 0
  )
  
  type Clone = Mat3x2d
  type Const = ConstMat3x2d

  type Accessor = Mat3x2d
  type Component = RDouble

  override def clone() = Mat3x2d(this)
  def toConst() = ConstMat3x2d(this)

  def :=(m: inMat3x2d) {
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
    m20 = m.m20; m21 = m.m21
  }

  def :=(m: inMat2d) {
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
  }


  override def m00_=(s: Double) { p00 = s }
  override def m01_=(s: Double) { p01 = s }

  override def m10_=(s: Double) { p10 = s }
  override def m11_=(s: Double) { p11 = s }

  override def m20_=(s: Double) { p20 = s }
  override def m21_=(s: Double) { p21 = s }


  def *=(s: Double) {
    m00 *= s; m01 *= s;
    m10 *= s; m11 *= s;
    m20 *= s; m21 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m01 += s
    m10 += s; m11 += s
    m20 += s; m21 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat3x2d) {
    m00 += m.m00; m01 += m.m01;
    m10 += m.m10; m11 += m.m11;
    m20 += m.m20; m21 += m.m21
  }
  def -=(m: inMat3x2d) {
    m00 -= m.m00; m01 -= m.m01;
    m10 -= m.m10; m11 -= m.m11;
    m20 -= m.m20; m21 -= m.m21
  }

  def *=(m: inMat3d) {
    val t00 = m00*m.m00 + m10*m.m01 + m20*m.m02
    val t01 = m01*m.m00 + m11*m.m01 + m21*m.m02

    val t10 = m00*m.m10 + m10*m.m11 + m20*m.m12
    val t11 = m01*m.m10 + m11*m.m11 + m21*m.m12

    val t20 = m00*m.m20 + m10*m.m21 + m20*m.m22
        m21 = m01*m.m20 + m11*m.m21 + m21*m.m22

    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat3x2d) {
    m00 /= m.m00; m01 /= m.m01
    m10 /= m.m10; m11 /= m.m11
    m20 /= m.m20; m21 /= m.m21
  }

  final def applyScale(s: Double) { this *= s }
  final def applyScale(s: inVec2d) {
    m00 *= s.x; m01 *= s.y
    m10 *= s.x; m11 *= s.y
    m20 *= s.x; m21 *= s.y
  }

  final def applyRotation(angle: Double) {
    val cosA = cos(angle)
    val sinA = sin(angle)
  
    val t00 = cosA*m00 - sinA*m01
    val t01 = sinA*m00 + cosA*m01

    val t10 = cosA*m10 - sinA*m11
    val t11 = sinA*m10 + cosA*m11

    val t20 = cosA*m20 - sinA*m21
        m21 = sinA*m20 + cosA*m21
    
    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20
  }

  final def applyTranslation(u: inVec2d) {
    m20 += u.x; m21 += u.y
  }

  final def applyTransformation(m: inMat3x2d) {
    val t00 = m.m00*m00 + m.m10*m01
    val t01 = m.m01*m00 + m.m11*m01

    val t10 = m.m00*m10 + m.m10*m11
    val t11 = m.m01*m10 + m.m11*m11

    val t20 = m.m00*m20 + m.m10*m21 + m.m20
        m21 = m.m01*m20 + m.m11*m21 + m.m21
    
    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20
  }
  final def applyTransformation(m: inMat2d) {
    val t00 = m.m00*m00 + m.m10*m01
    val t01 = m.m01*m00 + m.m11*m01

    val t10 = m.m00*m10 + m.m10*m11
    val t11 = m.m01*m10 + m.m11*m11

    val t20 = m.m00*m20 + m.m10*m21
        m21 = m.m01*m20 + m.m11*m21
    
    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20
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
      case 2 =>
        r match {
          case 0 => m20 = s
          case 1 => m21 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2d) {
    c match {
      case 0 => m00 = v.x; m01 = v.y
      case 1 => m10 = v.x; m11 = v.y
      case 2 => m20 = v.x; m21 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat3x2d {
  final val Zero = ConstMat3x2d(0)
  final val Identity = ConstMat3x2d(1)

  final val Tag = classTag[Mat3x2d]
  final val ConstTag = classTag[ConstMat3x2d]
  final val ReadTag = classTag[ReadMat3x2d]

  def apply(s: Double) = new Mat3x2d(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double,
    m20: Double, m21: Double
  ) = new Mat3x2d(
    m00, m01,
    m10, m11,
    m20, m21
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new Mat3x2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy
  )

  def apply(m: AnyMat[_]) = new Mat3x2d(
    m.d00, m.d01,
    m.d10, m.d11,
    m.d20, m.d21
  )

  def unapply(m: ReadMat3x2d) = Some((m(0), m(1), m(2)))

  def scale(s: Double) :Mat3x2d = Mat3x2d(s)
  def scale(s: inVec2d) :Mat3x2d = {
    val m = Mat3x2d(s.x)
    m.m11 = s.y
    m
  }

  def rotate(angle: Double) :Mat3x2d = {
    val cosA = cos(angle)
    val sinA = sin(angle)

    new Mat3x2d(
       cosA, sinA,
      -sinA, cosA,
       0, 0
    )
  }

  def translate(u: inVec2d) :Mat3x2d = {
    val m = Mat3x2d(1)
    m(2) = u
    m
  }

  def concat(m: inMat3x2d) :Mat3x2d = Mat3x2d(m)
  def concat(m: inMat2d) :Mat3x2d = Mat3x2d(m)
}
