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
sealed abstract class ReadMat3x2f extends ProtectedMat3x2f[Float]
with ReadPropertyValue[Mat3x2f] with Cloneable[ReadMat3x2f] with Serializable
{

  def toConst() :ConstMat3x2f
  final def mutableCopy() = Mat3x2f(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01
  final def m10 = p10; final def m11 = p11
  final def m20 = p20; final def m21 = p21


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m01_=(s: Float) { throw new UnsupportedOperationException }

  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }

  protected def m20_=(s: Float) { throw new UnsupportedOperationException }
  protected def m21_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f01 = m01

  private[math] final override def f10 = m10
  private[math] final override def f11 = m11

  private[math] final override def f20 = m20
  private[math] final override def f21 = m21


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21


  final def apply(c: Int) :ConstVec2f = {
    c match {
      case 0 => new ConstVec2f(m00, m01)
      case 1 => new ConstVec2f(m10, m11)
      case 2 => new ConstVec2f(m20, m21)
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
      case 2 =>
        r match {
          case 0 => m20
          case 1 => m21
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat3x2f = this
  final def unary_-() = new Mat3x2f(
    -m00, -m01,
    -m10, -m11,
    -m20, -m21
  )
  final def *(s: Float) = new Mat3x2f(
    s*m00, s*m01,
    s*m10, s*m11,
    s*m20, s*m21
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat3x2f(
    m00 + s, m01 + s,
    m10 + s, m11 + s,
    m20 + s, m21 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat3x2f) = new Mat3x2f(
    m00 + m.m00, m01 + m.m01,
    m10 + m.m10, m11 + m.m11,
    m20 + m.m20, m21 + m.m21
  )
  final def -(m: inMat3x2f) = new Mat3x2f(
    m00 - m.m00, m01 - m.m01,
    m10 - m.m10, m11 - m.m11,
    m20 - m.m20, m21 - m.m21
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat3x2f) = new Mat3x2f(
    m00/m.m00, m01/m.m01,
    m10/m.m10, m11/m.m11,
    m20/m.m20, m21/m.m21
  )
  private[math] final def divByComp(s: Float) = new Mat3x2f(
    s/m00, s/m01,
    s/m10, s/m11,
    s/m20, s/m21
  )

  final def *(m: inMat2x3f) = new Mat2f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12
  )
  final def *(m: inMat3f) = new Mat3x2f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22
  )
  final def *(m: inMat4x3f) = new Mat4x2f(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,

    m00*m.m30 + m10*m.m31 + m20*m.m32,
    m01*m.m30 + m11*m.m31 + m21*m.m32
  )

  final def *(u: inVec3f) = new Vec2f(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z
  )
  private[math] final def transposeMult(u: inVec2f) = new Vec3f(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y
  )

  final def scale(s: Float) :Mat3x2f = this*s
  final def scale(s: inVec2f) :Mat3x2f = new Mat3x2f(
    m00*s.x, m01*s.y,
    m10*s.x, m11*s.y,
    m20*s.x, m21*s.y
  )

  final def rotate(angle: Float) :Mat3x2f = {
    val cosA = cos(angle)
    val sinA = sin(angle)
  
    new Mat3x2f(
      cosA*m00 - sinA*m01, sinA*m00 + cosA*m01,
      cosA*m10 - sinA*m11, sinA*m10 + cosA*m11,
      cosA*m20 - sinA*m21, sinA*m20 + cosA*m21
    )
  }

  final def translate(u: inVec2f) :Mat3x2f = new Mat3x2f(
    m00, m01,
    m10, m11,
    m20 + u.x, m21 + u.y
  )

  final def concat(m: inMat3x2f) :Mat3x2f = new Mat3x2f(
    m.m00*m00 + m.m10*m01,
    m.m01*m00 + m.m11*m01,

    m.m00*m10 + m.m10*m11,
    m.m01*m10 + m.m11*m11,

    m.m00*m20 + m.m10*m21 + m.m20,
    m.m01*m20 + m.m11*m21 + m.m21
  )
  final def concat(m: inMat2f) :Mat3x2f = m*this

  final def transformPoint(p: inVec2f) :Vec2f = new Vec2f(
    m00*p.x + m10*p.y + m20,
    m01*p.x + m11*p.y + m21
  )
  final def transformVector(v: inVec2f) :Vec2f = new Vec2f(
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
              41 + m00.hashCode
            ) + m01.hashCode
          ) + m10.hashCode
        ) + m11.hashCode
      ) + m20.hashCode
    ) + m21.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2x3" +
    "(" +
      m00 + "f, " + m01 + "f,   " + 
      m10 + "f, " + m11 + "f,   " + 
      m20 + "f, " + m21 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3x2f private[math] (
  c00: Float, c01: Float,
  c10: Float, c11: Float,
  c20: Float, c21: Float
) extends ReadMat3x2f with Immutable with Cloneable[ConstMat3x2f] with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21

  override def clone() = this
  def toConst() = this
}

object ConstMat3x2f {
  def apply(s: Float) = new ConstMat3x2f(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float,
    m10: Float, m11: Float,
    m20: Float, m21: Float
  ) = new ConstMat3x2f(
    m00, m01,
    m10, m11,
    m20, m21
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new ConstMat3x2f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy
  )

  def apply(m: AnyMat[_]) = new ConstMat3x2f(
    m.f00, m.f01,
    m.f10, m.f11,
    m.f20, m.f21
  )

  implicit def toConst(m: ReadMat3x2f) = ConstMat3x2f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3x2f private[math] (
  c00: Float, c01: Float,
  c10: Float, c11: Float,
  c20: Float, c21: Float
)
extends ReadMat3x2f with Accessor with CompositeFormat
with PropertyValue[Mat3x2f] with Cloneable[Mat3x2f] with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21

  type Read = ReadMat3x2f
  type Const = ConstMat3x2f

  type Accessor = Mat3x2f
  type Component = RFloat

  override def clone() = Mat3x2f(this)
  def toConst() = ConstMat3x2f(this)
  def :=(u: ConstMat3x2f) { this := u.asInstanceOf[inMat3x2f] }
  
  def :=(r: Readable[Mat3x2f]) {
    val m = r.asInstanceOf[ReadMat3x2f]
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
    m20 = m.m20; m21 = m.m21
  }

  
  override def m00_=(s: Float) { p00 = s }
  override def m01_=(s: Float) { p01 = s }

  override def m10_=(s: Float) { p10 = s }
  override def m11_=(s: Float) { p11 = s }

  override def m20_=(s: Float) { p20 = s }
  override def m21_=(s: Float) { p21 = s }


  def *=(s: Float) {
    m00 *= s; m01 *= s;
    m10 *= s; m11 *= s;
    m20 *= s; m21 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m01 += s
    m10 += s; m11 += s
    m20 += s; m21 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat3x2f) {
    m00 += m.m00; m01 += m.m01;
    m10 += m.m10; m11 += m.m11;
    m20 += m.m20; m21 += m.m21
  }
  def -=(m: inMat3x2f) {
    m00 -= m.m00; m01 -= m.m01;
    m10 -= m.m10; m11 -= m.m11;
    m20 -= m.m20; m21 -= m.m21
  }

  def *=(m: inMat3f) {
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
  def /=(m: inMat3x2f) {
    m00 /= m.m00; m01 /= m.m01
    m10 /= m.m10; m11 /= m.m11
    m20 /= m.m20; m21 /= m.m21
  }

  final def applyScale(s: Float) { this *= s }
  final def applyScale(s: inVec2f) {
    m00 *= s.x; m01 *= s.y
    m10 *= s.x; m11 *= s.y
    m20 *= s.x; m21 *= s.y
  }

  final def applyRotation(angle: Float) {
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

  final def applyTranslation(u: inVec2f) {
    m20 += u.x; m21 += u.y
  }

  final def applyTransformation(m: inMat3x2f) {
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
  final def applyTransformation(m: inMat2f) {
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
      case 2 =>
        r match {
          case 0 => m20 = s
          case 1 => m21 = s
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
}

object Mat3x2f {
  final val Zero = ConstMat3x2f(0)
  final val Identity = ConstMat3x2f(1)

  final val Manifest = classType[Mat3x2f](classOf[Mat3x2f])
  final val ConstManifest = classType[ConstMat3x2f](classOf[ConstMat3x2f])
  final val ReadManifest = classType[ReadMat3x2f](classOf[ReadMat3x2f])

  def apply(s: Float) = new Mat3x2f(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m01: Float,
    m10: Float, m11: Float,
    m20: Float, m21: Float
  ) = new Mat3x2f(
    m00, m01,
    m10, m11,
    m20, m21
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new Mat3x2f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy
  )

  def apply(m: AnyMat[_]) = new Mat3x2f(
    m.f00, m.f01,
    m.f10, m.f11,
    m.f20, m.f21
  )

  def unapply(m: ReadMat3x2f) = Some((m(0), m(1), m(2)))

  def scale(s: Float) :Mat3x2f = Mat3x2f(s)
  def scale(s: inVec2f) :Mat3x2f = {
    val m = Mat3x2f(s.x)
    m.m11 = s.y
    m
  }

  def rotate(angle: Float) :Mat3x2f = {
    val cosA = cos(angle)
    val sinA = sin(angle)

    new Mat3x2f(
       cosA, sinA,
      -sinA, cosA,
       0, 0
    )
  }

  def translate(u: inVec2f) :Mat3x2f = {
    val m = Mat3x2f(1)
    m(2) = u
    m
  }

  def concat(m: inMat3x2f) :Mat3x2f = Mat3x2f(m)
  def concat(m: inMat2f) :Mat3x2f = Mat3x2f(m)
}
