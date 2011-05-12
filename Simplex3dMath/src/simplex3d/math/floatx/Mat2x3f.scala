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
sealed abstract class ReadMat2x3f extends ProtectedMat2x3f[Float]
with ReadPropertyRef[ReadMat2x3f] with Serializable
{

  type Clone <: ReadMat2x3f
  type Const = ConstMat2x3f
  def toConst() = ConstMat2x3f(this)

  // Column major order.
  final def m00 = p00; final def m10 = p10
  final def m01 = p01; final def m11 = p11
  final def m02 = p02; final def m12 = p12


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }

  protected def m02_=(s: Float) { throw new UnsupportedOperationException }
  protected def m12_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f10 = m10

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11

  private[math] final override def f02 = m02
  private[math] final override def f12 = m12


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12


  final def apply(c: Int) :ConstVec2f = {
    c match {
      case 0 => new ConstVec2f(m00, m10)
      case 1 => new ConstVec2f(m01, m11)
      case 2 => new ConstVec2f(m02, m12)
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 2, got " + j + "."
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
      case _ => error
    }
  }

  final def unary_+() :ReadMat2x3f = this
  final def unary_-() = new Mat2x3f(
    -m00, -m10,
    -m01, -m11,
    -m02, -m12
  )
  final def *(s: Float) = new Mat2x3f(
    s*m00, s*m10,
    s*m01, s*m11,
    s*m02, s*m12
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat2x3f(
    m00 + s, m10 + s,
    m01 + s, m11 + s,
    m02 + s, m12 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat2x3f) = new Mat2x3f(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11,
    m02 + m.m02, m12 + m.m12
  )
  final def -(m: inMat2x3f) = new Mat2x3f(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11,
    m02 - m.m02, m12 - m.m12
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2x3f) = new Mat2x3f(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11,
    m02/m.m02, m12/m.m12
  )
  private[math] final def divByComp(s: Float) = new Mat2x3f(
    s/m00, s/m10,
    s/m01, s/m11,
    s/m02, s/m12
  )

  final def *(m: inMat3x2f) = new Mat2f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21
  )
  final def *(m: inMat3f) = new Mat2x3f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22
  )
  final def *(m: inMat3x4f) = new Mat2x4f(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22,

    m00*m.m03 + m01*m.m13 + m02*m.m23,
    m10*m.m03 + m11*m.m13 + m12*m.m23
  )

  final def *(u: inVec3f) = new Vec2f(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z
  )
  private[math] final def transposeMult(u: inVec2f) = new Vec3f(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y,
    m02*u.x + m12*u.y
  )

  final def scale(s: Float) :Mat2x3f = this*s
  final def scale(s: inVec2f) :Mat2x3f = new Mat2x3f(
    m00*s.x, m10*s.y,
    m01*s.x, m11*s.y,
    m02*s.x, m12*s.y
  )

  final def rotate(angle: Float) :Mat2x3f = {
    concat(rotationMat(angle))
  }

  final def translate(u: inVec2f) :Mat2x3f = new Mat2x3f(
    m00, m10,
    m01, m11,
    m02 + u.x, m12 + u.y
  )

  final def concat(m: inMat2x3f) :Mat2x3f = new Mat2x3f(
    m.m00*m00 + m.m01*m10,
    m.m10*m00 + m.m11*m10,

    m.m00*m01 + m.m01*m11,
    m.m10*m01 + m.m11*m11,

    m.m00*m02 + m.m01*m12 + m.m02,
    m.m10*m02 + m.m11*m12 + m.m12
  )
  final def concat(m: inMat2f) :Mat2x3f = m*this

  final def transformPoint(p: inVec2f) :Vec2f = new Vec2f(
    m00*p.x + m01*p.y + m02,
    m10*p.x + m11*p.y + m12
  )
  final def transformVector(v: inVec2f) :Vec2f = new Vec2f(
    m00*v.x + m01*v.y,
    m10*v.x + m11*v.y
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2x3[_] =>
        d00 == m.d00 && d10 == m.d10 &&
        d01 == m.d01 && d11 == m.d11 &&
        d02 == m.d02 && d12 == m.d12
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
          ) + m01.hashCode
        ) + m11.hashCode
      ) + m02.hashCode
    ) + m12.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2x3" +
    "(" +
      m00 + "f, " + m10 + "f,   " + 
      m01 + "f, " + m11 + "f,   " + 
      m02 + "f, " + m12 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat2x3f private[math] (
  c00: Float, c10: Float,
  c01: Float, c11: Float,
  c02: Float, c12: Float
) extends ReadMat2x3f with Immutable with Serializable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12

  type Clone = ConstMat2x3f
  override def clone() = this
}

object ConstMat2x3f {
  def apply(s: Float) = new ConstMat2x3f(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float,
    m02: Float, m12: Float
  ) = new ConstMat2x3f(
    m00, m10,
    m01, m11,
    m02, m12
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new ConstMat2x3f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy
  )

  def apply(m: AnyMat[_]) = new ConstMat2x3f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12
  )

  implicit def toConst(m: ReadMat2x3f) = ConstMat2x3f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat2x3f private[math] (
  c00: Float, c10: Float,
  c01: Float, c11: Float,
  c02: Float, c12: Float
)
extends ReadMat2x3f with CompositeMeta with Implicits[On]
with PropertyRef[ReadMat2x3f] with Serializable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12

  type Read = ReadMat2x3f
  type Component = RFloat
  type Clone = Mat2x3f
  override def clone() = Mat2x3f(this)
  def :=(u: ConstMat2x3f) { this := u.asInstanceOf[inMat2x3f] }
  
  def :=(m: inMat2x3f) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11;
    m02 = m.m02; m12 = m.m12
  }

  
  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }

  override def m02_=(s: Float) { p02 = s }
  override def m12_=(s: Float) { p12 = s }


  def *=(s: Float) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s;
    m02 *= s; m12 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s
    m01 += s; m11 += s
    m02 += s; m12 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat2x3f) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11;
    m02 += m.m02; m12 += m.m12
  }
  def -=(m: inMat2x3f) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11;
    m02 -= m.m02; m12 -= m.m12
  }

  def *=(m: inMat3f) {
    val t00 = m00*m.m00 + m01*m.m10 + m02*m.m20
    val t10 = m10*m.m00 + m11*m.m10 + m12*m.m20

    val t01 = m00*m.m01 + m01*m.m11 + m02*m.m21
    val t11 = m10*m.m01 + m11*m.m11 + m12*m.m21

    val t02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        m12 = m10*m.m02 + m11*m.m12 + m12*m.m22

    m00 = t00; m10 = t10
    m01 = t01; m11 = t11
    m02 = t02
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat2x3f) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
    m02 /= m.m02; m12 /= m.m12
  }

  final def applyScale(s: Float) { this *= s }
  final def applyScale(s: inVec2f) {
    m00 *= s.x; m10 *= s.y
    m01 *= s.x; m11 *= s.y
    m02 *= s.x; m12 *= s.y
  }

  final def applyRotation(angle: Float) {
    applyTransform(rotationMat(angle))
  }

  final def applyTranslation(u: inVec2f) {
    m02 += u.x; m12 += u.y
  }

  final def applyTransform(m: inMat2x3f) {
    val t00 = m.m00*m00 + m.m01*m10
    val t10 = m.m10*m00 + m.m11*m10

    val t01 = m.m00*m01 + m.m01*m11
    val t11 = m.m10*m01 + m.m11*m11

    val t02 = m.m00*m02 + m.m01*m12 + m.m02
        m12 = m.m10*m02 + m.m11*m12 + m.m12
    
    m00 = t00; m10 = t10
    m01 = t01; m11 = t11
    m02 = t02
  }
  final def applyTransform(m: inMat2f) {
    val t00 = m.m00*m00 + m.m01*m10
    val t10 = m.m10*m00 + m.m11*m10

    val t01 = m.m00*m01 + m.m01*m11
    val t11 = m.m10*m01 + m.m11*m11

    val t02 = m.m00*m02 + m.m01*m12
        m12 = m.m10*m02 + m.m11*m12
    
    m00 = t00; m10 = t10
    m01 = t01; m11 = t11
    m02 = t02
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
      case _ => error
    }
  }

  def update(c: Int, v: inVec2f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y
      case 1 => m01 = v.x; m11 = v.y
      case 2 => m02 = v.x; m12 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
        )
    }
  }
}

object Mat2x3f {
  final val Zero = ConstMat2x3f(0)
  final val Identity = ConstMat2x3f(1)

  final val Manifest = classType[Mat2x3f](classOf[Mat2x3f])
  final val ConstManifest = classType[ConstMat2x3f](classOf[ConstMat2x3f])
  final val ReadManifest = classType[ReadMat2x3f](classOf[ReadMat2x3f])

  def apply(s: Float) = new Mat2x3f(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float,
    m01: Float, m11: Float,
    m02: Float, m12: Float
  ) = new Mat2x3f(
    m00, m10,
    m01, m11,
    m02, m12
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new Mat2x3f(
    c0.fx, c0.fy,
    c1.fx, c1.fy,
    c2.fx, c2.fy
  )

  def apply(m: AnyMat[_]) = new Mat2x3f(
    m.f00, m.f10,
    m.f01, m.f11,
    m.f02, m.f12
  )

  def unapply(m: ReadMat2x3f) = Some((m(0), m(1), m(2)))

  def scale(s: Float) :Mat2x3f = Mat2x3f(s)
  def scale(s: inVec2f) :Mat2x3f = {
    val m = Mat2x3f(s.x)
    m.m11 = s.y
    m
  }

  def rotate(angle: Float) :Mat2x3f = {
    Mat2x3f(rotationMat(angle))
  }

  def translate(u: inVec2f) :Mat2x3f = {
    val m = Mat2x3f(1)
    m(2) = u
    m
  }

  def concat(m: inMat2x3f) :Mat2x3f = Mat2x3f(m)
  def concat(m: inMat2f) :Mat2x3f = Mat2x3f(m)

  implicit def toMutable(m: ReadMat2x3f) = Mat2x3f(m)
}
