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
                      
import scala.reflect.ClassManifest.{classType}
import simplex3d.math.integration._
import simplex3d.math.types._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat4x2d extends ProtectedMat4x2d[Double]
with ReadPropertyValue[Mat4x2d] with Cloneable[ReadMat4x2d] with Serializable
{

  def toConst() :ConstMat4x2d
  final def mutableCopy() = Mat4x2d(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01
  final def m10 = p10; final def m11 = p11
  final def m20 = p20; final def m21 = p21
  final def m30 = p30; final def m31 = p31


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m01_=(s: Double) { throw new UnsupportedOperationException }

  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }

  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }

  protected def m30_=(s: Double) { throw new UnsupportedOperationException }
  protected def m31_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f01 = m01.toFloat

  private[math] final override def f10 = m10.toFloat
  private[math] final override def f11 = m11.toFloat

  private[math] final override def f20 = m20.toFloat
  private[math] final override def f21 = m21.toFloat

  private[math] final override def f30 = m30.toFloat
  private[math] final override def f31 = m31.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21

  private[math] final override def d30 = m30
  private[math] final override def d31 = m31


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m01)
      case 1 => new ConstVec2d(m10, m11)
      case 2 => new ConstVec2d(m20, m21)
      case 3 => new ConstVec2d(m30, m31)
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
      case 3 =>
        r match {
          case 0 => m30
          case 1 => m31
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat4x2d = this
  final def unary_-() = new Mat4x2d(
    -m00, -m01,
    -m10, -m11,
    -m20, -m21,
    -m30, -m31
  )
  final def *(s: Double) = new Mat4x2d(
    s*m00, s*m01,
    s*m10, s*m11,
    s*m20, s*m21,
    s*m30, s*m31
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat4x2d(
    m00 + s, m01 + s,
    m10 + s, m11 + s,
    m20 + s, m21 + s,
    m30 + s, m31 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat4x2d) = new Mat4x2d(
    m00 + m.m00, m01 + m.m01,
    m10 + m.m10, m11 + m.m11,
    m20 + m.m20, m21 + m.m21,
    m30 + m.m30, m31 + m.m31
  )
  final def -(m: inMat4x2d) = new Mat4x2d(
    m00 - m.m00, m01 - m.m01,
    m10 - m.m10, m11 - m.m11,
    m20 - m.m20, m21 - m.m21,
    m30 - m.m30, m31 - m.m31
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat4x2d) = new Mat4x2d(
    m00/m.m00, m01/m.m01,
    m10/m.m10, m11/m.m11,
    m20/m.m20, m21/m.m21,
    m30/m.m30, m31/m.m31
  )
  private[math] final def divByComp(s: Double) = new Mat4x2d(
    s/m00, s/m01,
    s/m10, s/m11,
    s/m20, s/m21,
    s/m30, s/m31
  )

  final def *(m: inMat2x4d) = new Mat2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13
  )
  final def *(m: inMat3x4d) = new Mat3x2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13,

    m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23,
    m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23
  )
  final def *(m: inMat4d) = new Mat4x2d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13,

    m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23,
    m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23,

    m00*m.m30 + m10*m.m31 + m20*m.m32 + m30*m.m33,
    m01*m.m30 + m11*m.m31 + m21*m.m32 + m31*m.m33
  )

  final def *(u: inVec4d) = new Vec2d(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w
  )
  private[math] final def transposeMult(u: inVec2d) = new Vec4d(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y,
    m30*u.x + m31*u.y
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat4x2[_] =>
        d00 == m.d00 && d01 == m.d01 &&
        d10 == m.d10 && d11 == m.d11 &&
        d20 == m.d20 && d21 == m.d21 &&
        d30 == m.d30 && d31 == m.d31
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
                ) + m01.hashCode
              ) + m10.hashCode
            ) + m11.hashCode
          ) + m20.hashCode
        ) + m21.hashCode
      ) + m30.hashCode
    ) + m31.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat2x4" +
    "(" +
      m00 + ", " + m01 + ",   " + 
      m10 + ", " + m11 + ",   " + 
      m20 + ", " + m21 + ",   " + 
      m30 + ", " + m31 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat4x2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double,
  c20: Double, c21: Double,
  c30: Double, c31: Double
) extends ReadMat4x2d with Immutable with Cloneable[ConstMat4x2d] with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21
  p30 = c30; p31 = c31

  override def clone() = this
  def toConst() = this
}

object ConstMat4x2d {
  def apply(s: Double) = new ConstMat4x2d(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double,
    m20: Double, m21: Double,
    m30: Double, m31: Double
  ) = new ConstMat4x2d(
    m00, m01,
    m10, m11,
    m20, m21,
    m30, m31
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_], c3: AnyVec2[_]) = 
  new ConstMat4x2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy,
    c3.dx, c3.dy
  )

  def apply(m: AnyMat[_]) = new ConstMat4x2d(
    m.d00, m.d01,
    m.d10, m.d11,
    m.d20, m.d21,
    m.d30, m.d31
  )

  implicit def toConst(m: ReadMat4x2d) = ConstMat4x2d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat4x2d private[math] (
  c00: Double, c01: Double,
  c10: Double, c11: Double,
  c20: Double, c21: Double,
  c30: Double, c31: Double
)
extends ReadMat4x2d with Accessor with CompositeFormat
with PropertyValue[Mat4x2d] with Cloneable[Mat4x2d] with Serializable
{
  p00 = c00; p01 = c01
  p10 = c10; p11 = c11
  p20 = c20; p21 = c21
  p30 = c30; p31 = c31

  type Read = ReadMat4x2d
  type Const = ConstMat4x2d

  type Accessor = Mat4x2d
  type Component = RDouble

  override def clone() = Mat4x2d(this)
  def toConst() = ConstMat4x2d(this)
  def :=(u: ConstMat4x2d) { this := u.asInstanceOf[inMat4x2d] }
  
  def :=(r: Readable[Mat4x2d]) {
    val m = r.asInstanceOf[ReadMat4x2d]
    m00 = m.m00; m01 = m.m01
    m10 = m.m10; m11 = m.m11
    m20 = m.m20; m21 = m.m21
    m30 = m.m30; m31 = m.m31
  }

  
  override def m00_=(s: Double) { p00 = s }
  override def m01_=(s: Double) { p01 = s }

  override def m10_=(s: Double) { p10 = s }
  override def m11_=(s: Double) { p11 = s }

  override def m20_=(s: Double) { p20 = s }
  override def m21_=(s: Double) { p21 = s }

  override def m30_=(s: Double) { p30 = s }
  override def m31_=(s: Double) { p31 = s }


  def *=(s: Double) {
    m00 *= s; m01 *= s;
    m10 *= s; m11 *= s;
    m20 *= s; m21 *= s;
    m30 *= s; m31 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m01 += s
    m10 += s; m11 += s
    m20 += s; m21 += s
    m30 += s; m31 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat4x2d) {
    m00 += m.m00; m01 += m.m01;
    m10 += m.m10; m11 += m.m11;
    m20 += m.m20; m21 += m.m21;
    m30 += m.m30; m31 += m.m31
  }
  def -=(m: inMat4x2d) {
    m00 -= m.m00; m01 -= m.m01;
    m10 -= m.m10; m11 -= m.m11;
    m20 -= m.m20; m21 -= m.m21;
    m30 -= m.m30; m31 -= m.m31
  }

  def *=(m: inMat4d) {
    val t00 = m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03
    val t01 = m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03

    val t10 = m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13
    val t11 = m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13

    val t20 = m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23
    val t21 = m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23

    val t30 = m00*m.m30 + m10*m.m31 + m20*m.m32 + m30*m.m33
        m31 = m01*m.m30 + m11*m.m31 + m21*m.m32 + m31*m.m33

    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20; m21 = t21
    m30 = t30
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat4x2d) {
    m00 /= m.m00; m01 /= m.m01
    m10 /= m.m10; m11 /= m.m11
    m20 /= m.m20; m21 /= m.m21
    m30 /= m.m30; m31 /= m.m31
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
      case 3 =>
        r match {
          case 0 => m30 = s
          case 1 => m31 = s
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
      case 3 => m30 = v.x; m31 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat4x2d {
  final val Zero = ConstMat4x2d(0)
  final val Identity = ConstMat4x2d(1)

  final val Manifest = classType[Mat4x2d](classOf[Mat4x2d])
  final val ConstManifest = classType[ConstMat4x2d](classOf[ConstMat4x2d])
  final val ReadManifest = classType[ReadMat4x2d](classOf[ReadMat4x2d])

  def apply(s: Double) = new Mat4x2d(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double,
    m10: Double, m11: Double,
    m20: Double, m21: Double,
    m30: Double, m31: Double
  ) = new Mat4x2d(
    m00, m01,
    m10, m11,
    m20, m21,
    m30, m31
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_], c3: AnyVec2[_]) = 
  new Mat4x2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy,
    c3.dx, c3.dy
  )

  def apply(m: AnyMat[_]) = new Mat4x2d(
    m.d00, m.d01,
    m.d10, m.d11,
    m.d20, m.d21,
    m.d30, m.d31
  )

  def unapply(m: ReadMat4x2d) = Some((m(0), m(1), m(2), m(3)))
}
