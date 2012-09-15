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
sealed abstract class ReadMat3x4d extends ProtectedMat3x4d[Double]
with Protected with Serializable
{

  type Clone <: ReadMat3x4d
  def toConst() :ConstMat3x4d
  
  type Read = ReadMat3x4d
  type Mutable = Mat3x4d
  final def readType: Class[Read] = classOf[ReadMat3x4d]
  final def mutableCopy() = Mat3x4d(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01; final def m02 = p02; final def m03 = p03
  final def m10 = p10; final def m11 = p11; final def m12 = p12; final def m13 = p13
  final def m20 = p20; final def m21 = p21; final def m22 = p22; final def m23 = p23


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m02_=(s: Double) { throw new UnsupportedOperationException }
  protected def m03_=(s: Double) { throw new UnsupportedOperationException }

  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }
  protected def m12_=(s: Double) { throw new UnsupportedOperationException }
  protected def m13_=(s: Double) { throw new UnsupportedOperationException }

  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }
  protected def m22_=(s: Double) { throw new UnsupportedOperationException }
  protected def m23_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f01 = m01.toFloat
  private[math] final override def f02 = m02.toFloat
  private[math] final override def f03 = m03.toFloat

  private[math] final override def f10 = m10.toFloat
  private[math] final override def f11 = m11.toFloat
  private[math] final override def f12 = m12.toFloat
  private[math] final override def f13 = m13.toFloat

  private[math] final override def f20 = m20.toFloat
  private[math] final override def f21 = m21.toFloat
  private[math] final override def f22 = m22.toFloat
  private[math] final override def f23 = m23.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01
  private[math] final override def d02 = m02
  private[math] final override def d03 = m03

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11
  private[math] final override def d12 = m12
  private[math] final override def d13 = m13

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21
  private[math] final override def d22 = m22
  private[math] final override def d23 = m23


  final def apply(c: Int) :ConstVec4d = {
    c match {
      case 0 => new ConstVec4d(m00, m01, m02, m03)
      case 1 => new ConstVec4d(m10, m11, m12, m13)
      case 2 => new ConstVec4d(m20, m21, m22, m23)
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
          case 2 => m02
          case 3 => m03
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10
          case 1 => m11
          case 2 => m12
          case 3 => m13
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20
          case 1 => m21
          case 2 => m22
          case 3 => m23
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat3x4d = this
  final def unary_-() = new Mat3x4d(
    -m00, -m01, -m02, -m03,
    -m10, -m11, -m12, -m13,
    -m20, -m21, -m22, -m23
  )
  final def *(s: Double) = new Mat3x4d(
    s*m00, s*m01, s*m02, s*m03,
    s*m10, s*m11, s*m12, s*m13,
    s*m20, s*m21, s*m22, s*m23
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat3x4d(
    m00 + s, m01 + s, m02 + s, m03 + s,
    m10 + s, m11 + s, m12 + s, m13 + s,
    m20 + s, m21 + s, m22 + s, m23 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat3x4d) = new Mat3x4d(
    m00 + m.m00, m01 + m.m01, m02 + m.m02, m03 + m.m03,
    m10 + m.m10, m11 + m.m11, m12 + m.m12, m13 + m.m13,
    m20 + m.m20, m21 + m.m21, m22 + m.m22, m23 + m.m23
  )
  final def -(m: inMat3x4d) = new Mat3x4d(
    m00 - m.m00, m01 - m.m01, m02 - m.m02, m03 - m.m03,
    m10 - m.m10, m11 - m.m11, m12 - m.m12, m13 - m.m13,
    m20 - m.m20, m21 - m.m21, m22 - m.m22, m23 - m.m23
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat3x4d) = new Mat3x4d(
    m00/m.m00, m01/m.m01, m02/m.m02, m03/m.m03,
    m10/m.m10, m11/m.m11, m12/m.m12, m13/m.m13,
    m20/m.m20, m21/m.m21, m22/m.m22, m23/m.m23
  )
  private[math] final def divByComp(s: Double) = new Mat3x4d(
    s/m00, s/m01, s/m02, s/m03,
    s/m10, s/m11, s/m12, s/m13,
    s/m20, s/m21, s/m22, s/m23
  )

  final def *(m: inMat2x3d) = new Mat2x4d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,
    m03*m.m00 + m13*m.m01 + m23*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,
    m03*m.m10 + m13*m.m11 + m23*m.m12
  )
  final def *(m: inMat3d) = new Mat3x4d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,
    m03*m.m00 + m13*m.m01 + m23*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,
    m03*m.m10 + m13*m.m11 + m23*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,
    m02*m.m20 + m12*m.m21 + m22*m.m22,
    m03*m.m20 + m13*m.m21 + m23*m.m22
  )
  final def *(m: inMat4x3d) = new Mat4d(
    m00*m.m00 + m10*m.m01 + m20*m.m02,
    m01*m.m00 + m11*m.m01 + m21*m.m02,
    m02*m.m00 + m12*m.m01 + m22*m.m02,
    m03*m.m00 + m13*m.m01 + m23*m.m02,

    m00*m.m10 + m10*m.m11 + m20*m.m12,
    m01*m.m10 + m11*m.m11 + m21*m.m12,
    m02*m.m10 + m12*m.m11 + m22*m.m12,
    m03*m.m10 + m13*m.m11 + m23*m.m12,

    m00*m.m20 + m10*m.m21 + m20*m.m22,
    m01*m.m20 + m11*m.m21 + m21*m.m22,
    m02*m.m20 + m12*m.m21 + m22*m.m22,
    m03*m.m20 + m13*m.m21 + m23*m.m22,

    m00*m.m30 + m10*m.m31 + m20*m.m32,
    m01*m.m30 + m11*m.m31 + m21*m.m32,
    m02*m.m30 + m12*m.m31 + m22*m.m32,
    m03*m.m30 + m13*m.m31 + m23*m.m32
  )

  final def *(u: inVec3d) = new Vec4d(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z,
    m02*u.x + m12*u.y + m22*u.z,
    m03*u.x + m13*u.y + m23*u.z
  )
  private[math] final def transposeMult(u: inVec4d) = new Vec3d(
    m00*u.x + m01*u.y + m02*u.z + m03*u.w,
    m10*u.x + m11*u.y + m12*u.z + m13*u.w,
    m20*u.x + m21*u.y + m22*u.z + m23*u.w
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat3x4[_] =>
        d00 == m.d00 && d01 == m.d01 && d02 == m.d02 && d03 == m.d03 &&
        d10 == m.d10 && d11 == m.d11 && d12 == m.d12 && d13 == m.d13 &&
        d20 == m.d20 && d21 == m.d21 && d22 == m.d22 && d23 == m.d23
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
                  41 * (
                    41 * (
                      41 * (
                        41 * (
                          41 + m00.hashCode
                        ) + m01.hashCode
                      ) + m02.hashCode
                    ) + m03.hashCode
                  ) + m10.hashCode
                ) + m11.hashCode
              ) + m12.hashCode
            ) + m13.hashCode
          ) + m20.hashCode
        ) + m21.hashCode
      ) + m22.hashCode
    ) + m23.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat3x4" +
    "(" +
      m00 + ", " + m01 + ", " + m02 + ", " + m03 + ",   " + 
      m10 + ", " + m11 + ", " + m12 + ", " + m13 + ",   " + 
      m20 + ", " + m21 + ", " + m22 + ", " + m23 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3x4d private[math] (
  c00: Double, c01: Double, c02: Double, c03: Double,
  c10: Double, c11: Double, c12: Double, c13: Double,
  c20: Double, c21: Double, c22: Double, c23: Double
) extends ReadMat3x4d with Immutable with Serializable
{
  p00 = c00; p01 = c01; p02 = c02; p03 = c03
  p10 = c10; p11 = c11; p12 = c12; p13 = c13
  p20 = c20; p21 = c21; p22 = c22; p23 = c23


  type Clone = ConstMat3x4d
  override def clone() = this
  def toConst() = this
}

object ConstMat3x4d {
  def apply(s: Double) = new ConstMat3x4d(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double, m02: Double, m03: Double,
    m10: Double, m11: Double, m12: Double, m13: Double,
    m20: Double, m21: Double, m22: Double, m23: Double
  ) = new ConstMat3x4d(
    m00, m01, m02, m03,
    m10, m11, m12, m13,
    m20, m21, m22, m23
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_], c2: AnyVec4[_]) = 
  new ConstMat3x4d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw,
    c2.dx, c2.dy, c2.dz, c2.dw
  )

  def apply(m: AnyMat[_]) = new ConstMat3x4d(
    m.d00, m.d01, m.d02, m.d03,
    m.d10, m.d11, m.d12, m.d13,
    m.d20, m.d21, m.d22, m.d23
  )

  implicit def toConst(m: ReadMat3x4d) = ConstMat3x4d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3x4d private[math] (
  c00: Double, c01: Double, c02: Double, c03: Double,
  c10: Double, c11: Double, c12: Double, c13: Double,
  c20: Double, c21: Double, c22: Double, c23: Double
)
extends ReadMat3x4d with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01; p02 = c02; p03 = c03
  p10 = c10; p11 = c11; p12 = c12; p13 = c13
  p20 = c20; p21 = c21; p22 = c22; p23 = c23

  private[math] def this() = this(
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0
  )
  
  type Clone = Mat3x4d
  type Const = ConstMat3x4d

  type Accessor = Mat3x4d
  type Component = RDouble

  override def clone() = Mat3x4d(this)
  def toConst() = ConstMat3x4d(this)

  def :=(m: inMat3x4d) {
    m00 = m.m00; m01 = m.m01; m02 = m.m02; m03 = m.m03
    m10 = m.m10; m11 = m.m11; m12 = m.m12; m13 = m.m13
    m20 = m.m20; m21 = m.m21; m22 = m.m22; m23 = m.m23
  }


  override def m00_=(s: Double) { p00 = s }
  override def m01_=(s: Double) { p01 = s }
  override def m02_=(s: Double) { p02 = s }
  override def m03_=(s: Double) { p03 = s }

  override def m10_=(s: Double) { p10 = s }
  override def m11_=(s: Double) { p11 = s }
  override def m12_=(s: Double) { p12 = s }
  override def m13_=(s: Double) { p13 = s }

  override def m20_=(s: Double) { p20 = s }
  override def m21_=(s: Double) { p21 = s }
  override def m22_=(s: Double) { p22 = s }
  override def m23_=(s: Double) { p23 = s }


  def *=(s: Double) {
    m00 *= s; m01 *= s; m02 *= s; m03 *= s;
    m10 *= s; m11 *= s; m12 *= s; m13 *= s;
    m20 *= s; m21 *= s; m22 *= s; m23 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m01 += s; m02 += s; m03 += s
    m10 += s; m11 += s; m12 += s; m13 += s
    m20 += s; m21 += s; m22 += s; m23 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat3x4d) {
    m00 += m.m00; m01 += m.m01; m02 += m.m02; m03 += m.m03;
    m10 += m.m10; m11 += m.m11; m12 += m.m12; m13 += m.m13;
    m20 += m.m20; m21 += m.m21; m22 += m.m22; m23 += m.m23
  }
  def -=(m: inMat3x4d) {
    m00 -= m.m00; m01 -= m.m01; m02 -= m.m02; m03 -= m.m03;
    m10 -= m.m10; m11 -= m.m11; m12 -= m.m12; m13 -= m.m13;
    m20 -= m.m20; m21 -= m.m21; m22 -= m.m22; m23 -= m.m23
  }

  def *=(m: inMat3d) {
    val t00 = m00*m.m00 + m10*m.m01 + m20*m.m02
    val t01 = m01*m.m00 + m11*m.m01 + m21*m.m02
    val t02 = m02*m.m00 + m12*m.m01 + m22*m.m02
    val t03 = m03*m.m00 + m13*m.m01 + m23*m.m02

    val t10 = m00*m.m10 + m10*m.m11 + m20*m.m12
    val t11 = m01*m.m10 + m11*m.m11 + m21*m.m12
    val t12 = m02*m.m10 + m12*m.m11 + m22*m.m12
    val t13 = m03*m.m10 + m13*m.m11 + m23*m.m12

    val t20 = m00*m.m20 + m10*m.m21 + m20*m.m22
    val t21 = m01*m.m20 + m11*m.m21 + m21*m.m22
    val t22 = m02*m.m20 + m12*m.m21 + m22*m.m22
        m23 = m03*m.m20 + m13*m.m21 + m23*m.m22

    m00 = t00; m01 = t01; m02 = t02; m03 = t03
    m10 = t10; m11 = t11; m12 = t12; m13 = t13
    m20 = t20; m21 = t21; m22 = t22
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat3x4d) {
    m00 /= m.m00; m01 /= m.m01; m02 /= m.m02; m03 /= m.m03
    m10 /= m.m10; m11 /= m.m11; m12 /= m.m12; m13 /= m.m13
    m20 /= m.m20; m21 /= m.m21; m22 /= m.m22; m23 /= m.m23
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
          case 2 => m02 = s
          case 3 => m03 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10 = s
          case 1 => m11 = s
          case 2 => m12 = s
          case 3 => m13 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20 = s
          case 1 => m21 = s
          case 2 => m22 = s
          case 3 => m23 = s
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

  def update(c: Int, v: inVec3d) {
    c match {
      case 0 => m00 = v.x; m01 = v.y; m02 = v.z
      case 1 => m10 = v.x; m11 = v.y; m12 = v.z
      case 2 => m20 = v.x; m21 = v.y; m22 = v.z
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def update(c: Int, v: inVec4d) {
    c match {
      case 0 => m00 = v.x; m01 = v.y; m02 = v.z; m03 = v.w
      case 1 => m10 = v.x; m11 = v.y; m12 = v.z; m13 = v.w
      case 2 => m20 = v.x; m21 = v.y; m22 = v.z; m23 = v.w
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat3x4d {
  final val Zero = ConstMat3x4d(0)
  final val Identity = ConstMat3x4d(1)

  final val Manifest = classType[Mat3x4d](classOf[Mat3x4d])
  final val ConstManifest = classType[ConstMat3x4d](classOf[ConstMat3x4d])
  final val ReadManifest = classType[ReadMat3x4d](classOf[ReadMat3x4d])

  def apply(s: Double) = new Mat3x4d(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double, m02: Double, m03: Double,
    m10: Double, m11: Double, m12: Double, m13: Double,
    m20: Double, m21: Double, m22: Double, m23: Double
  ) = new Mat3x4d(
    m00, m01, m02, m03,
    m10, m11, m12, m13,
    m20, m21, m22, m23
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_], c2: AnyVec4[_]) = 
  new Mat3x4d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw,
    c2.dx, c2.dy, c2.dz, c2.dw
  )

  def apply(m: AnyMat[_]) = new Mat3x4d(
    m.d00, m.d01, m.d02, m.d03,
    m.d10, m.d11, m.d12, m.d13,
    m.d20, m.d21, m.d22, m.d23
  )

  def unapply(m: ReadMat3x4d) = Some((m(0), m(1), m(2)))
}
