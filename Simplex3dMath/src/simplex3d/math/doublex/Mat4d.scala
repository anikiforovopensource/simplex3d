/*
 * Simplex3d, DoubleMath module
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
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat4d extends ProtectedMat4d[Double]
with ReadPropertyRef[ReadMat4d] with Serializable
{

  type Clone <: ReadMat4d
  type Const = ConstMat4d
  def toConst() = ConstMat4d(this)

  // Column major order.
  final def m00 = p00; final def m10 = p10; final def m20 = p20; final def m30 = p30
  final def m01 = p01; final def m11 = p11; final def m21 = p21; final def m31 = p31
  final def m02 = p02; final def m12 = p12; final def m22 = p22; final def m32 = p32
  final def m03 = p03; final def m13 = p13; final def m23 = p23; final def m33 = p33


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m30_=(s: Double) { throw new UnsupportedOperationException }

  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }
  protected def m31_=(s: Double) { throw new UnsupportedOperationException }

  protected def m02_=(s: Double) { throw new UnsupportedOperationException }
  protected def m12_=(s: Double) { throw new UnsupportedOperationException }
  protected def m22_=(s: Double) { throw new UnsupportedOperationException }
  protected def m32_=(s: Double) { throw new UnsupportedOperationException }

  protected def m03_=(s: Double) { throw new UnsupportedOperationException }
  protected def m13_=(s: Double) { throw new UnsupportedOperationException }
  protected def m23_=(s: Double) { throw new UnsupportedOperationException }
  protected def m33_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f10 = m10.toFloat
  private[math] final override def f20 = m20.toFloat
  private[math] final override def f30 = m30.toFloat

  private[math] final override def f01 = m01.toFloat
  private[math] final override def f11 = m11.toFloat
  private[math] final override def f21 = m21.toFloat
  private[math] final override def f31 = m31.toFloat

  private[math] final override def f02 = m02.toFloat
  private[math] final override def f12 = m12.toFloat
  private[math] final override def f22 = m22.toFloat
  private[math] final override def f32 = m32.toFloat

  private[math] final override def f03 = m03.toFloat
  private[math] final override def f13 = m13.toFloat
  private[math] final override def f23 = m23.toFloat
  private[math] final override def f33 = m33.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20
  private[math] final override def d30 = m30

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21
  private[math] final override def d31 = m31

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12
  private[math] final override def d22 = m22
  private[math] final override def d32 = m32

  private[math] final override def d03 = m03
  private[math] final override def d13 = m13
  private[math] final override def d23 = m23
  private[math] final override def d33 = m33


  final def apply(c: Int) :ConstVec4d = {
    c match {
      case 0 => new ConstVec4d(m00, m10, m20, m30)
      case 1 => new ConstVec4d(m01, m11, m21, m31)
      case 2 => new ConstVec4d(m02, m12, m22, m32)
      case 3 => new ConstVec4d(m03, m13, m23, m33)
      case j => throw new IndexOutOfBoundsException(
          "Expected from 0 to 3, got " + j + "."
        )
    }
  }

  final def apply(c: Int, r: Int) :Double = {
    def error() :Double = throw new IndexOutOfBoundsException(
      "Trying to read index (" + c + ", " + r + ") in " +
      this.getClass.getSimpleName
    )

    c match {
      case 0 =>
        r match {
          case 0 => m00
          case 1 => m10
          case 2 => m20
          case 3 => m30
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case 2 => m21
          case 3 => m31
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02
          case 1 => m12
          case 2 => m22
          case 3 => m32
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03
          case 1 => m13
          case 2 => m23
          case 3 => m33
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat4d = this
  final def unary_-() = new Mat4d(
    -m00, -m10, -m20, -m30,
    -m01, -m11, -m21, -m31,
    -m02, -m12, -m22, -m32,
    -m03, -m13, -m23, -m33
  )
  final def *(s: Double) = new Mat4d(
    s*m00, s*m10, s*m20, s*m30,
    s*m01, s*m11, s*m21, s*m31,
    s*m02, s*m12, s*m22, s*m32,
    s*m03, s*m13, s*m23, s*m33
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat4d(
    m00 + s, m10 + s, m20 + s, m30 + s,
    m01 + s, m11 + s, m21 + s, m31 + s,
    m02 + s, m12 + s, m22 + s, m32 + s,
    m03 + s, m13 + s, m23 + s, m33 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat4d) = new Mat4d(
    m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
    m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31,
    m02 + m.m02, m12 + m.m12, m22 + m.m22, m32 + m.m32,
    m03 + m.m03, m13 + m.m13, m23 + m.m23, m33 + m.m33
  )
  final def -(m: inMat4d) = new Mat4d(
    m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
    m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31,
    m02 - m.m02, m12 - m.m12, m22 - m.m22, m32 - m.m32,
    m03 - m.m03, m13 - m.m13, m23 - m.m23, m33 - m.m33
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat4d) = new Mat4d(
    m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
    m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31,
    m02/m.m02, m12/m.m12, m22/m.m22, m32/m.m32,
    m03/m.m03, m13/m.m13, m23/m.m23, m33/m.m33
  )
  private[math] final def divByComp(s: Double) = new Mat4d(
    s/m00, s/m10, s/m20, s/m30,
    s/m01, s/m11, s/m21, s/m31,
    s/m02, s/m12, s/m22, s/m32,
    s/m03, s/m13, s/m23, s/m33
  )

  final def *(m: inMat4x2d) = new Mat4x2d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
    m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
    m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31
  )
  final def *(m: inMat4x3d) = new Mat4x3d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
    m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
    m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
    m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32,
    m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32
  )
  final def *(m: inMat4d) = new Mat4d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
    m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
    m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
    m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32,
    m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32,

    m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
    m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33,
    m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33,
    m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33
  )

  final def *(u: inVec4d) = new Vec4d(
    m00*u.x + m01*u.y + m02*u.z + m03*u.w,
    m10*u.x + m11*u.y + m12*u.z + m13*u.w,
    m20*u.x + m21*u.y + m22*u.z + m23*u.w,
    m30*u.x + m31*u.y + m32*u.z + m33*u.w
  )
  private[math] final def transposeMult(u: inVec4d) = new Vec4d(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w,
    m02*u.x + m12*u.y + m22*u.z + m32*u.w,
    m03*u.x + m13*u.y + m23*u.z + m33*u.w
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat4[_] =>
        d00 == m.d00 && d10 == m.d10 && d20 == m.d20 && d30 == m.d30 &&
        d01 == m.d01 && d11 == m.d11 && d21 == m.d21 && d31 == m.d31 &&
        d02 == m.d02 && d12 == m.d12 && d22 == m.d22 && d32 == m.d32 &&
        d03 == m.d03 && d13 == m.d13 && d23 == m.d23 && d33 == m.d33
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
                          41 * (
                            41 * (
                              41 * (
                                41 * (
                                  41 + m00.hashCode
                                ) + m10.hashCode
                              ) + m20.hashCode
                            ) + m30.hashCode
                          ) + m01.hashCode
                        ) + m11.hashCode
                      ) + m21.hashCode
                    ) + m31.hashCode
                  ) + m02.hashCode
                ) + m12.hashCode
              ) + m22.hashCode
            ) + m32.hashCode
          ) + m03.hashCode
        ) + m13.hashCode
      ) + m23.hashCode
    ) + m33.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat4" +
    "(" +
      m00 + ", " + m10 + ", " + m20 + ", " + m30 + ",   " + 
      m01 + ", " + m11 + ", " + m21 + ", " + m31 + ",   " + 
      m02 + ", " + m12 + ", " + m22 + ", " + m32 + ",   " + 
      m03 + ", " + m13 + ", " + m23 + ", " + m33 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat4d private[math] (
  c00: Double, c10: Double, c20: Double, c30: Double,
  c01: Double, c11: Double, c21: Double, c31: Double,
  c02: Double, c12: Double, c22: Double, c32: Double,
  c03: Double, c13: Double, c23: Double, c33: Double
) extends ReadMat4d with Immutable with Serializable
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31
  p02 = c02; p12 = c12; p22 = c22; p32 = c32
  p03 = c03; p13 = c13; p23 = c23; p33 = c33

  type Clone = ConstMat4d
  override def clone() = this
}

object ConstMat4d {
  def apply(s: Double) = new ConstMat4d(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0,
    0, 0, 0, s
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double, m20: Double, m30: Double,
    m01: Double, m11: Double, m21: Double, m31: Double,
    m02: Double, m12: Double, m22: Double, m32: Double,
    m03: Double, m13: Double, m23: Double, m33: Double
  ) = new ConstMat4d(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_], c2: AnyVec4[_], c3: AnyVec4[_]) = 
  new ConstMat4d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw,
    c2.dx, c2.dy, c2.dz, c2.dw,
    c3.dx, c3.dy, c3.dz, c3.dw
  )

  def apply(m: AnyMat[_]) = new ConstMat4d(
    m.d00, m.d10, m.d20, m.d30,
    m.d01, m.d11, m.d21, m.d31,
    m.d02, m.d12, m.d22, m.d32,
    m.d03, m.d13, m.d23, m.d33
  )

  implicit def toConst(m: ReadMat4d) = ConstMat4d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat4d private[math] (
  c00: Double, c10: Double, c20: Double, c30: Double,
  c01: Double, c11: Double, c21: Double, c31: Double,
  c02: Double, c12: Double, c22: Double, c32: Double,
  c03: Double, c13: Double, c23: Double, c33: Double
)
extends ReadMat4d with Meta with CompositeFormat
with PropertyRef[ReadMat4d] with Serializable
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31
  p02 = c02; p12 = c12; p22 = c22; p32 = c32
  p03 = c03; p13 = c13; p23 = c23; p33 = c33

  type Read = ReadMat4d

  type Meta = Mat4d
  type Component = RDouble

  type Clone = Mat4d
  override def clone() = Mat4d(this)
  def :=(u: ConstMat4d) { this := u.asInstanceOf[inMat4d] }
  
  def :=(m: inMat4d) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
    m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31;
    m02 = m.m02; m12 = m.m12; m22 = m.m22; m32 = m.m32;
    m03 = m.m03; m13 = m.m13; m23 = m.m23; m33 = m.m33
  }

  
  override def m00_=(s: Double) { p00 = s }
  override def m10_=(s: Double) { p10 = s }
  override def m20_=(s: Double) { p20 = s }
  override def m30_=(s: Double) { p30 = s }

  override def m01_=(s: Double) { p01 = s }
  override def m11_=(s: Double) { p11 = s }
  override def m21_=(s: Double) { p21 = s }
  override def m31_=(s: Double) { p31 = s }

  override def m02_=(s: Double) { p02 = s }
  override def m12_=(s: Double) { p12 = s }
  override def m22_=(s: Double) { p22 = s }
  override def m32_=(s: Double) { p32 = s }

  override def m03_=(s: Double) { p03 = s }
  override def m13_=(s: Double) { p13 = s }
  override def m23_=(s: Double) { p23 = s }
  override def m33_=(s: Double) { p33 = s }


  def *=(s: Double) {
    m00 *= s; m10 *= s; m20 *= s; m30 *= s;
    m01 *= s; m11 *= s; m21 *= s; m31 *= s;
    m02 *= s; m12 *= s; m22 *= s; m32 *= s;
    m03 *= s; m13 *= s; m23 *= s; m33 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m10 += s; m20 += s; m30 += s
    m01 += s; m11 += s; m21 += s; m31 += s
    m02 += s; m12 += s; m22 += s; m32 += s
    m03 += s; m13 += s; m23 += s; m33 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat4d) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
    m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31;
    m02 += m.m02; m12 += m.m12; m22 += m.m22; m32 += m.m32;
    m03 += m.m03; m13 += m.m13; m23 += m.m23; m33 += m.m33
  }
  def -=(m: inMat4d) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31;
    m02 -= m.m02; m12 -= m.m12; m22 -= m.m22; m32 -= m.m32;
    m03 -= m.m03; m13 -= m.m13; m23 -= m.m23; m33 -= m.m33
  }

  def *=(m: inMat4d) {
    val t00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
    val t10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
    val t20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30
    val t30 = m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30

    val t01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
    val t11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
    val t21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
    val t31 = m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31

    val t02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
    val t12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
    val t22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
    val t32 = m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32

    val t03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
    val t13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
    val t23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
        m33 = m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33

    m00 = t00; m10 = t10; m20 = t20; m30 = t30
    m01 = t01; m11 = t11; m21 = t21; m31 = t31
    m02 = t02; m12 = t12; m22 = t22; m32 = t32
    m03 = t03; m13 = t13; m23 = t23
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat4d) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20; m30 /= m.m30
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21; m31 /= m.m31
    m02 /= m.m02; m12 /= m.m12; m22 /= m.m22; m32 /= m.m32
    m03 /= m.m03; m13 /= m.m13; m23 /= m.m23; m33 /= m.m33
  }


  def update(c: Int, r: Int, s: Double) {
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
          case 3 => m30 = s
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01 = s
          case 1 => m11 = s
          case 2 => m21 = s
          case 3 => m31 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m02 = s
          case 1 => m12 = s
          case 2 => m22 = s
          case 3 => m32 = s
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03 = s
          case 1 => m13 = s
          case 2 => m23 = s
          case 3 => m33 = s
          case _ => error
        }
      case _ => error
    }
  }

  def update(c: Int, v: inVec2d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y
      case 1 => m01 = v.x; m11 = v.y
      case 2 => m02 = v.x; m12 = v.y
      case 3 => m03 = v.x; m13 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  def update(c: Int, v: inVec3d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z
      case 2 => m02 = v.x; m12 = v.y; m22 = v.z
      case 3 => m03 = v.x; m13 = v.y; m23 = v.z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }

  def update(c: Int, v: inVec4d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
      case 2 => m02 = v.x; m12 = v.y; m22 = v.z; m32 = v.w
      case 3 => m03 = v.x; m13 = v.y; m23 = v.z; m33 = v.w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
        )
    }
  }
}

object Mat4d {
  final val Zero = ConstMat4d(0)
  final val Identity = ConstMat4d(1)

  final val Manifest = classType[Mat4d](classOf[Mat4d])
  final val ConstManifest = classType[ConstMat4d](classOf[ConstMat4d])
  final val ReadManifest = classType[ReadMat4d](classOf[ReadMat4d])

  def apply(s: Double) = new Mat4d(
    s, 0, 0, 0,
    0, s, 0, 0,
    0, 0, s, 0,
    0, 0, 0, s
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double, m20: Double, m30: Double,
    m01: Double, m11: Double, m21: Double, m31: Double,
    m02: Double, m12: Double, m22: Double, m32: Double,
    m03: Double, m13: Double, m23: Double, m33: Double
  ) = new Mat4d(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_], c2: AnyVec4[_], c3: AnyVec4[_]) = 
  new Mat4d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw,
    c2.dx, c2.dy, c2.dz, c2.dw,
    c3.dx, c3.dy, c3.dz, c3.dw
  )

  def apply(m: AnyMat[_]) = new Mat4d(
    m.d00, m.d10, m.d20, m.d30,
    m.d01, m.d11, m.d21, m.d31,
    m.d02, m.d12, m.d22, m.d32,
    m.d03, m.d13, m.d23, m.d33
  )

  def unapply(m: ReadMat4d) = Some((m(0), m(1), m(2), m(3)))
}
