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
sealed abstract class ReadMat4x3d extends ProtectedMat4x3d[Double]
with Protected with Serializable
{

  type Clone <: ReadMat4x3d
  def toConst() :ConstMat4x3d
  
  type Read = ReadMat4x3d
  type Mutable = Mat4x3d
  final def readType: Class[Read] = classOf[ReadMat4x3d]
  final def mutableCopy() = Mat4x3d(this)

  // Column major order.
  final def m00 = p00; final def m01 = p01; final def m02 = p02
  final def m10 = p10; final def m11 = p11; final def m12 = p12
  final def m20 = p20; final def m21 = p21; final def m22 = p22
  final def m30 = p30; final def m31 = p31; final def m32 = p32


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m02_=(s: Double) { throw new UnsupportedOperationException }

  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }
  protected def m12_=(s: Double) { throw new UnsupportedOperationException }

  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }
  protected def m22_=(s: Double) { throw new UnsupportedOperationException }

  protected def m30_=(s: Double) { throw new UnsupportedOperationException }
  protected def m31_=(s: Double) { throw new UnsupportedOperationException }
  protected def m32_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f01 = m01.toFloat
  private[math] final override def f02 = m02.toFloat

  private[math] final override def f10 = m10.toFloat
  private[math] final override def f11 = m11.toFloat
  private[math] final override def f12 = m12.toFloat

  private[math] final override def f20 = m20.toFloat
  private[math] final override def f21 = m21.toFloat
  private[math] final override def f22 = m22.toFloat

  private[math] final override def f30 = m30.toFloat
  private[math] final override def f31 = m31.toFloat
  private[math] final override def f32 = m32.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d01 = m01
  private[math] final override def d02 = m02

  private[math] final override def d10 = m10
  private[math] final override def d11 = m11
  private[math] final override def d12 = m12

  private[math] final override def d20 = m20
  private[math] final override def d21 = m21
  private[math] final override def d22 = m22

  private[math] final override def d30 = m30
  private[math] final override def d31 = m31
  private[math] final override def d32 = m32


  final def apply(c: Int) :ConstVec3d = {
    c match {
      case 0 => new ConstVec3d(m00, m01, m02)
      case 1 => new ConstVec3d(m10, m11, m12)
      case 2 => new ConstVec3d(m20, m21, m22)
      case 3 => new ConstVec3d(m30, m31, m32)
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10
          case 1 => m11
          case 2 => m12
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20
          case 1 => m21
          case 2 => m22
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m30
          case 1 => m31
          case 2 => m32
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat4x3d = this
  final def unary_-() = new Mat4x3d(
    -m00, -m01, -m02,
    -m10, -m11, -m12,
    -m20, -m21, -m22,
    -m30, -m31, -m32
  )
  final def *(s: Double) = new Mat4x3d(
    s*m00, s*m01, s*m02,
    s*m10, s*m11, s*m12,
    s*m20, s*m21, s*m22,
    s*m30, s*m31, s*m32
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat4x3d(
    m00 + s, m01 + s, m02 + s,
    m10 + s, m11 + s, m12 + s,
    m20 + s, m21 + s, m22 + s,
    m30 + s, m31 + s, m32 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat4x3d) = new Mat4x3d(
    m00 + m.m00, m01 + m.m01, m02 + m.m02,
    m10 + m.m10, m11 + m.m11, m12 + m.m12,
    m20 + m.m20, m21 + m.m21, m22 + m.m22,
    m30 + m.m30, m31 + m.m31, m32 + m.m32
  )
  final def -(m: inMat4x3d) = new Mat4x3d(
    m00 - m.m00, m01 - m.m01, m02 - m.m02,
    m10 - m.m10, m11 - m.m11, m12 - m.m12,
    m20 - m.m20, m21 - m.m21, m22 - m.m22,
    m30 - m.m30, m31 - m.m31, m32 - m.m32
  )

  /**
   * Component-wise division.
   */
  final def /(m: inMat4x3d) = new Mat4x3d(
    m00/m.m00, m01/m.m01, m02/m.m02,
    m10/m.m10, m11/m.m11, m12/m.m12,
    m20/m.m20, m21/m.m21, m22/m.m22,
    m30/m.m30, m31/m.m31, m32/m.m32
  )
  private[math] final def divByComp(s: Double) = new Mat4x3d(
    s/m00, s/m01, s/m02,
    s/m10, s/m11, s/m12,
    s/m20, s/m21, s/m22,
    s/m30, s/m31, s/m32
  )

  final def *(m: inMat2x4d) = new Mat2x3d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,
    m02*m.m00 + m12*m.m01 + m22*m.m02 + m32*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13,
    m02*m.m10 + m12*m.m11 + m22*m.m12 + m32*m.m13
  )
  final def *(m: inMat3x4d) = new Mat3d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,
    m02*m.m00 + m12*m.m01 + m22*m.m02 + m32*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13,
    m02*m.m10 + m12*m.m11 + m22*m.m12 + m32*m.m13,

    m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23,
    m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23,
    m02*m.m20 + m12*m.m21 + m22*m.m22 + m32*m.m23
  )
  final def *(m: inMat4d) = new Mat4x3d(
    m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03,
    m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03,
    m02*m.m00 + m12*m.m01 + m22*m.m02 + m32*m.m03,

    m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13,
    m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13,
    m02*m.m10 + m12*m.m11 + m22*m.m12 + m32*m.m13,

    m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23,
    m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23,
    m02*m.m20 + m12*m.m21 + m22*m.m22 + m32*m.m23,

    m00*m.m30 + m10*m.m31 + m20*m.m32 + m30*m.m33,
    m01*m.m30 + m11*m.m31 + m21*m.m32 + m31*m.m33,
    m02*m.m30 + m12*m.m31 + m22*m.m32 + m32*m.m33
  )

  final def *(u: inVec4d) = new Vec3d(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w,
    m02*u.x + m12*u.y + m22*u.z + m32*u.w
  )
  private[math] final def transposeMult(u: inVec3d) = new Vec4d(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z,
    m20*u.x + m21*u.y + m22*u.z,
    m30*u.x + m31*u.y + m32*u.z
  )

  final def scale(s: Double) :Mat4x3d = this*s
  final def scale(s: inVec3d) :Mat4x3d = new Mat4x3d(
    m00*s.x, m01*s.y, m02*s.z,
    m10*s.x, m11*s.y, m12*s.z,
    m20*s.x, m21*s.y, m22*s.z,
    m30*s.x, m31*s.y, m32*s.z
  )

  /** Combines current transformation with rotation. The rotation quaternion
   * is normalized first and then transformed into a rotation matrix which
   * is concatenated with the current transformation. If you want to avoid
   * normalization, use <code>concat(rotationMat(q))</code> instead.
   * @param q rotation quaternion.
   * @return a new transformation wtih the specified rotation as
   *         the last operation.
   */
  final def rotate(q: inQuat4d) :Mat4x3d = {
    val s = 2/normSquare(q)
  
    import q._
    val tb = s*b*b
    val tc = 1 - s*c*c
    val td = s*d*d
    val bc = s*b*c
    val da = s*d*a
    val bd = s*b*d
    val ca = s*c*a
    val cd = s*c*d
    val ba = s*b*a

    // Rotation matrix
    val n00 = tc - td; val n01 = bc + da;     val n02 = bd - ca
    val n10 = bc - da; val n11 = 1 - tb - td; val n12 = cd + ba
    val n20 = bd + ca; val n21 = cd - ba;     val n22 = tc - tb
    
    // Combining with rotation matrix
    new Mat4x3d(
      n00*m00 + n10*m01 + n20*m02, n01*m00 + n11*m01 + n21*m02, n02*m00 + n12*m01 + n22*m02,
      n00*m10 + n10*m11 + n20*m12, n01*m10 + n11*m11 + n21*m12, n02*m10 + n12*m11 + n22*m12,
      n00*m20 + n10*m21 + n20*m22, n01*m20 + n11*m21 + n21*m22, n02*m20 + n12*m21 + n22*m22,
      n00*m30 + n10*m31 + n20*m32, n01*m30 + n11*m31 + n21*m32, n02*m30 + n12*m31 + n22*m32
    )
  }

  final def rotateX(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      m00, cosA*m01 - sinA*m02, sinA*m01 + cosA*m02,
      m10, cosA*m11 - sinA*m12, sinA*m11 + cosA*m12,
      m20, cosA*m21 - sinA*m22, sinA*m21 + cosA*m22,
      m30, cosA*m31 - sinA*m32, sinA*m31 + cosA*m32
    )
  }
  final def rotateY(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      cosA*m00 + sinA*m02, m01, cosA*m02 - sinA*m00,
      cosA*m10 + sinA*m12, m11, cosA*m12 - sinA*m10,
      cosA*m20 + sinA*m22, m21, cosA*m22 - sinA*m20,
      cosA*m30 + sinA*m32, m31, cosA*m32 - sinA*m30
    )
  }
  final def rotateZ(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      cosA*m00 - sinA*m01, sinA*m00 + cosA*m01, m02,
      cosA*m10 - sinA*m11, sinA*m10 + cosA*m11, m12,
      cosA*m20 - sinA*m21, sinA*m20 + cosA*m21, m22,
      cosA*m30 - sinA*m31, sinA*m30 + cosA*m31, m32
    )
  }

  final def translate(u: inVec3d) :Mat4x3d = new Mat4x3d(
    m00, m01, m02,
    m10, m11, m12,
    m20, m21, m22,
    m30 + u.x, m31 + u.y, m32 + u.z
  )

  final def concat(m: inMat4x3d) :Mat4x3d = new Mat4x3d(
    m.m00*m00 + m.m10*m01 + m.m20*m02,
    m.m01*m00 + m.m11*m01 + m.m21*m02,
    m.m02*m00 + m.m12*m01 + m.m22*m02,

    m.m00*m10 + m.m10*m11 + m.m20*m12,
    m.m01*m10 + m.m11*m11 + m.m21*m12,
    m.m02*m10 + m.m12*m11 + m.m22*m12,

    m.m00*m20 + m.m10*m21 + m.m20*m22,
    m.m01*m20 + m.m11*m21 + m.m21*m22,
    m.m02*m20 + m.m12*m21 + m.m22*m22,

    m.m00*m30 + m.m10*m31 + m.m20*m32 + m.m30,
    m.m01*m30 + m.m11*m31 + m.m21*m32 + m.m31,
    m.m02*m30 + m.m12*m31 + m.m22*m32 + m.m32
  )
  final def concat(m: inMat3d) :Mat4x3d = m*this

  final def transformPoint(p: inVec3d) :Vec3d = new Vec3d(
    m00*p.x + m10*p.y + m20*p.z + m30,
    m01*p.x + m11*p.y + m21*p.z + m31,
    m02*p.x + m12*p.y + m22*p.z + m32
  )
  final def transformVector(v: inVec3d) :Vec3d = new Vec3d(
    m00*v.x + m10*v.y + m20*v.z,
    m01*v.x + m11*v.y + m21*v.z,
    m02*v.x + m12*v.y + m22*v.z
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat4x3[_] =>
        d00 == m.d00 && d01 == m.d01 && d02 == m.d02 &&
        d10 == m.d10 && d11 == m.d11 && d12 == m.d12 &&
        d20 == m.d20 && d21 == m.d21 && d22 == m.d22 &&
        d30 == m.d30 && d31 == m.d31 && d32 == m.d32
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
                    ) + m10.hashCode
                  ) + m11.hashCode
                ) + m12.hashCode
              ) + m20.hashCode
            ) + m21.hashCode
          ) + m22.hashCode
        ) + m30.hashCode
      ) + m31.hashCode
    ) + m32.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat4x3" +
    "(" +
      m00 + ", " + m01 + ", " + m02 + ",   " + 
      m10 + ", " + m11 + ", " + m12 + ",   " + 
      m20 + ", " + m21 + ", " + m22 + ",   " + 
      m30 + ", " + m31 + ", " + m32 +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat4x3d private[math] (
  c00: Double, c01: Double, c02: Double,
  c10: Double, c11: Double, c12: Double,
  c20: Double, c21: Double, c22: Double,
  c30: Double, c31: Double, c32: Double
) extends ReadMat4x3d with Immutable with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12
  p20 = c20; p21 = c21; p22 = c22
  p30 = c30; p31 = c31; p32 = c32


  type Clone = ConstMat4x3d
  override def clone() = this
  def toConst() = this
}

object ConstMat4x3d {
  def apply(s: Double) = new ConstMat4x3d(
    s, 0, 0,
    0, s, 0,
    0, 0, s,
    0, 0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double, m02: Double,
    m10: Double, m11: Double, m12: Double,
    m20: Double, m21: Double, m22: Double,
    m30: Double, m31: Double, m32: Double
  ) = new ConstMat4x3d(
    m00, m01, m02,
    m10, m11, m12,
    m20, m21, m22,
    m30, m31, m32
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_], c3: AnyVec3[_]) = 
  new ConstMat4x3d(
    c0.dx, c0.dy, c0.dz,
    c1.dx, c1.dy, c1.dz,
    c2.dx, c2.dy, c2.dz,
    c3.dx, c3.dy, c3.dz
  )

  def apply(m: AnyMat[_]) = new ConstMat4x3d(
    m.d00, m.d01, m.d02,
    m.d10, m.d11, m.d12,
    m.d20, m.d21, m.d22,
    m.d30, m.d31, m.d32
  )

  implicit def toConst(m: ReadMat4x3d) = ConstMat4x3d(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat4x3d private[math] (
  c00: Double, c01: Double, c02: Double,
  c10: Double, c11: Double, c12: Double,
  c20: Double, c21: Double, c22: Double,
  c30: Double, c31: Double, c32: Double
)
extends ReadMat4x3d with Accessor with CompositeFormat
with Accessible with Serializable
{
  p00 = c00; p01 = c01; p02 = c02
  p10 = c10; p11 = c11; p12 = c12
  p20 = c20; p21 = c21; p22 = c22
  p30 = c30; p31 = c31; p32 = c32

  private[math] def this() = this(
    1, 0, 0,
    0, 1, 0,
    0, 0, 1,
    0, 0, 0
  )
  
  type Clone = Mat4x3d
  type Const = ConstMat4x3d

  type Accessor = Mat4x3d
  type Component = RDouble

  override def clone() = Mat4x3d(this)
  def toConst() = ConstMat4x3d(this)

  def :=(m: inMat4x3d) {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
    m20 = m.m20; m21 = m.m21; m22 = m.m22
    m30 = m.m30; m31 = m.m31; m32 = m.m32
  }

  def :=(m: inMat3d) {
    m00 = m.m00; m01 = m.m01; m02 = m.m02
    m10 = m.m10; m11 = m.m11; m12 = m.m12
    m20 = m.m20; m21 = m.m21; m22 = m.m22
  }


  override def m00_=(s: Double) { p00 = s }
  override def m01_=(s: Double) { p01 = s }
  override def m02_=(s: Double) { p02 = s }

  override def m10_=(s: Double) { p10 = s }
  override def m11_=(s: Double) { p11 = s }
  override def m12_=(s: Double) { p12 = s }

  override def m20_=(s: Double) { p20 = s }
  override def m21_=(s: Double) { p21 = s }
  override def m22_=(s: Double) { p22 = s }

  override def m30_=(s: Double) { p30 = s }
  override def m31_=(s: Double) { p31 = s }
  override def m32_=(s: Double) { p32 = s }


  def *=(s: Double) {
    m00 *= s; m01 *= s; m02 *= s;
    m10 *= s; m11 *= s; m12 *= s;
    m20 *= s; m21 *= s; m22 *= s;
    m30 *= s; m31 *= s; m32 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m01 += s; m02 += s
    m10 += s; m11 += s; m12 += s
    m20 += s; m21 += s; m22 += s
    m30 += s; m31 += s; m32 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat4x3d) {
    m00 += m.m00; m01 += m.m01; m02 += m.m02;
    m10 += m.m10; m11 += m.m11; m12 += m.m12;
    m20 += m.m20; m21 += m.m21; m22 += m.m22;
    m30 += m.m30; m31 += m.m31; m32 += m.m32
  }
  def -=(m: inMat4x3d) {
    m00 -= m.m00; m01 -= m.m01; m02 -= m.m02;
    m10 -= m.m10; m11 -= m.m11; m12 -= m.m12;
    m20 -= m.m20; m21 -= m.m21; m22 -= m.m22;
    m30 -= m.m30; m31 -= m.m31; m32 -= m.m32
  }

  def *=(m: inMat4d) {
    val t00 = m00*m.m00 + m10*m.m01 + m20*m.m02 + m30*m.m03
    val t01 = m01*m.m00 + m11*m.m01 + m21*m.m02 + m31*m.m03
    val t02 = m02*m.m00 + m12*m.m01 + m22*m.m02 + m32*m.m03

    val t10 = m00*m.m10 + m10*m.m11 + m20*m.m12 + m30*m.m13
    val t11 = m01*m.m10 + m11*m.m11 + m21*m.m12 + m31*m.m13
    val t12 = m02*m.m10 + m12*m.m11 + m22*m.m12 + m32*m.m13

    val t20 = m00*m.m20 + m10*m.m21 + m20*m.m22 + m30*m.m23
    val t21 = m01*m.m20 + m11*m.m21 + m21*m.m22 + m31*m.m23
    val t22 = m02*m.m20 + m12*m.m21 + m22*m.m22 + m32*m.m23

    val t30 = m00*m.m30 + m10*m.m31 + m20*m.m32 + m30*m.m33
    val t31 = m01*m.m30 + m11*m.m31 + m21*m.m32 + m31*m.m33
        m32 = m02*m.m30 + m12*m.m31 + m22*m.m32 + m32*m.m33

    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11; m12 = t12
    m20 = t20; m21 = t21; m22 = t22
    m30 = t30; m31 = t31
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat4x3d) {
    m00 /= m.m00; m01 /= m.m01; m02 /= m.m02
    m10 /= m.m10; m11 /= m.m11; m12 /= m.m12
    m20 /= m.m20; m21 /= m.m21; m22 /= m.m22
    m30 /= m.m30; m31 /= m.m31; m32 /= m.m32
  }

  final def applyScale(s: Double) { this *= s }
  final def applyScale(s: inVec3d) {
    m00 *= s.x; m01 *= s.y; m02 *= s.z
    m10 *= s.x; m11 *= s.y; m12 *= s.z
    m20 *= s.x; m21 *= s.y; m22 *= s.z
    m30 *= s.x; m31 *= s.y; m32 *= s.z
  }

  /** Appends rotation to the current transformation. The rotation quaternion
   * is normalized first and then transformed into a rotation matrix which
   * is concatenated with the current transformation. If you want to avoid
   * normalization, use <code>applyTransformation(rotationMat(q))</code> instead.
   * @param q rotation quaternion.
   */
  final def applyRotation(q: inQuat4d) {
    val s = 2/normSquare(q)
  
    import q._
    val tb = s*b*b
    val tc = 1 - s*c*c
    val td = s*d*d
    val bc = s*b*c
    val da = s*d*a
    val bd = s*b*d
    val ca = s*c*a
    val cd = s*c*d
    val ba = s*b*a

    // Rotation matrix
    val n00 = tc - td; val n01 = bc + da;     val n02 = bd - ca
    val n10 = bc - da; val n11 = 1 - tb - td; val n12 = cd + ba
    val n20 = bd + ca; val n21 = cd - ba;     val n22 = tc - tb
    
    // Combining with rotation matrix
    val t00 = n00*m00 + n10*m01 + n20*m02
    val t01 = n01*m00 + n11*m01 + n21*m02
    val t02 = n02*m00 + n12*m01 + n22*m02

    val t10 = n00*m10 + n10*m11 + n20*m12
    val t11 = n01*m10 + n11*m11 + n21*m12
    val t12 = n02*m10 + n12*m11 + n22*m12

    val t20 = n00*m20 + n10*m21 + n20*m22
    val t21 = n01*m20 + n11*m21 + n21*m22
    val t22 = n02*m20 + n12*m21 + n22*m22

    val t30 = n00*m30 + n10*m31 + n20*m32
    val t31 = n01*m30 + n11*m31 + n21*m32
        m32 = n02*m30 + n12*m31 + n22*m32
    
    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11; m12 = t12
    m20 = t20; m21 = t21; m22 = t22
    m30 = t30; m31 = t31
  }

  final def applyRotationX(angle: Double) {
    val sinA = sin(angle)
    val cosA = cos(angle)

    val t01 = cosA*m01 - sinA*m02; val t02 = sinA*m01 + cosA*m02
    val t11 = cosA*m11 - sinA*m12; val t12 = sinA*m11 + cosA*m12
    val t21 = cosA*m21 - sinA*m22; val t22 = sinA*m21 + cosA*m22
    val t31 = cosA*m31 - sinA*m32;     m32 = sinA*m31 + cosA*m32
    
    m01 = t01; m02 = t02
    m11 = t11; m12 = t12
    m21 = t21; m22 = t22
    m31 = t31
  }
  final def applyRotationY(angle: Double) {
    val sinA = sin(angle)
    val cosA = cos(angle)

    val t00 = cosA*m00 + sinA*m02; val t02 = cosA*m02 - sinA*m00
    val t10 = cosA*m10 + sinA*m12; val t12 = cosA*m12 - sinA*m10
    val t20 = cosA*m20 + sinA*m22; val t22 = cosA*m22 - sinA*m20
    val t30 = cosA*m30 + sinA*m32;     m32 = cosA*m32 - sinA*m30
    
    m00 = t00; m02 = t02
    m10 = t10; m12 = t12
    m20 = t20; m22 = t22
    m30 = t30
  }
  final def applyRotationZ(angle: Double) {
    val sinA = sin(angle)
    val cosA = cos(angle)
    
    val t00 = cosA*m00 - sinA*m01; val t01 = sinA*m00 + cosA*m01
    val t10 = cosA*m10 - sinA*m11; val t11 = sinA*m10 + cosA*m11
    val t20 = cosA*m20 - sinA*m21; val t21 = sinA*m20 + cosA*m21
    val t30 = cosA*m30 - sinA*m31;     m31 = sinA*m30 + cosA*m31
    
    m00 = t00; m01 = t01
    m10 = t10; m11 = t11
    m20 = t20; m21 = t21
    m30 = t30
  }

  final def applyTranslation(u: inVec3d) {
    m30 += u.x; m31 += u.y; m32 += u.z
  }

  final def applyTransformation(m: inMat4x3d) {
    val t00 = m.m00*m00 + m.m10*m01 + m.m20*m02
    val t01 = m.m01*m00 + m.m11*m01 + m.m21*m02
    val t02 = m.m02*m00 + m.m12*m01 + m.m22*m02

    val t10 = m.m00*m10 + m.m10*m11 + m.m20*m12
    val t11 = m.m01*m10 + m.m11*m11 + m.m21*m12
    val t12 = m.m02*m10 + m.m12*m11 + m.m22*m12

    val t20 = m.m00*m20 + m.m10*m21 + m.m20*m22
    val t21 = m.m01*m20 + m.m11*m21 + m.m21*m22
    val t22 = m.m02*m20 + m.m12*m21 + m.m22*m22

    val t30 = m.m00*m30 + m.m10*m31 + m.m20*m32 + m.m30
    val t31 = m.m01*m30 + m.m11*m31 + m.m21*m32 + m.m31
        m32 = m.m02*m30 + m.m12*m31 + m.m22*m32 + m.m32
    
    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11; m12 = t12
    m20 = t20; m21 = t21; m22 = t22
    m30 = t30; m31 = t31
  }
  final def applyTransformation(m: inMat3d) {
    val t00 = m.m00*m00 + m.m10*m01 + m.m20*m02
    val t01 = m.m01*m00 + m.m11*m01 + m.m21*m02
    val t02 = m.m02*m00 + m.m12*m01 + m.m22*m02

    val t10 = m.m00*m10 + m.m10*m11 + m.m20*m12
    val t11 = m.m01*m10 + m.m11*m11 + m.m21*m12
    val t12 = m.m02*m10 + m.m12*m11 + m.m22*m12

    val t20 = m.m00*m20 + m.m10*m21 + m.m20*m22
    val t21 = m.m01*m20 + m.m11*m21 + m.m21*m22
    val t22 = m.m02*m20 + m.m12*m21 + m.m22*m22

    val t30 = m.m00*m30 + m.m10*m31 + m.m20*m32
    val t31 = m.m01*m30 + m.m11*m31 + m.m21*m32
        m32 = m.m02*m30 + m.m12*m31 + m.m22*m32
    
    m00 = t00; m01 = t01; m02 = t02
    m10 = t10; m11 = t11; m12 = t12
    m20 = t20; m21 = t21; m22 = t22
    m30 = t30; m31 = t31
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m10 = s
          case 1 => m11 = s
          case 2 => m12 = s
          case _ => error
        }
      case 2 =>
        r match {
          case 0 => m20 = s
          case 1 => m21 = s
          case 2 => m22 = s
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m30 = s
          case 1 => m31 = s
          case 2 => m32 = s
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

  def update(c: Int, v: inVec3d) {
    c match {
      case 0 => m00 = v.x; m01 = v.y; m02 = v.z
      case 1 => m10 = v.x; m11 = v.y; m12 = v.z
      case 2 => m20 = v.x; m21 = v.y; m22 = v.z
      case 3 => m30 = v.x; m31 = v.y; m32 = v.z
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat4x3d {
  final val Zero = ConstMat4x3d(0)
  final val Identity = ConstMat4x3d(1)

  final val Manifest = classType[Mat4x3d](classOf[Mat4x3d])
  final val ConstManifest = classType[ConstMat4x3d](classOf[ConstMat4x3d])
  final val ReadManifest = classType[ReadMat4x3d](classOf[ReadMat4x3d])

  def apply(s: Double) = new Mat4x3d(
    s, 0, 0,
    0, s, 0,
    0, 0, s,
    0, 0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m01: Double, m02: Double,
    m10: Double, m11: Double, m12: Double,
    m20: Double, m21: Double, m22: Double,
    m30: Double, m31: Double, m32: Double
  ) = new Mat4x3d(
    m00, m01, m02,
    m10, m11, m12,
    m20, m21, m22,
    m30, m31, m32
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_], c3: AnyVec3[_]) = 
  new Mat4x3d(
    c0.dx, c0.dy, c0.dz,
    c1.dx, c1.dy, c1.dz,
    c2.dx, c2.dy, c2.dz,
    c3.dx, c3.dy, c3.dz
  )

  def apply(m: AnyMat[_]) = new Mat4x3d(
    m.d00, m.d01, m.d02,
    m.d10, m.d11, m.d12,
    m.d20, m.d21, m.d22,
    m.d30, m.d31, m.d32
  )

  def unapply(m: ReadMat4x3d) = Some((m(0), m(1), m(2), m(3)))

  def scale(s: Double) :Mat4x3d = Mat4x3d(s)
  def scale(s: inVec3d) :Mat4x3d = {
    val m = Mat4x3d(s.x)
    m.m11 = s.y
    m.m22 = s.z
    m
  }

  def rotate(q: inQuat4d) :Mat4x3d = {
    val s = 2/normSquare(q)
  
    import q._
    val tb = s*b*b
    val tc = 1 - s*c*c
    val td = s*d*d
    val bc = s*b*c
    val da = s*d*a
    val bd = s*b*d
    val ca = s*c*a
    val cd = s*c*d
    val ba = s*b*a

    new Mat4x3d(
      tc - td, bc + da, bd - ca,
      bc - da, 1 - tb - td, cd + ba,
      bd + ca, cd - ba, tc - tb,
      0, 0, 0
    )
  }

  def rotateX(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      1, 0, 0,
      0, cosA, sinA,
      0, -sinA, cosA,
      0, 0, 0
    )
  }
  def rotateY(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      cosA, 0, -sinA,
      0, 1, 0,
      sinA, 0, cosA,
      0, 0, 0
    )
  }
  def rotateZ(angle: Double) :Mat4x3d = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat4x3d(
      cosA, sinA, 0,
      -sinA, cosA, 0,
      0, 0, 1,
      0, 0, 0
    )
  }

  def translate(u: inVec3d) :Mat4x3d = {
    val m = Mat4x3d(1)
    m(3) = u
    m
  }

  def concat(m: inMat4x3d) :Mat4x3d = Mat4x3d(m)
  def concat(m: inMat3d) :Mat4x3d = Mat4x3d(m)
}
