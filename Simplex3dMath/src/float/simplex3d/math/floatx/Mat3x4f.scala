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
sealed abstract class ReadMat3x4f extends ProtectedMat3x4f[Float]
with ReadPropertyValue[Mat3x4f] with Cloneable[ReadMat3x4f] with Serializable
{

  def toConst() :ConstMat3x4f
  final def mutableCopy() = Mat3x4f(this)

  // Column major order.
  final def m00 = p00; final def m10 = p10; final def m20 = p20
  final def m01 = p01; final def m11 = p11; final def m21 = p21
  final def m02 = p02; final def m12 = p12; final def m22 = p22
  final def m03 = p03; final def m13 = p13; final def m23 = p23


  protected def m00_=(s: Float) { throw new UnsupportedOperationException }
  protected def m10_=(s: Float) { throw new UnsupportedOperationException }
  protected def m20_=(s: Float) { throw new UnsupportedOperationException }

  protected def m01_=(s: Float) { throw new UnsupportedOperationException }
  protected def m11_=(s: Float) { throw new UnsupportedOperationException }
  protected def m21_=(s: Float) { throw new UnsupportedOperationException }

  protected def m02_=(s: Float) { throw new UnsupportedOperationException }
  protected def m12_=(s: Float) { throw new UnsupportedOperationException }
  protected def m22_=(s: Float) { throw new UnsupportedOperationException }

  protected def m03_=(s: Float) { throw new UnsupportedOperationException }
  protected def m13_=(s: Float) { throw new UnsupportedOperationException }
  protected def m23_=(s: Float) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00
  private[math] final override def f10 = m10
  private[math] final override def f20 = m20

  private[math] final override def f01 = m01
  private[math] final override def f11 = m11
  private[math] final override def f21 = m21

  private[math] final override def f02 = m02
  private[math] final override def f12 = m12
  private[math] final override def f22 = m22

  private[math] final override def f03 = m03
  private[math] final override def f13 = m13
  private[math] final override def f23 = m23


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12
  private[math] final override def d22 = m22

  private[math] final override def d03 = m03
  private[math] final override def d13 = m13
  private[math] final override def d23 = m23


  final def apply(c: Int) :ConstVec3f = {
    c match {
      case 0 => new ConstVec3f(m00, m10, m20)
      case 1 => new ConstVec3f(m01, m11, m21)
      case 2 => new ConstVec3f(m02, m12, m22)
      case 3 => new ConstVec3f(m03, m13, m23)
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
      case 2 =>
        r match {
          case 0 => m02
          case 1 => m12
          case 2 => m22
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03
          case 1 => m13
          case 2 => m23
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat3x4f = this
  final def unary_-() = new Mat3x4f(
    -m00, -m10, -m20,
    -m01, -m11, -m21,
    -m02, -m12, -m22,
    -m03, -m13, -m23
  )
  final def *(s: Float) = new Mat3x4f(
    s*m00, s*m10, s*m20,
    s*m01, s*m11, s*m21,
    s*m02, s*m12, s*m22,
    s*m03, s*m13, s*m23
  )
  final def /(s: Float) = this * (1/s)

  final def +(s: Float) = new Mat3x4f(
    m00 + s, m10 + s, m20 + s,
    m01 + s, m11 + s, m21 + s,
    m02 + s, m12 + s, m22 + s,
    m03 + s, m13 + s, m23 + s
  )
  final def -(s: Float) = this + (-s)

  final def +(m: inMat3x4f) = new Mat3x4f(
    m00 + m.m00, m10 + m.m10, m20 + m.m20,
    m01 + m.m01, m11 + m.m11, m21 + m.m21,
    m02 + m.m02, m12 + m.m12, m22 + m.m22,
    m03 + m.m03, m13 + m.m13, m23 + m.m23
  )
  final def -(m: inMat3x4f) = new Mat3x4f(
    m00 - m.m00, m10 - m.m10, m20 - m.m20,
    m01 - m.m01, m11 - m.m11, m21 - m.m21,
    m02 - m.m02, m12 - m.m12, m22 - m.m22,
    m03 - m.m03, m13 - m.m13, m23 - m.m23
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat3x4f) = new Mat3x4f(
    m00/m.m00, m10/m.m10, m20/m.m20,
    m01/m.m01, m11/m.m11, m21/m.m21,
    m02/m.m02, m12/m.m12, m22/m.m22,
    m03/m.m03, m13/m.m13, m23/m.m23
  )
  private[math] final def divByComp(s: Float) = new Mat3x4f(
    s/m00, s/m10, s/m20,
    s/m01, s/m11, s/m21,
    s/m02, s/m12, s/m22,
    s/m03, s/m13, s/m23
  )

  final def *(m: inMat4x2f) = new Mat3x2f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
  )
  final def *(m: inMat4x3f) = new Mat3f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
    m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
  )
  final def *(m: inMat4f) = new Mat3x4f(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
    m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
    m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
    m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32,

    m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
    m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33,
    m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
  )

  final def *(u: inVec4f) = new Vec3f(
    m00*u.x + m01*u.y + m02*u.z + m03*u.w,
    m10*u.x + m11*u.y + m12*u.z + m13*u.w,
    m20*u.x + m21*u.y + m22*u.z + m23*u.w
  )
  private[math] final def transposeMult(u: inVec3f) = new Vec4f(
    m00*u.x + m10*u.y + m20*u.z,
    m01*u.x + m11*u.y + m21*u.z,
    m02*u.x + m12*u.y + m22*u.z,
    m03*u.x + m13*u.y + m23*u.z
  )

  final def scale(s: Float) :Mat3x4f = this*s
  final def scale(s: inVec3f) :Mat3x4f = new Mat3x4f(
    m00*s.x, m10*s.y, m20*s.z,
    m01*s.x, m11*s.y, m21*s.z,
    m02*s.x, m12*s.y, m22*s.z,
    m03*s.x, m13*s.y, m23*s.z
  )

  /** Combines current transformation with rotation. The rotation quaternion
   * is normalized first and then transformed into a rotation matrix which
   * is concatenated with the current transformation. If you want to avoid
   * normalization, use <code>concat(rotationMat(q))</code> instead.
   * @param q rotation quaternion.
   * @return a new transformation wtih the specified rotation as
   *         the last operation.
   */
  final def rotate(q: inQuat4f) :Mat3x4f = {
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
    val n00 = tc - td; val n10 = bc + da;     val n20 = bd - ca
    val n01 = bc - da; val n11 = 1 - tb - td; val n21 = cd + ba
    val n02 = bd + ca; val n12 = cd - ba;     val n22 = tc - tb
    
    // Combining with rotation matrix
    new Mat3x4f(
      n00*m00 + n01*m10 + n02*m20, n10*m00 + n11*m10 + n12*m20, n20*m00 + n21*m10 + n22*m20,
      n00*m01 + n01*m11 + n02*m21, n10*m01 + n11*m11 + n12*m21, n20*m01 + n21*m11 + n22*m21,
      n00*m02 + n01*m12 + n02*m22, n10*m02 + n11*m12 + n12*m22, n20*m02 + n21*m12 + n22*m22,
      n00*m03 + n01*m13 + n02*m23, n10*m03 + n11*m13 + n12*m23, n20*m03 + n21*m13 + n22*m23
    )
  }

  final def rotateX(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      m00, cosA*m10 - sinA*m20, sinA*m10 + cosA*m20,
      m01, cosA*m11 - sinA*m21, sinA*m11 + cosA*m21,
      m02, cosA*m12 - sinA*m22, sinA*m12 + cosA*m22,
      m03, cosA*m13 - sinA*m23, sinA*m13 + cosA*m23
    )
  }
  final def rotateY(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      cosA*m00 + sinA*m20, m10, cosA*m20 - sinA*m00,
      cosA*m01 + sinA*m21, m11, cosA*m21 - sinA*m01,
      cosA*m02 + sinA*m22, m12, cosA*m22 - sinA*m02,
      cosA*m03 + sinA*m23, m13, cosA*m23 - sinA*m03
    )
  }
  final def rotateZ(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      cosA*m00 - sinA*m10, sinA*m00 + cosA*m10, m20,
      cosA*m01 - sinA*m11, sinA*m01 + cosA*m11, m21,
      cosA*m02 - sinA*m12, sinA*m02 + cosA*m12, m22,
      cosA*m03 - sinA*m13, sinA*m03 + cosA*m13, m23
    )
  }

  final def translate(u: inVec3f) :Mat3x4f = new Mat3x4f(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22,
    m03 + u.x, m13 + u.y, m23 + u.z
  )

  final def concat(m: inMat3x4f) :Mat3x4f = new Mat3x4f(
    m.m00*m00 + m.m01*m10 + m.m02*m20,
    m.m10*m00 + m.m11*m10 + m.m12*m20,
    m.m20*m00 + m.m21*m10 + m.m22*m20,

    m.m00*m01 + m.m01*m11 + m.m02*m21,
    m.m10*m01 + m.m11*m11 + m.m12*m21,
    m.m20*m01 + m.m21*m11 + m.m22*m21,

    m.m00*m02 + m.m01*m12 + m.m02*m22,
    m.m10*m02 + m.m11*m12 + m.m12*m22,
    m.m20*m02 + m.m21*m12 + m.m22*m22,

    m.m00*m03 + m.m01*m13 + m.m02*m23 + m.m03,
    m.m10*m03 + m.m11*m13 + m.m12*m23 + m.m13,
    m.m20*m03 + m.m21*m13 + m.m22*m23 + m.m23
  )
  final def concat(m: inMat3f) :Mat3x4f = m*this

  final def transformPoint(p: inVec3f) :Vec3f = new Vec3f(
    m00*p.x + m01*p.y + m02*p.z + m03,
    m10*p.x + m11*p.y + m12*p.z + m13,
    m20*p.x + m21*p.y + m22*p.z + m23
  )
  final def transformVector(v: inVec3f) :Vec3f = new Vec3f(
    m00*v.x + m01*v.y + m02*v.z,
    m10*v.x + m11*v.y + m12*v.z,
    m20*v.x + m21*v.y + m22*v.z
  )


  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat3x4[_] =>
        d00 == m.d00 && d10 == m.d10 && d20 == m.d20 &&
        d01 == m.d01 && d11 == m.d11 && d21 == m.d21 &&
        d02 == m.d02 && d12 == m.d12 && d22 == m.d22 &&
        d03 == m.d03 && d13 == m.d13 && d23 == m.d23
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
                        ) + m10.hashCode
                      ) + m20.hashCode
                    ) + m01.hashCode
                  ) + m11.hashCode
                ) + m21.hashCode
              ) + m02.hashCode
            ) + m12.hashCode
          ) + m22.hashCode
        ) + m03.hashCode
      ) + m13.hashCode
    ) + m23.hashCode
  }

  final override def toString() :String = {
    val prefix = this match {
      case self: Immutable => "Const"
      case _ => ""
    }
    prefix + "Mat3x4" +
    "(" +
      m00 + "f, " + m10 + "f, " + m20 + "f,   " + 
      m01 + "f, " + m11 + "f, " + m21 + "f,   " + 
      m02 + "f, " + m12 + "f, " + m22 + "f,   " + 
      m03 + "f, " + m13 + "f, " + m23 + "f" +
    ")"
  }
}


@SerialVersionUID(8104346712419693669L)
final class ConstMat3x4f private[math] (
  c00: Float, c10: Float, c20: Float,
  c01: Float, c11: Float, c21: Float,
  c02: Float, c12: Float, c22: Float,
  c03: Float, c13: Float, c23: Float
) extends ReadMat3x4f with Immutable with Cloneable[ConstMat3x4f] with Serializable
{
  p00 = c00; p10 = c10; p20 = c20
  p01 = c01; p11 = c11; p21 = c21
  p02 = c02; p12 = c12; p22 = c22
  p03 = c03; p13 = c13; p23 = c23

  override def clone() = this
  def toConst() = this
}

object ConstMat3x4f {
  def apply(s: Float) = new ConstMat3x4f(
    s, 0, 0,
    0, s, 0,
    0, 0, s,
    0, 0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float,
    m02: Float, m12: Float, m22: Float,
    m03: Float, m13: Float, m23: Float
  ) = new ConstMat3x4f(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22,
    m03, m13, m23
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_], c3: AnyVec3[_]) = 
  new ConstMat3x4f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz,
    c2.fx, c2.fy, c2.fz,
    c3.fx, c3.fy, c3.fz
  )

  def apply(m: AnyMat[_]) = new ConstMat3x4f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21,
    m.f02, m.f12, m.f22,
    m.f03, m.f13, m.f23
  )

  implicit def toConst(m: ReadMat3x4f) = ConstMat3x4f(m)
}


@SerialVersionUID(8104346712419693669L)
final class Mat3x4f private[math] (
  c00: Float, c10: Float, c20: Float,
  c01: Float, c11: Float, c21: Float,
  c02: Float, c12: Float, c22: Float,
  c03: Float, c13: Float, c23: Float
)
extends ReadMat3x4f with Accessor with CompositeFormat
with PropertyValue[Mat3x4f] with Cloneable[Mat3x4f] with Serializable
{
  p00 = c00; p10 = c10; p20 = c20
  p01 = c01; p11 = c11; p21 = c21
  p02 = c02; p12 = c12; p22 = c22
  p03 = c03; p13 = c13; p23 = c23

  type Read = ReadMat3x4f
  type Const = ConstMat3x4f

  type Accessor = Mat3x4f
  type Component = RFloat

  override def clone() = Mat3x4f(this)
  def toConst() = ConstMat3x4f(this)
  def :=(u: ConstMat3x4f) { this := u.asInstanceOf[inMat3x4f] }
  
  def :=(r: Readable[Mat3x4f]) {
    val m = r.asInstanceOf[ReadMat3x4f]
    m00 = m.m00; m10 = m.m10; m20 = m.m20
    m01 = m.m01; m11 = m.m11; m21 = m.m21
    m02 = m.m02; m12 = m.m12; m22 = m.m22
    m03 = m.m03; m13 = m.m13; m23 = m.m23
  }

  
  override def m00_=(s: Float) { p00 = s }
  override def m10_=(s: Float) { p10 = s }
  override def m20_=(s: Float) { p20 = s }

  override def m01_=(s: Float) { p01 = s }
  override def m11_=(s: Float) { p11 = s }
  override def m21_=(s: Float) { p21 = s }

  override def m02_=(s: Float) { p02 = s }
  override def m12_=(s: Float) { p12 = s }
  override def m22_=(s: Float) { p22 = s }

  override def m03_=(s: Float) { p03 = s }
  override def m13_=(s: Float) { p13 = s }
  override def m23_=(s: Float) { p23 = s }


  def *=(s: Float) {
    m00 *= s; m10 *= s; m20 *= s;
    m01 *= s; m11 *= s; m21 *= s;
    m02 *= s; m12 *= s; m22 *= s;
    m03 *= s; m13 *= s; m23 *= s
  }
  def /=(s: Float) { this *= (1/s) }

  def +=(s: Float) {
    m00 += s; m10 += s; m20 += s
    m01 += s; m11 += s; m21 += s
    m02 += s; m12 += s; m22 += s
    m03 += s; m13 += s; m23 += s
  }
  def -=(s: Float) { this += (-s) }

  def +=(m: inMat3x4f) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20;
    m01 += m.m01; m11 += m.m11; m21 += m.m21;
    m02 += m.m02; m12 += m.m12; m22 += m.m22;
    m03 += m.m03; m13 += m.m13; m23 += m.m23
  }
  def -=(m: inMat3x4f) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21;
    m02 -= m.m02; m12 -= m.m12; m22 -= m.m22;
    m03 -= m.m03; m13 -= m.m13; m23 -= m.m23
  }

  def *=(m: inMat4f) {
    val t00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
    val t10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
    val t20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30

    val t01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
    val t11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
    val t21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31

    val t02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
    val t12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
    val t22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32

    val t03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
    val t13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        m23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33

    m00 = t00; m10 = t10; m20 = t20
    m01 = t01; m11 = t11; m21 = t21
    m02 = t02; m12 = t12; m22 = t22
    m03 = t03; m13 = t13
  }
  /**
   * Component-wise division.
   */
  def /=(m: inMat3x4f) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21
    m02 /= m.m02; m12 /= m.m12; m22 /= m.m22
    m03 /= m.m03; m13 /= m.m13; m23 /= m.m23
  }

  final def applyScale(s: Float) { this *= s }
  final def applyScale(s: inVec3f) {
    m00 *= s.x; m10 *= s.y; m20 *= s.z
    m01 *= s.x; m11 *= s.y; m21 *= s.z
    m02 *= s.x; m12 *= s.y; m22 *= s.z
    m03 *= s.x; m13 *= s.y; m23 *= s.z
  }

  /** Appends rotation to the current transformation. The rotation quaternion
   * is normalized first and then transformed into a rotation matrix which
   * is concatenated with the current transformation. If you want to avoid
   * normalization, use <code>applyTransformation(rotationMat(q))</code> instead.
   * @param q rotation quaternion.
   */
  final def applyRotation(q: inQuat4f) {
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
    val n00 = tc - td; val n10 = bc + da;     val n20 = bd - ca
    val n01 = bc - da; val n11 = 1 - tb - td; val n21 = cd + ba
    val n02 = bd + ca; val n12 = cd - ba;     val n22 = tc - tb
    
    // Combining with rotation matrix
    val t00 = n00*m00 + n01*m10 + n02*m20
    val t10 = n10*m00 + n11*m10 + n12*m20
    val t20 = n20*m00 + n21*m10 + n22*m20

    val t01 = n00*m01 + n01*m11 + n02*m21
    val t11 = n10*m01 + n11*m11 + n12*m21
    val t21 = n20*m01 + n21*m11 + n22*m21

    val t02 = n00*m02 + n01*m12 + n02*m22
    val t12 = n10*m02 + n11*m12 + n12*m22
    val t22 = n20*m02 + n21*m12 + n22*m22

    val t03 = n00*m03 + n01*m13 + n02*m23
    val t13 = n10*m03 + n11*m13 + n12*m23
        m23 = n20*m03 + n21*m13 + n22*m23
    
    m00 = t00; m10 = t10; m20 = t20
    m01 = t01; m11 = t11; m21 = t21
    m02 = t02; m12 = t12; m22 = t22
    m03 = t03; m13 = t13
  }

  final def applyRotationX(angle: Float) {
    val sinA = sin(angle)
    val cosA = cos(angle)

    val t10 = cosA*m10 - sinA*m20; val t20 = sinA*m10 + cosA*m20
    val t11 = cosA*m11 - sinA*m21; val t21 = sinA*m11 + cosA*m21
    val t12 = cosA*m12 - sinA*m22; val t22 = sinA*m12 + cosA*m22
    val t13 = cosA*m13 - sinA*m23;     m23 = sinA*m13 + cosA*m23
    
    m10 = t10; m20 = t20
    m11 = t11; m21 = t21
    m12 = t12; m22 = t22
    m13 = t13
  }
  final def applyRotationY(angle: Float) {
    val sinA = sin(angle)
    val cosA = cos(angle)

    val t00 = cosA*m00 + sinA*m20; val t20 = cosA*m20 - sinA*m00
    val t01 = cosA*m01 + sinA*m21; val t21 = cosA*m21 - sinA*m01
    val t02 = cosA*m02 + sinA*m22; val t22 = cosA*m22 - sinA*m02
    val t03 = cosA*m03 + sinA*m23;     m23 = cosA*m23 - sinA*m03
    
    m00 = t00; m20 = t20
    m01 = t01; m21 = t21
    m02 = t02; m22 = t22
    m03 = t03
  }
  final def applyRotationZ(angle: Float) {
    val sinA = sin(angle)
    val cosA = cos(angle)
    
    val t00 = cosA*m00 - sinA*m10; val t10 = sinA*m00 + cosA*m10
    val t01 = cosA*m01 - sinA*m11; val t11 = sinA*m01 + cosA*m11
    val t02 = cosA*m02 - sinA*m12; val t12 = sinA*m02 + cosA*m12
    val t03 = cosA*m03 - sinA*m13;     m13 = sinA*m03 + cosA*m13
    
    m00 = t00; m10 = t10
    m01 = t01; m11 = t11
    m02 = t02; m12 = t12
    m03 = t03
  }

  final def applyTranslation(u: inVec3f) {
    m03 += u.x; m13 += u.y; m23 += u.z
  }

  final def applyTransformation(m: inMat3x4f) {
    val t00 = m.m00*m00 + m.m01*m10 + m.m02*m20
    val t10 = m.m10*m00 + m.m11*m10 + m.m12*m20
    val t20 = m.m20*m00 + m.m21*m10 + m.m22*m20

    val t01 = m.m00*m01 + m.m01*m11 + m.m02*m21
    val t11 = m.m10*m01 + m.m11*m11 + m.m12*m21
    val t21 = m.m20*m01 + m.m21*m11 + m.m22*m21

    val t02 = m.m00*m02 + m.m01*m12 + m.m02*m22
    val t12 = m.m10*m02 + m.m11*m12 + m.m12*m22
    val t22 = m.m20*m02 + m.m21*m12 + m.m22*m22

    val t03 = m.m00*m03 + m.m01*m13 + m.m02*m23 + m.m03
    val t13 = m.m10*m03 + m.m11*m13 + m.m12*m23 + m.m13
        m23 = m.m20*m03 + m.m21*m13 + m.m22*m23 + m.m23
    
    m00 = t00; m10 = t10; m20 = t20
    m01 = t01; m11 = t11; m21 = t21
    m02 = t02; m12 = t12; m22 = t22
    m03 = t03; m13 = t13
  }
  final def applyTransformation(m: inMat3f) {
    val t00 = m.m00*m00 + m.m01*m10 + m.m02*m20
    val t10 = m.m10*m00 + m.m11*m10 + m.m12*m20
    val t20 = m.m20*m00 + m.m21*m10 + m.m22*m20

    val t01 = m.m00*m01 + m.m01*m11 + m.m02*m21
    val t11 = m.m10*m01 + m.m11*m11 + m.m12*m21
    val t21 = m.m20*m01 + m.m21*m11 + m.m22*m21

    val t02 = m.m00*m02 + m.m01*m12 + m.m02*m22
    val t12 = m.m10*m02 + m.m11*m12 + m.m12*m22
    val t22 = m.m20*m02 + m.m21*m12 + m.m22*m22

    val t03 = m.m00*m03 + m.m01*m13 + m.m02*m23
    val t13 = m.m10*m03 + m.m11*m13 + m.m12*m23
        m23 = m.m20*m03 + m.m21*m13 + m.m22*m23
    
    m00 = t00; m10 = t10; m20 = t20
    m01 = t01; m11 = t11; m21 = t21
    m02 = t02; m12 = t12; m22 = t22
    m03 = t03; m13 = t13
  }


  def update(c: Int, r: Int, s: Float) {
    def error() = throw new IndexOutOfBoundsException(
      "Trying to update index (" + c + ", " + r + ") in " + this.getClass.getSimpleName + "."
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
      case 2 =>
        r match {
          case 0 => m02 = s
          case 1 => m12 = s
          case 2 => m22 = s
          case _ => error
        }
      case 3 =>
        r match {
          case 0 => m03 = s
          case 1 => m13 = s
          case 2 => m23 = s
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
      case 3 => m03 = v.x; m13 = v.y
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }

  def update(c: Int, v: inVec3f) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z
      case 2 => m02 = v.x; m12 = v.y; m22 = v.z
      case 3 => m03 = v.x; m13 = v.y; m23 = v.z
      case j => throw new IndexOutOfBoundsException(
          "Trying to update column (" + j + ") in " + this.getClass.getSimpleName + "."
        )
    }
  }
}

object Mat3x4f {
  final val Zero = ConstMat3x4f(0)
  final val Identity = ConstMat3x4f(1)

  final val Manifest = classType[Mat3x4f](classOf[Mat3x4f])
  final val ConstManifest = classType[ConstMat3x4f](classOf[ConstMat3x4f])
  final val ReadManifest = classType[ReadMat3x4f](classOf[ReadMat3x4f])

  def apply(s: Float) = new Mat3x4f(
    s, 0, 0,
    0, s, 0,
    0, 0, s,
    0, 0, 0
  )

  /*main factory*/ def apply(
    m00: Float, m10: Float, m20: Float,
    m01: Float, m11: Float, m21: Float,
    m02: Float, m12: Float, m22: Float,
    m03: Float, m13: Float, m23: Float
  ) = new Mat3x4f(
    m00, m10, m20,
    m01, m11, m21,
    m02, m12, m22,
    m03, m13, m23
  )

  def apply(c0: AnyVec3[_], c1: AnyVec3[_], c2: AnyVec3[_], c3: AnyVec3[_]) = 
  new Mat3x4f(
    c0.fx, c0.fy, c0.fz,
    c1.fx, c1.fy, c1.fz,
    c2.fx, c2.fy, c2.fz,
    c3.fx, c3.fy, c3.fz
  )

  def apply(m: AnyMat[_]) = new Mat3x4f(
    m.f00, m.f10, m.f20,
    m.f01, m.f11, m.f21,
    m.f02, m.f12, m.f22,
    m.f03, m.f13, m.f23
  )

  def unapply(m: ReadMat3x4f) = Some((m(0), m(1), m(2), m(3)))

  def scale(s: Float) :Mat3x4f = Mat3x4f(s)
  def scale(s: inVec3f) :Mat3x4f = {
    val m = Mat3x4f(s.x)
    m.m11 = s.y
    m.m22 = s.z
    m
  }

  def rotate(q: inQuat4f) :Mat3x4f = {
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

    new Mat3x4f(
      tc - td, bc + da, bd - ca,
      bc - da, 1 - tb - td, cd + ba,
      bd + ca, cd - ba, tc - tb,
      0, 0, 0
    )
  }

  def rotateX(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      1, 0, 0,
      0, cosA, sinA,
      0, -sinA, cosA,
      0, 0, 0
    )
  }
  def rotateY(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      cosA, 0, -sinA,
      0, 1, 0,
      sinA, 0, cosA,
      0, 0, 0
    )
  }
  def rotateZ(angle: Float) :Mat3x4f = {
    val sinA = sin(angle)
    val cosA = cos(angle)

    new Mat3x4f(
      cosA, sinA, 0,
      -sinA, cosA, 0,
      0, 0, 1,
      0, 0, 0
    )
  }

  def translate(u: inVec3f) :Mat3x4f = {
    val m = Mat3x4f(1)
    m(3) = u
    m
  }

  def concat(m: inMat3x4f) :Mat3x4f = Mat3x4f(m)
  def concat(m: inMat3f) :Mat3x4f = Mat3x4f(m)
}
