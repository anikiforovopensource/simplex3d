/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.doublem

import scala.reflect.ClassManifest._
import simplex3d.math.integration.data._
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
@serializable @SerialVersionUID(8104346712419693669L)
sealed abstract class ReadMat4x2d
extends ProtectedMat4x2d[Double]
{
  // Column major order.
  final def m00= p00; final def m10= p10; final def m20= p20; final def m30= p30
  final def m01= p01; final def m11= p11; final def m21= p21; final def m31= p31


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m10_=(s: Double) { throw new UnsupportedOperationException }
  protected def m20_=(s: Double) { throw new UnsupportedOperationException }
  protected def m30_=(s: Double) { throw new UnsupportedOperationException }

  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }
  protected def m21_=(s: Double) { throw new UnsupportedOperationException }
  protected def m31_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f10 = m10.toFloat
  private[math] final override def f20 = m20.toFloat
  private[math] final override def f30 = m30.toFloat

  private[math] final override def f01 = m01.toFloat
  private[math] final override def f11 = m11.toFloat
  private[math] final override def f21 = m21.toFloat
  private[math] final override def f31 = m31.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10
  private[math] final override def d20 = m20
  private[math] final override def d30 = m30

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11
  private[math] final override def d21 = m21
  private[math] final override def d31 = m31


  final def apply(c: Int) :ConstVec4d = {
    c match {
      case 0 => new ConstVec4d(m00, m10, m20, m30)
      case 1 => new ConstVec4d(m01, m11, m21, m31)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
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
      case _ => error
    }
  }

  final def unary_+() :ReadMat4x2d = this
  final def unary_-() = new Mat4x2d(
    -m00, -m10, -m20, -m30,
    -m01, -m11, -m21, -m31
  )
  final def *(s: Double) = new Mat4x2d(
    s*m00, s*m10, s*m20, s*m30,
    s*m01, s*m11, s*m21, s*m31
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat4x2d(
    m00 + s, m10 + s, m20 + s, m30 + s,
    m01 + s, m11 + s, m21 + s, m31 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat4x2d) = new Mat4x2d(
    m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
    m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31
  )
  final def -(m: inMat4x2d) = new Mat4x2d(
    m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
    m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat4x2d) = new Mat4x2d(
    m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
    m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31
  )
  private[math] final def divideByComponent(s: Double) = new Mat4x2d(
    s/m00, s/m10, s/m20, s/m30,
    s/m01, s/m11, s/m21, s/m31
  )

  final def *(m: inMat2d) = new Mat4x2d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11
  )
  final def *(m: inMat2x3d) = new Mat4x3d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12,
    m30*m.m02 + m31*m.m12
  )
  final def *(m: inMat2x4d) = new Mat4d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,
    m20*m.m00 + m21*m.m10,
    m30*m.m00 + m31*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,
    m20*m.m01 + m21*m.m11,
    m30*m.m01 + m31*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,
    m20*m.m02 + m21*m.m12,
    m30*m.m02 + m31*m.m12,

    m00*m.m03 + m01*m.m13,
    m10*m.m03 + m11*m.m13,
    m20*m.m03 + m21*m.m13,
    m30*m.m03 + m31*m.m13
  )

  final def *(u: inVec2d) = new Vec4d(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y,
    m20*u.x + m21*u.y,
    m30*u.x + m31*u.y
  )
  private[math] final def transposeMul(u: inVec4d) = new Vec2d(
    m00*u.x + m10*u.y + m20*u.z + m30*u.w,
    m01*u.x + m11*u.y + m21*u.z + m31*u.w
  )


  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat4x2[_] =>
        d00 == m.d00 && d10 == m.d10 && d20 == m.d20 && d30 == m.d30 &&
        d01 == m.d01 && d11 == m.d11 && d21 == m.d21 && d31 == m.d31
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
                ) + m10.hashCode
              ) + m20.hashCode
            ) + m30.hashCode
          ) + m01.hashCode
        ) + m11.hashCode
      ) + m21.hashCode
    ) + m31.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
      m01 + ", " + m11 + ", " + m21 + ", " + m31 +
    ")"
  }
}


@serializable @SerialVersionUID(8104346712419693669L)
final class ConstMat4x2d private[math] (
  c00: Double, c10: Double, c20: Double, c30: Double,
  c01: Double, c11: Double, c21: Double, c31: Double
) extends ReadMat4x2d with Immutable
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31

  override def clone() = this
}

object ConstMat4x2d {
  def apply(s: Double) = new ConstMat4x2d(
    s, 0, 0, 0,
    0, s, 0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double, m20: Double, m30: Double,
    m01: Double, m11: Double, m21: Double, m31: Double
  ) = new ConstMat4x2d(
    m00, m10, m20, m30,
    m01, m11, m21, m31
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_]) = 
  new ConstMat4x2d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw
  )

  def apply(m: AnyMat[_]) = new ConstMat4x2d(
    m.d00, m.d10, m.d20, m.d30,
    m.d01, m.d11, m.d21, m.d31
  )

  implicit def toConst(m: ReadMat4x2d) = ConstMat4x2d(m)
}


@serializable @SerialVersionUID(8104346712419693669L)
final class Mat4x2d private[math] (
  c00: Double, c10: Double, c20: Double, c30: Double,
  c01: Double, c11: Double, c21: Double, c31: Double
) extends ReadMat4x2d with Implicits[On] with Composite
{
  p00 = c00; p10 = c10; p20 = c20; p30 = c30
  p01 = c01; p11 = c11; p21 = c21; p31 = c31

  override def m00_=(s: Double) { p00 = s }
  override def m10_=(s: Double) { p10 = s }
  override def m20_=(s: Double) { p20 = s }
  override def m30_=(s: Double) { p30 = s }

  override def m01_=(s: Double) { p01 = s }
  override def m11_=(s: Double) { p11 = s }
  override def m21_=(s: Double) { p21 = s }
  override def m31_=(s: Double) { p31 = s }

  type Read = ReadMat4x2d
  type Const = ConstMat4x2d
  type Component = RDouble

  def *=(s: Double) {
    m00 *= s; m10 *= s; m20 *= s; m30 *= s;
    m01 *= s; m11 *= s; m21 *= s; m31 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m10 += s; m20 += s; m30 += s
    m01 += s; m11 += s; m21 += s; m31 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat4x2d) {
    m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
    m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31
  }
  def -=(m: inMat4x2d) {
    m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
    m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31
  }

  def *=(m: inMat2d) {
    val a00 = m00*m.m00 + m01*m.m10
    val a10 = m10*m.m00 + m11*m.m10
    val a20 = m20*m.m00 + m21*m.m10
    val a30 = m30*m.m00 + m31*m.m10

    val a01 = m00*m.m01 + m01*m.m11
    val a11 = m10*m.m01 + m11*m.m11
    val a21 = m20*m.m01 + m21*m.m11
    val a31 = m30*m.m01 + m31*m.m11

    m00 = a00; m10 = a10; m20 = a20; m30 = a30
    m01 = a01; m11 = a11; m21 = a21; m31 = a31
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat4x2d) {
    m00 /= m.m00; m10 /= m.m10; m20 /= m.m20; m30 /= m.m30
    m01 /= m.m01; m11 /= m.m11; m21 /= m.m21; m31 /= m.m31
  }

  override def clone() = Mat4x2d(this)
  
  def :=(m: inMat4x2d) {
    m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
    m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31
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
      case _ => error
    }
  }

  def update(c: Int, v: inVec2d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y
      case 1 => m01 = v.x; m11 = v.y
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  def update(c: Int, v: inVec3d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
        )
    }
  }

  def update(c: Int, v: inVec4d) {
    c match {
      case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
      case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 1, got " + j
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
    s, 0, 0, 0,
    0, s, 0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double, m20: Double, m30: Double,
    m01: Double, m11: Double, m21: Double, m31: Double
  ) = new Mat4x2d(
    m00, m10, m20, m30,
    m01, m11, m21, m31
  )

  def apply(c0: AnyVec4[_], c1: AnyVec4[_]) = 
  new Mat4x2d(
    c0.dx, c0.dy, c0.dz, c0.dw,
    c1.dx, c1.dy, c1.dz, c1.dw
  )

  def apply(m: AnyMat[_]) = new Mat4x2d(
    m.d00, m.d10, m.d20, m.d30,
    m.d01, m.d11, m.d21, m.d31
  )

  def unapply(m: ReadMat4x2d) = Some((m(0), m(1)))

  implicit def toMutable(m: ReadMat4x2d) = Mat4x2d(m)
  implicit def castFloat(m: AnyMat4x2[Float]) = apply(m)
}
