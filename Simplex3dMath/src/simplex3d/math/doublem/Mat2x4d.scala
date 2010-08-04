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

import scala.reflect.Manifest._
import simplex3d.math.integration.buffer._
import simplex3d.math.integration.property._
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadMat2x4d
extends ProtectedMat2x4d[Double]
{
  // Column major order.
  final def m00= p00; final def m10= p10
  final def m01= p01; final def m11= p11
  final def m02= p02; final def m12= p12
  final def m03= p03; final def m13= p13


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m10_=(s: Double) { throw new UnsupportedOperationException }

  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }

  protected def m02_=(s: Double) { throw new UnsupportedOperationException }
  protected def m12_=(s: Double) { throw new UnsupportedOperationException }

  protected def m03_=(s: Double) { throw new UnsupportedOperationException }
  protected def m13_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = float(m00)
  private[math] final override def f10 = float(m10)

  private[math] final override def f01 = float(m01)
  private[math] final override def f11 = float(m11)

  private[math] final override def f02 = float(m02)
  private[math] final override def f12 = float(m12)

  private[math] final override def f03 = float(m03)
  private[math] final override def f13 = float(m13)


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12

  private[math] final override def d03 = m03
  private[math] final override def d13 = m13


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m10)
      case 1 => new ConstVec2d(m01, m11)
      case 2 => new ConstVec2d(m02, m12)
      case 3 => new ConstVec2d(m03, m13)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 3, got " + j
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
      case 3 =>
        r match {
          case 0 => m03
          case 1 => m13
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :ReadMat2x4d = this
  final def unary_-() = new Mat2x4d(
    -m00, -m10,
    -m01, -m11,
    -m02, -m12,
    -m03, -m13
  )
  final def *(s: Double) = new Mat2x4d(
    s*m00, s*m10,
    s*m01, s*m11,
    s*m02, s*m12,
    s*m03, s*m13
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat2x4d(
    m00 + s, m10 + s,
    m01 + s, m11 + s,
    m02 + s, m12 + s,
    m03 + s, m13 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat2x4d) = new Mat2x4d(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11,
    m02 + m.m02, m12 + m.m12,
    m03 + m.m03, m13 + m.m13
  )
  final def -(m: inMat2x4d) = new Mat2x4d(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11,
    m02 - m.m02, m12 - m.m12,
    m03 - m.m03, m13 - m.m13
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2x4d) = new Mat2x4d(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11,
    m02/m.m02, m12/m.m12,
    m03/m.m03, m13/m.m13
  )
  private[math] final def divideByComponent(s: Double) = new Mat2x4d(
    s/m00, s/m10,
    s/m01, s/m11,
    s/m02, s/m12,
    s/m03, s/m13
  )

  final def *(m: inMat4x2d) = new Mat2d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
  )
  final def *(m: inMat4x3d) = new Mat2x3d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
  )
  final def *(m: inMat4d) = new Mat2x4d(
    m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
    m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

    m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
    m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

    m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
    m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,

    m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
    m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
  )

  final def *(u: inVec4d) = new Vec2d(
    m00*u.x + m01*u.y + m02*u.z + m03*u.w,
    m10*u.x + m11*u.y + m12*u.z + m13*u.w
  )
  private[math] final def transposeMul(u: inVec2d) = new Vec4d(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y,
    m02*u.x + m12*u.y,
    m03*u.x + m13*u.y
  )


  override def clone() = this

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: AnyMat2x4[_] =>
        d00 == m.d00 && d10 == m.d10 &&
        d01 == m.d01 && d11 == m.d11 &&
        d02 == m.d02 && d12 == m.d12 &&
        d03 == m.d03 && d13 == m.d13
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
              ) + m01.hashCode
            ) + m11.hashCode
          ) + m02.hashCode
        ) + m12.hashCode
      ) + m03.hashCode
    ) + m13.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + "; " + 
      m01 + ", " + m11 + "; " + 
      m02 + ", " + m12 + "; " + 
      m03 + ", " + m13 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat2x4d private[math] (
  c00: Double, c10: Double,
  c01: Double, c11: Double,
  c02: Double, c12: Double,
  c03: Double, c13: Double
) extends ReadMat2x4d with Immutable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12
  p03 = c03; p13 = c13

  override def clone() = this
}

object ConstMat2x4d {
  def apply(s: Double) = new ConstMat2x4d(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double,
    m02: Double, m12: Double,
    m03: Double, m13: Double
  ) = new ConstMat2x4d(
    m00, m10,
    m01, m11,
    m02, m12,
    m03, m13
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_], c3: AnyVec2[_]) = 
  new ConstMat2x4d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy,
    c3.dx, c3.dy
  )

  def apply(m: AnyMat[_]) = new ConstMat2x4d(
    m.d00, m.d10,
    m.d01, m.d11,
    m.d02, m.d12,
    m.d03, m.d13
  )

  implicit def toConst(m: ReadMat2x4d) = ConstMat2x4d(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2x4d private[math] (
  c00: Double, c10: Double,
  c01: Double, c11: Double,
  c02: Double, c12: Double,
  c03: Double, c13: Double
) extends ReadMat2x4d
  with PropertyObject[ReadMat2x4d] with Implicits[On] with Composite
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12
  p03 = c03; p13 = c13

  override def m00_=(s: Double) { p00 = s }
  override def m10_=(s: Double) { p10 = s }

  override def m01_=(s: Double) { p01 = s }
  override def m11_=(s: Double) { p11 = s }

  override def m02_=(s: Double) { p02 = s }
  override def m12_=(s: Double) { p12 = s }

  override def m03_=(s: Double) { p03 = s }
  override def m13_=(s: Double) { p13 = s }

  type Element = ReadMat2x4d
  type Component = Double1

  def *=(s: Double) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s;
    m02 *= s; m12 *= s;
    m03 *= s; m13 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m10 += s
    m01 += s; m11 += s
    m02 += s; m12 += s
    m03 += s; m13 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat2x4d) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11;
    m02 += m.m02; m12 += m.m12;
    m03 += m.m03; m13 += m.m13
  }
  def -=(m: inMat2x4d) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11;
    m02 -= m.m02; m12 -= m.m12;
    m03 -= m.m03; m13 -= m.m13
  }

  def *=(m: inMat4d) {
    val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
    val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30

    val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
    val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31

    val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
    val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32

    val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
    val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33

    m00 = a00; m10 = a10
    m01 = a01; m11 = a11
    m02 = a02; m12 = a12
    m03 = a03; m13 = a13
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat2x4d) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
    m02 /= m.m02; m12 /= m.m12
    m03 /= m.m03; m13 /= m.m13
  }

  def cloneValue() = ConstMat2x4d(this)
  def asReadInstance() :ReadMat2x4d = this /*asReadInstance*/
  override def clone() = Mat2x4d(this)
  
  override def :=(m: inMat2x4d) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11;
    m02 = m.m02; m12 = m.m12;
    m03 = m.m03; m13 = m.m13
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
      case 3 =>
        r match {
          case 0 => m03 = s
          case 1 => m13 = s
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
}

object Mat2x4d {
  final val Zero = ConstMat2x4d(0)
  final val Identity = ConstMat2x4d(1)
  final val Manifest = classType[ReadMat2x4d](classOf[ReadMat2x4d])

  def apply(s: Double) = new Mat2x4d(
    s, 0,
    0, s,
    0, 0,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double,
    m02: Double, m12: Double,
    m03: Double, m13: Double
  ) = new Mat2x4d(
    m00, m10,
    m01, m11,
    m02, m12,
    m03, m13
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_], c3: AnyVec2[_]) = 
  new Mat2x4d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy,
    c3.dx, c3.dy
  )

  def apply(m: AnyMat[_]) = new Mat2x4d(
    m.d00, m.d10,
    m.d01, m.d11,
    m.d02, m.d12,
    m.d03, m.d13
  )

  def unapply(m: ReadMat2x4d) = Some((m(0), m(1), m(2), m(3)))

  implicit def toMutable(m: ReadMat2x4d) = Mat2x4d(m)
  implicit def castFloat(m: AnyMat2x4[Float]) = apply(m)
}
