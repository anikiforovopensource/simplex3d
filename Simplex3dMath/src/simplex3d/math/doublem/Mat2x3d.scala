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
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadMat2x3d
extends ProtectedMat2x3d[Double]
{
  // Column major order.
  final def m00= p00; final def m10= p10
  final def m01= p01; final def m11= p11
  final def m02= p02; final def m12= p12


  protected def m00_=(s: Double) { throw new UnsupportedOperationException }
  protected def m10_=(s: Double) { throw new UnsupportedOperationException }

  protected def m01_=(s: Double) { throw new UnsupportedOperationException }
  protected def m11_=(s: Double) { throw new UnsupportedOperationException }

  protected def m02_=(s: Double) { throw new UnsupportedOperationException }
  protected def m12_=(s: Double) { throw new UnsupportedOperationException }


  private[math] final override def f00 = m00.toFloat
  private[math] final override def f10 = m10.toFloat

  private[math] final override def f01 = m01.toFloat
  private[math] final override def f11 = m11.toFloat

  private[math] final override def f02 = m02.toFloat
  private[math] final override def f12 = m12.toFloat


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11

  private[math] final override def d02 = m02
  private[math] final override def d12 = m12


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m10)
      case 1 => new ConstVec2d(m01, m11)
      case 2 => new ConstVec2d(m02, m12)
      case j => throw new IndexOutOfBoundsException(
          "excpected from 0 to 2, got " + j
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
      case _ => error
    }
  }

  final def unary_+() :ReadMat2x3d = this
  final def unary_-() = new Mat2x3d(
    -m00, -m10,
    -m01, -m11,
    -m02, -m12
  )
  final def *(s: Double) = new Mat2x3d(
    s*m00, s*m10,
    s*m01, s*m11,
    s*m02, s*m12
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat2x3d(
    m00 + s, m10 + s,
    m01 + s, m11 + s,
    m02 + s, m12 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat2x3d) = new Mat2x3d(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11,
    m02 + m.m02, m12 + m.m12
  )
  final def -(m: inMat2x3d) = new Mat2x3d(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11,
    m02 - m.m02, m12 - m.m12
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2x3d) = new Mat2x3d(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11,
    m02/m.m02, m12/m.m12
  )
  private[math] final def divideByComponent(s: Double) = new Mat2x3d(
    s/m00, s/m10,
    s/m01, s/m11,
    s/m02, s/m12
  )

  final def *(m: inMat3x2d) = new Mat2d(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21
  )
  final def *(m: inMat3d) = new Mat2x3d(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22
  )
  final def *(m: inMat3x4d) = new Mat2x4d(
    m00*m.m00 + m01*m.m10 + m02*m.m20,
    m10*m.m00 + m11*m.m10 + m12*m.m20,

    m00*m.m01 + m01*m.m11 + m02*m.m21,
    m10*m.m01 + m11*m.m11 + m12*m.m21,

    m00*m.m02 + m01*m.m12 + m02*m.m22,
    m10*m.m02 + m11*m.m12 + m12*m.m22,

    m00*m.m03 + m01*m.m13 + m02*m.m23,
    m10*m.m03 + m11*m.m13 + m12*m.m23
  )

  final def *(u: inVec3d) = new Vec2d(
    m00*u.x + m01*u.y + m02*u.z,
    m10*u.x + m11*u.y + m12*u.z
  )
  private[math] final def transposeMul(u: inVec2d) = new Vec3d(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y,
    m02*u.x + m12*u.y
  )

  final def scale(s: Double) :Mat2x3d = this*s
  final def scale(s: inVec2d) :Mat2x3d = new Mat2x3d(
    m00*s.x, m10*s.y,
    m01*s.x, m11*s.y,
    m02*s.x, m12*s.y
  )

  final def rotate(angle: Double) :Mat2x3d = {
    concat(rotationMat(angle))
  }

  final def translate(u: inVec2d) :Mat2x3d = new Mat2x3d(
    m00, m10,
    m01, m11,
    m02 + u.x, m12 + u.y
  )

  final def concat(m: inMat2x3d) :Mat2x3d = new Mat2x3d(
    m.m00*m00 + m.m01*m10,
    m.m10*m00 + m.m11*m10,

    m.m00*m01 + m.m01*m11,
    m.m10*m01 + m.m11*m11,

    m.m00*m02 + m.m01*m12 + m.m02,
    m.m10*m02 + m.m11*m12 + m.m12
  )
  final def concat(m: inMat2d) :Mat2x3d = m*this

  final def transformPoint(p: inVec2d) :Vec2d = new Vec2d(
    m00*p.x + m01*p.y + m02,
    m10*p.x + m11*p.y + m12
  )
  final def transformVector(v: inVec2d) :Vec2d = new Vec2d(
    m00*v.x + m01*v.y,
    m10*v.x + m11*v.y
  )


  override def clone() = this

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
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + "; " + 
      m01 + ", " + m11 + "; " + 
      m02 + ", " + m12 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat2x3d private[math] (
  c00: Double, c10: Double,
  c01: Double, c11: Double,
  c02: Double, c12: Double
) extends ReadMat2x3d with Immutable
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12

  override def clone() = this
}

object ConstMat2x3d {
  def apply(s: Double) = new ConstMat2x3d(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double,
    m02: Double, m12: Double
  ) = new ConstMat2x3d(
    m00, m10,
    m01, m11,
    m02, m12
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new ConstMat2x3d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy
  )

  def apply(m: AnyMat[_]) = new ConstMat2x3d(
    m.d00, m.d10,
    m.d01, m.d11,
    m.d02, m.d12
  )

  implicit def toConst(m: ReadMat2x3d) = ConstMat2x3d(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2x3d private[math] (
  c00: Double, c10: Double,
  c01: Double, c11: Double,
  c02: Double, c12: Double
) extends ReadMat2x3d with Implicits[On] with Composite
{
  p00 = c00; p10 = c10
  p01 = c01; p11 = c11
  p02 = c02; p12 = c12

  override def m00_=(s: Double) { p00 = s }
  override def m10_=(s: Double) { p10 = s }

  override def m01_=(s: Double) { p01 = s }
  override def m11_=(s: Double) { p11 = s }

  override def m02_=(s: Double) { p02 = s }
  override def m12_=(s: Double) { p12 = s }

  type Element = ReadMat2x3d
  type Immutable = ConstMat2x3d
  type Component = Double1

  def *=(s: Double) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s;
    m02 *= s; m12 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m10 += s
    m01 += s; m11 += s
    m02 += s; m12 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat2x3d) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11;
    m02 += m.m02; m12 += m.m12
  }
  def -=(m: inMat2x3d) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11;
    m02 -= m.m02; m12 -= m.m12
  }

  def *=(m: inMat3d) {
    val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
    val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20

    val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
    val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21

    val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
    val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22

    m00 = a00; m10 = a10
    m01 = a01; m11 = a11
    m02 = a02; m12 = a12
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat2x3d) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
    m02 /= m.m02; m12 /= m.m12
  }

  override def clone() = Mat2x3d(this)
  
  def :=(m: inMat2x3d) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11;
    m02 = m.m02; m12 = m.m12
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
      case _ => error
    }
  }

  def update(c: Int, v: inVec2d) {
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

object Mat2x3d {
  final val Zero = ConstMat2x3d(0)
  final val Identity = ConstMat2x3d(1)

  final val Manifest = classType[Mat2x3d](classOf[Mat2x3d])
  final val ConstManifest = classType[ConstMat2x3d](classOf[ConstMat2x3d])
  final val ReadManifest = classType[ReadMat2x3d](classOf[ReadMat2x3d])

  def apply(s: Double) = new Mat2x3d(
    s, 0,
    0, s,
    0, 0
  )

  /*main factory*/ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double,
    m02: Double, m12: Double
  ) = new Mat2x3d(
    m00, m10,
    m01, m11,
    m02, m12
  )

  def apply(c0: AnyVec2[_], c1: AnyVec2[_], c2: AnyVec2[_]) = 
  new Mat2x3d(
    c0.dx, c0.dy,
    c1.dx, c1.dy,
    c2.dx, c2.dy
  )

  def apply(m: AnyMat[_]) = new Mat2x3d(
    m.d00, m.d10,
    m.d01, m.d11,
    m.d02, m.d12
  )

  def unapply(m: ReadMat2x3d) = Some((m(0), m(1), m(2)))

  def scale(s: Double) :Mat2x3d = Mat2x3d(s)
  def scale(s: inVec2d) :Mat2x3d = {
    val m = Mat2x3d(s.x)
    m.m11 = s.y
    m
  }

  def rotate(angle: Double) :Mat2x3d = {
    Mat2x3d(rotationMat(angle))
  }

  def translate(u: inVec2d) :Mat2x3d = {
    val m = Mat2x3d(1)
    m(2) = u
    m
  }

  def concat(m: inMat2x3d) :Mat2x3d = Mat2x3d(m)
  def concat(m: inMat2d) :Mat2x3d = Mat2x3d(m)

  implicit def toMutable(m: ReadMat2x3d) = Mat2x3d(m)
  implicit def castFloat(m: AnyMat2x3[Float]) = apply(m)
}
