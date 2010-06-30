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
import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat2d extends Read2x2[Double]
{
  // Column major order.
  def m00: Double; def m10: Double // column
  def m01: Double; def m11: Double // column

  private[math] final override def f00 = float(m00)
  private[math] final override def f10 = float(m10)

  private[math] final override def f01 = float(m01)
  private[math] final override def f11 = float(m11)


  private[math] final override def d00 = m00
  private[math] final override def d10 = m10

  private[math] final override def d01 = m01
  private[math] final override def d11 = m11


  final def apply(c: Int) :ConstVec2d = {
    c match {
      case 0 => new ConstVec2d(m00, m10)
      case 1 => new ConstVec2d(m01, m11)
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
          case _ => error
        }
      case 1 =>
        r match {
          case 0 => m01
          case 1 => m11
          case _ => error
        }
      case _ => error
    }
  }

  final def unary_+() :AnyMat2d = this
  final def unary_-() = new Mat2d(
    -m00, -m10,
    -m01, -m11
  )
  final def *(s: Double) = new Mat2d(
    s*m00, s*m10,
    s*m01, s*m11
  )
  final def /(s: Double) = this * (1/s)

  final def +(s: Double) = new Mat2d(
    m00 + s, m10 + s,
    m01 + s, m11 + s
  )
  final def -(s: Double) = this + (-s)

  final def +(m: inMat2d) = new Mat2d(
    m00 + m.m00, m10 + m.m10,
    m01 + m.m01, m11 + m.m11
  )
  final def -(m: inMat2d) = new Mat2d(
    m00 - m.m00, m10 - m.m10,
    m01 - m.m01, m11 - m.m11
  )

  /**
   * Component-wise devision.
   */
  final def /(m: inMat2d) = new Mat2d(
    m00/m.m00, m10/m.m10,
    m01/m.m01, m11/m.m11
  )
  private[math] final def divideByComponent(s: Double) = new Mat2d(
    s/m00, s/m10,
    s/m01, s/m11
  )

  final def *(m: inMat2d) = new Mat2d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11
  )
  final def *(m: inMat2x3d) = new Mat2x3d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12
  )
  final def *(m: inMat2x4d) = new Mat2x4d(
    m00*m.m00 + m01*m.m10,
    m10*m.m00 + m11*m.m10,

    m00*m.m01 + m01*m.m11,
    m10*m.m01 + m11*m.m11,

    m00*m.m02 + m01*m.m12,
    m10*m.m02 + m11*m.m12,

    m00*m.m03 + m01*m.m13,
    m10*m.m03 + m11*m.m13
  )

  final def *(u: inVec2d) = new Vec2d(
    m00*u.x + m01*u.y,
    m10*u.x + m11*u.y
  )
  private[math] final def transposeMul(u: inVec2d) = new Vec2d(
    m00*u.x + m10*u.y,
    m01*u.x + m11*u.y
  )

  final override def equals(other: Any) :Boolean = {
    other match {
      case m: Read2x2[_] =>
        d00 == m.d00 && d10 == m.d10 &&
        d01 == m.d01 && d11 == m.d11
      case _ =>
        false
    }
  }

  final override def hashCode() :Int = {
    41 * (
      41 * (
        41 * (
          41 + m00.hashCode
        ) + m10.hashCode
      ) + m01.hashCode
    ) + m11.hashCode
  }

  final override def toString() :String = {
    this.getClass.getSimpleName +
    "(" +
      m00 + ", " + m10 + "; " + 
      m01 + ", " + m11 +
    ")"
  }
}


@serializable @SerialVersionUID(5359695191257934190L)
final class ConstMat2d private[math] (
  val m00: Double, val m10: Double,
  val m01: Double, val m11: Double
) extends AnyMat2d with Immutable

object ConstMat2d {
  def apply(s: Double) = new ConstMat2d(
    s, 0,
    0, s
  )

  /* main factory */ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double
  ) = new ConstMat2d(
    m00, m10,
    m01, m11
  )

  def apply(c0: Read2[_], c1: Read2[_]) = 
  new ConstMat2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy
  )

  def apply(u: Read4[_]) = new ConstMat2d(
    u.dx, u.dy,
    u.dz, u.dw
  )

  def apply(m: ReadMat[_]) = new ConstMat2d(
    m.d00, m.d10,
    m.d01, m.d11
  )

  implicit def toConst(m: AnyMat2d) = ConstMat2d(m)
}


@serializable @SerialVersionUID(5359695191257934190L)
final class Mat2d private[math] (
  var m00: Double, var m10: Double,
  var m01: Double, var m11: Double
) extends AnyMat2d with Mutable with Implicits[On] with Composite
{
  type Element = AnyMat2d
  type Component = Double1

  def *=(s: Double) {
    m00 *= s; m10 *= s;
    m01 *= s; m11 *= s
  }
  def /=(s: Double) { this *= (1/s) }

  def +=(s: Double) {
    m00 += s; m10 += s
    m01 += s; m11 += s
  }
  def -=(s: Double) { this += (-s) }

  def +=(m: inMat2d) {
    m00 += m.m00; m10 += m.m10;
    m01 += m.m01; m11 += m.m11
  }
  def -=(m: inMat2d) {
    m00 -= m.m00; m10 -= m.m10;
    m01 -= m.m01; m11 -= m.m11
  }

  def *=(m: inMat2d) {
    val a00 = m00*m.m00 + m01*m.m10
    val a10 = m10*m.m00 + m11*m.m10

    val a01 = m00*m.m01 + m01*m.m11
    val a11 = m10*m.m01 + m11*m.m11

    m00 = a00; m10 = a10
    m01 = a01; m11 = a11
  }
  /**
   * Component-wise devision.
   */
  def /=(m: inMat2d) {
    m00 /= m.m00; m10 /= m.m10
    m01 /= m.m01; m11 /= m.m11
  }

  def :=(m: inMat2d) {
    m00 = m.m00; m10 = m.m10;
    m01 = m.m01; m11 = m.m11
  }

  def set(
    m00: Double, m10: Double,
    m01: Double, m11: Double
  ) {
    this.m00 = m00; this.m10 = m10;
    this.m01 = m01; this.m11 = m11
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
}

object Mat2d {
  final val Zero = ConstMat2d(0)
  final val Identity = ConstMat2d(1)
  final val Manifest = classType[AnyMat2d](classOf[AnyMat2d])

  def apply(s: Double) = new Mat2d(
    s, 0,
    0, s
  )

  /* main factory */ def apply(
    m00: Double, m10: Double,
    m01: Double, m11: Double
  ) = new Mat2d(
    m00, m10,
    m01, m11
  )

  def apply(c0: Read2[_], c1: Read2[_]) = 
  new Mat2d(
    c0.dx, c0.dy,
    c1.dx, c1.dy
  )

  def apply(u: Read4[_]) = new Mat2d(
    u.dx, u.dy,
    u.dz, u.dw
  )

  def apply(m: ReadMat[_]) = new Mat2d(
    m.d00, m.d10,
    m.d01, m.d11
  )

  def unapply(m: AnyMat2d) = Some((m(0), m(1)))

  implicit def toMutable(m: AnyMat2d) = Mat2d(m)
  implicit def castFloat(m: Read2x2[Float]) = apply(m)
}
