/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009 Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat2x3d
{
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column
    def m02: Double; def m12: Double // column

    def apply(c: Int) :ConstVec2d = {
        c match {
            case 0 => new ConstVec2d(m00, m10)
            case 1 => new ConstVec2d(m01, m11)
            case 2 => new ConstVec2d(m02, m12)
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def apply(c: Int, r: Int) :Double = {
        def error() :Double = {
            throw new IndexOutOfBoundsException("Trying to read index (" +
                     c + ", " + r + ") in " + this.getClass.getSimpleName)
        }

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

    def unary_-() = new Mat2x3d(
        -m00, -m10,
        -m01, -m11,
        -m02, -m12
    )
    def *(s: Double) = new Mat2x3d(
        s*m00, s*m10,
        s*m01, s*m11,
        s*m02, s*m12
    )
    def /(s: Double) = { val inv = 1/s; new Mat2x3d(
        inv*m00, inv*m10,
        inv*m01, inv*m11,
        inv*m02, inv*m12
    )}

    def +(m: AnyMat2x3d) = new Mat2x3d(
        m00 + m.m00, m10 + m.m10,
        m01 + m.m01, m11 + m.m11,
        m02 + m.m02, m12 + m.m12
    )
    def -(m: AnyMat2x3d) = new Mat2x3d(
        m00 - m.m00, m10 - m.m10,
        m01 - m.m01, m11 - m.m11,
        m02 - m.m02, m12 - m.m12
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2x3d) = new Mat2x3d(
        m00/m.m00, m10/m.m10,
        m01/m.m01, m11/m.m11,
        m02/m.m02, m12/m.m12
    )
    private[math] def divideByComponent(s: Double) = new Mat2x3d(
        s/m00, s/m10,
        s/m01, s/m11,
        s/m02, s/m12
    )

    def *(m: AnyMat3x2d) = new Mat2d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21
    )
    def *(m: AnyMat3d) = new Mat2x3d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21,

        m00*m.m02 + m01*m.m12 + m02*m.m22,
        m10*m.m02 + m11*m.m12 + m12*m.m22
    )
    def *(m: AnyMat3x4d) = new Mat2x4d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21,

        m00*m.m02 + m01*m.m12 + m02*m.m22,
        m10*m.m02 + m11*m.m12 + m12*m.m22,

        m00*m.m03 + m01*m.m13 + m02*m.m23,
        m10*m.m03 + m11*m.m13 + m12*m.m23
    )

    def *(u: AnyVec3d) = new Vec2d(
        m00*u.x + m01*u.y + m02*u.z,
        m10*u.x + m11*u.y + m12*u.z
    )
    private[math] def transposeMul(u: AnyVec2d) = new Vec3d(
        m00*u.x + m10*u.y,
        m01*u.x + m11*u.y,
        m02*u.x + m12*u.y
    )

    /**
     * This method will apply the matrix transformation to a point
     * (such as vertex or object location).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 1).
     */
    def transformPoint(u: AnyVec2d) = new Vec2d(
        m00*u.x + m01*u.y + m02,
        m10*u.x + m11*u.y + m12
    )
    /**
     * This method will apply the matrix transformation to a vector
     * (such as object speed).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 0).
     */
    def transformVector(u: AnyVec2d) = new Vec2d(
        m00*u.x + m01*u.y,
        m10*u.x + m11*u.y
    )

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *(m: AnyMat2x3d) = new Mat2x3d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m00*m.m02 + m01*m.m12 + m02,
        m10*m.m02 + m11*m.m12 + m12
    )

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *(m: AnyMat2d) = new Mat2x3d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m02,
        m12
    )

    def ==(m: AnyMat2x3d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 &&
            m01 == m.m01 && m11 == m.m11 &&
            m02 == m.m02 && m12 == m.m12
    }

    def !=(m: AnyMat2x3d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||

            isNaN(m02) || isInfinite(m02) ||
            isNaN(m12) || isInfinite(m12)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case m: AnyMat2x3d => this == m
            case _ => false
        }
    }

    override def hashCode :Int = {
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

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + "; " + 
            m01 + ", " + m11 + "; " + 
            m02 + ", " + m12 +
        ")"
    }
}

final class ConstMat2x3d private[math] (
    val m00: Double, val m10: Double,
    val m01: Double, val m11: Double,
    val m02: Double, val m12: Double
) extends AnyMat2x3d

object ConstMat2x3d {

    def apply(
        m00: Double, m10: Double,
        m01: Double, m11: Double,
        m02: Double, m12: Double
      ) = new ConstMat2x3d(
            m00, m10,
            m01, m11,
            m02, m12
      )

    def apply(c0: AnyVec2d, c1: AnyVec2d, c2: AnyVec2d) = 
    new ConstMat2x3d(
        c0.x, c0.y,
        c1.x, c1.y,
        c2.x, c2.y
    )

    def apply(m: AnyMat2x3d) = new ConstMat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    implicit def mutableToConst(m: Mat2x3d) = ConstMat2x3d(m)
}


final class Mat2x3d private[math] (
    var m00: Double, var m10: Double,
    var m01: Double, var m11: Double,
    var m02: Double, var m12: Double
) extends AnyMat2x3d
{
    def *=(s: Double) {
        m00 *= s; m10 *= s;
        m01 *= s; m11 *= s;
        m02 *= s; m12 *= s
    }
    def /=(s: Double) { val inv = 1/s;
        m00 *= inv; m10 *= inv;
        m01 *= inv; m11 *= inv;
        m02 *= inv; m12 *= inv
    }

    def +=(m: AnyMat2x3d) {
        m00 += m.m00; m10 += m.m10;
        m01 += m.m01; m11 += m.m11;
        m02 += m.m02; m12 += m.m12
    }
    def -=(m: AnyMat2x3d) {
        m00 -= m.m00; m10 -= m.m10;
        m01 -= m.m01; m11 -= m.m11;
        m02 -= m.m02; m12 -= m.m12
    }

    def *=(m: AnyMat3d) {
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
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *=(m: AnyMat2x3d) {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        val a02 = m00*m.m02 + m01*m.m12 + m02
        val a12 = m10*m.m02 + m11*m.m12 + m12

        m00 = a00; m10 = a10
        m01 = a01; m11 = a11
        m02 = a02; m12 = a12
    }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *=(m: AnyMat2d) {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        m00 = a00; m10 = a10
        m01 = a01; m11 = a11
    }

    def :=(m: AnyMat2x3d) {
        m00 = m.m00; m10 = m.m10;
        m01 = m.m01; m11 = m.m11;
        m02 = m.m02; m12 = m.m12
    }

    def set(
        m00: Double, m10: Double,
        m01: Double, m11: Double,
        m02: Double, m12: Double
    ) {
        this.m00 = m00; this.m10 = m10;
        this.m01 = m01; this.m11 = m11;
        this.m02 = m02; this.m12 = m12
    }

    def update(c: Int, r: Int, s: Double) {
        def error() {
            throw new IndexOutOfBoundsException("Trying to update index (" +
                     c + ", " + r + ") in " + this.getClass.getSimpleName)
        }

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

    def update(c: Int, v: AnyVec2d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y
            case 1 => m01 = v.x; m11 = v.y
            case 2 => m02 = v.x; m12 = v.y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Mat2x3d {

    val Zero: ConstMat2x3d = Mat2x3d(0)
    val Identity: ConstMat2x3d = Mat2x3d(1)

    def apply(s: Double) = new Mat2x3d(
        s, 0,
        0, s,
        0, 0
    )

    def apply(c0: Read2Float, c1: Read2Float, c2: Read2Float) = 
    new Mat2x3d(
        c0.x, c0.y,
        c1.x, c1.y,
        c2.x, c2.y
    )

    def apply(
        m00: Double, m10: Double,
        m01: Double, m11: Double,
        m02: Double, m12: Double
      ) = new Mat2x3d(
            m00, m10,
            m01, m11,
            m02, m12
      )

    def apply(c0: AnyVec2d, c1: AnyVec2d, c2: AnyVec2d) = 
    new Mat2x3d(
        c0.x, c0.y,
        c1.x, c1.y,
        c2.x, c2.y
    )

    def apply(m: AnyMat2d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0
    )

    def apply(m: AnyMat2x3d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    def apply(m: AnyMat2x4d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    def apply(m: AnyMat3x2d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0
    )

    def apply(m: AnyMat3d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    def apply(m: AnyMat3x4d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    def apply(m: AnyMat4x2d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0
    )

    def apply(m: AnyMat4x3d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    def apply(m: AnyMat4d) = new Mat2x3d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12
    )

    implicit def constToMutable(m: ConstMat2x3d) = Mat2x3d(m)
}
