/*
 * Simplex3D, DoubleMath module
 * Copyright (C) 2009 Simplex3D team
 *
 * This file is part of Simplex3d.
 *
 * Simplex3d is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3d is distributed in the hope that it will be useful,
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
sealed abstract class AnyMat2d
{
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column

    def apply(c: Int) :ConstVec2d = {
        c match {
            case 0 => new ConstVec2d(m00, m10)
            case 1 => new ConstVec2d(m01, m11)
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
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
            case _ => error
        }
    }

    def unary_-() = new Mat2d(
        -m00, -m10,
        -m01, -m11
    )
    def *(s: Double) = new Mat2d(
        s*m00, s*m10,
        s*m01, s*m11
    )
    def /(s: Double) = { val inv = 1/s; new Mat2d(
        inv*m00, inv*m10,
        inv*m01, inv*m11
    )}

    def +(m: AnyMat2d) = new Mat2d(
        m00 + m.m00, m10 + m.m10,
        m01 + m.m01, m11 + m.m11
    )
    def -(m: AnyMat2d) = new Mat2d(
        m00 - m.m00, m10 - m.m10,
        m01 - m.m01, m11 - m.m11
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2d) = new Mat2d(
        m00/m.m00, m10/m.m10,
        m01/m.m01, m11/m.m11
    )
    private[math] def divideByComponent(s: Double) = new Mat2d(
        s/m00, s/m10,
        s/m01, s/m11
    )

    def *(m: AnyMat2d) = new Mat2d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11
    )
    def *(m: AnyMat2x3d) = new Mat2x3d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m00*m.m02 + m01*m.m12,
        m10*m.m02 + m11*m.m12
    )
    def *(m: AnyMat2x4d) = new Mat2x4d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m00*m.m02 + m01*m.m12,
        m10*m.m02 + m11*m.m12,

        m00*m.m03 + m01*m.m13,
        m10*m.m03 + m11*m.m13
    )

    def *(u: AnyVec2d) = new Vec2d(
        m00*u.x + m01*u.y,
        m10*u.x + m11*u.y
    )
    protected[math] def transposeMul(u: AnyVec2d) = new Vec2d(
        m00*u.x + m10*u.y,
        m01*u.x + m11*u.y
    )

    def ==(m: AnyMat2d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 &&
            m01 == m.m01 && m11 == m.m11
    }

    def !=(m: AnyMat2d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + "; " + 
            m01 + ", " + m11 +
        ")"
    }
}

final class ConstMat2d private[math] (
    val m00: Double, val m10: Double,
    val m01: Double, val m11: Double
) extends AnyMat2d

object ConstMat2d {

    def apply(
        m00: Double, m10: Double,
        m01: Double, m11: Double
      ) = new ConstMat2d(
            m00, m10,
            m01, m11
      )

    def apply(c0: AnyVec2d, c1: AnyVec2d) = 
    new ConstMat2d(
        c0.x, c0.y,
        c1.x, c1.y
    )

    def apply(m: AnyMat2d) = new ConstMat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    implicit def mutableToConst(m: Mat2d) = ConstMat2d(m)
}


final class Mat2d private[math] (
    var m00: Double, var m10: Double,
    var m01: Double, var m11: Double
) extends AnyMat2d
{
    def *=(s: Double) {
        m00 *= s; m10 *= s;
        m01 *= s; m11 *= s
    }
    def /=(s: Double) { val inv = 1/s;
        m00 *= inv; m10 *= inv;
        m01 *= inv; m11 *= inv
    }

    def +=(m: AnyMat2d) {
        m00 += m.m00; m10 += m.m10;
        m01 += m.m01; m11 += m.m11
    }
    def -=(m: AnyMat2d) {
        m00 -= m.m00; m10 -= m.m10;
        m01 -= m.m01; m11 -= m.m11
    }

    def *=(m: AnyMat2d) {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        m00 = a00; m10 = a10
        m01 = a01; m11 = a11
    }

    def :=(m: AnyMat2d) {
        m00 = m.m00; m10 = m.m10;
        m01 = m.m01; m11 = m.m11
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
            case _ => error
        }
    }

    def update(c: Int, v: AnyVec2d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y
            case 1 => m01 = v.x; m11 = v.y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

}

object Mat2d {

    val Zero: ConstMat2d = Mat2d(0)
    val Identity: ConstMat2d = Mat2d(1)

    def apply(s: Double) = new Mat2d(
        s, 0,
        0, s
    )

    def apply(c0: Read2Float, c1: Read2Float) = 
    new Mat2d(
        c0.x, c0.y,
        c1.x, c1.y
    )

    def apply(
        m00: Double, m10: Double,
        m01: Double, m11: Double
      ) = new Mat2d(
            m00, m10,
            m01, m11
      )

    def apply(c0: AnyVec2d, c1: AnyVec2d) = 
    new Mat2d(
        c0.x, c0.y,
        c1.x, c1.y
    )

    def apply(m: AnyMat2d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x3d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x4d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x2d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x4d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x2d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x3d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4d) = new Mat2d(
        m.m00, m.m10,
        m.m01, m.m11
    )

    implicit def constToMutable(m: ConstMat2d) = Mat2d(m)
}
