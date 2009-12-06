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
import Read._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat4x2d
{
    // Column major order.
    def m00: Double; def m10: Double; def m20: Double; def m30: Double // column
    def m01: Double; def m11: Double; def m21: Double; def m31: Double // column

    def apply(c: Int) :ConstVec4d = {
        c match {
            case 0 => ConstVec4d(m00, m10, m20, m30)
            case 1 => ConstVec4d(m01, m11, m21, m31)
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

    def unary_-() = Mat4x2d(
        -m00, -m10, -m20, -m30,
        -m01, -m11, -m21, -m31
    )
    def *(s: Double) = Mat4x2d(
        s*m00, s*m10, s*m20, s*m30,
        s*m01, s*m11, s*m21, s*m31
    )
    def /(s: Double) = { val inv = 1/s; Mat4x2d(
        inv*m00, inv*m10, inv*m20, inv*m30,
        inv*m01, inv*m11, inv*m21, inv*m31
    )}

    def +(m: AnyMat4x2d) = Mat4x2d(
        m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
        m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31
    )
    def -(m: AnyMat4x2d) = Mat4x2d(
        m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
        m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat4x2d) = Mat4x2d(
        m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
        m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31
    )
    private[math] def divideByComponent(s: Double) = Mat4x2d(
        s/m00, s/m10, s/m20, s/m30,
        s/m01, s/m11, s/m21, s/m31
    )

    def *(m: AnyMat2d) = Mat4x2d(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,
        m20*m.m00 + m21*m.m10,
        m30*m.m00 + m31*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,
        m20*m.m01 + m21*m.m11,
        m30*m.m01 + m31*m.m11
    )
    def *(m: AnyMat2x3d) = Mat4x3d(
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
    def *(m: AnyMat2x4d) = Mat4d(
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

    def *(u: AnyVec2d) = Vec4d(
        m00*u.x + m01*u.y,
        m10*u.x + m11*u.y,
        m20*u.x + m21*u.y,
        m30*u.x + m31*u.y
    )
    protected[math] def transposeMul(u: AnyVec4d) = Vec2d(
        m00*u.x + m10*u.y + m20*u.z + m30*u.w,
        m01*u.x + m11*u.y + m21*u.z + m31*u.w
    )

    def ==(m: AnyMat4x2d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 && m30 == m.m30 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 && m31 == m.m31
    }

    def !=(m: AnyMat4x2d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||
            isNaN(m20) || isInfinite(m20) ||
            isNaN(m30) || isInfinite(m30) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||
            isNaN(m21) || isInfinite(m21) ||
            isNaN(m31) || isInfinite(m31)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
            m01 + ", " + m11 + ", " + m21 + ", " + m31 +
        ")"
    }
}

final class ConstMat4x2d private (
    val m00: Double, val m10: Double, val m20: Double, val m30: Double,
    val m01: Double, val m11: Double, val m21: Double, val m31: Double
) extends AnyMat4x2d

object ConstMat4x2d {

    def apply(s: Double) = new ConstMat4x2d(
        s, 0, 0, 0,
        0, s, 0, 0
    )

    def apply(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double
      ) = new ConstMat4x2d(
            m00, m10, m20, m30,
            m01, m11, m21, m31
      )

    def apply(args: ReadAny[Double]*) :ConstMat4x2d = {
        val mat = new Array[Double](8)
        mat(0) = 1
        mat(5) = 1

        var index = 0
        try {
            var i = 0; while (i < args.length) {
                index = read(args(i), mat, index)
                i += 1
            }
        }
        catch {
            case iae: IllegalArgumentException => {
                throw new IllegalArgumentException(iae.getMessage)
            }
            case aob: ArrayIndexOutOfBoundsException => {
                throw new IllegalArgumentException(
                    "Too many values for this matrix.")
            }
        }

        if (index < 8) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new ConstMat4x2d(
            mat(0), mat(1), mat(2), mat(3),
            mat(4), mat(5), mat(6), mat(7)
        )
    }

    def apply(m: AnyMat2d) = new ConstMat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x3d) = new ConstMat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x4d) = new ConstMat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat3x2d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3x4d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat4x2d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4x3d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4d) = new ConstMat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    implicit def mutableToConst(m: Mat4x2d) = ConstMat4x2d(m)
}


final class Mat4x2d private (
    var m00: Double, var m10: Double, var m20: Double, var m30: Double,
    var m01: Double, var m11: Double, var m21: Double, var m31: Double
) extends AnyMat4x2d
{
    def *=(s: Double) {
        m00 *= s; m10 *= s; m20 *= s; m30 *= s;
        m01 *= s; m11 *= s; m21 *= s; m31 *= s
    }
    def /=(s: Double) { val inv = 1/s;
        m00 *= inv; m10 *= inv; m20 *= inv; m30 *= inv;
        m01 *= inv; m11 *= inv; m21 *= inv; m31 *= inv
    }

    def +=(m: AnyMat4x2d) {
        m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
        m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31
    }
    def -=(m: AnyMat4x2d) {
        m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
        m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31
    }

    def *=(m: AnyMat2d) {
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

    def :=(m: AnyMat4x2d) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
        m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31
    }

    def set(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20; this.m30 = m30;
        this.m01 = m01; this.m11 = m11; this.m21 = m21; this.m31 = m31
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

    def update(c: Int, v: AnyVec4d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

}

object Mat4x2d {

    def apply(s: Double) = new Mat4x2d(
        s, 0, 0, 0,
        0, s, 0, 0
    )

    def apply(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double
      ) = new Mat4x2d(
            m00, m10, m20, m30,
            m01, m11, m21, m31
      )

    def apply(args: ReadAny[Double]*) :Mat4x2d = {
        val mat = new Array[Double](8)
        mat(0) = 1
        mat(5) = 1

        var index = 0
        try {
            var i = 0; while (i < args.length) {
                index = read(args(i), mat, index)
                i += 1
            }
        }
        catch {
            case iae: IllegalArgumentException => {
                throw new IllegalArgumentException(iae.getMessage)
            }
            case aob: ArrayIndexOutOfBoundsException => {
                throw new IllegalArgumentException(
                    "Too many values for this matrix.")
            }
        }

        if (index < 8) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat4x2d(
            mat(0), mat(1), mat(2), mat(3),
            mat(4), mat(5), mat(6), mat(7)
        )
    }

    def apply(m: AnyMat2d) = new Mat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x3d) = new Mat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x4d) = new Mat4x2d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat3x2d) = new Mat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3d) = new Mat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3x4d) = new Mat4x2d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat4x2d) = new Mat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4x3d) = new Mat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4d) = new Mat4x2d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )
}
