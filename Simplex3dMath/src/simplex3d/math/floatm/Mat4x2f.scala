/*
 * Simplex3D, Math module
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

package simplex3d.math.floatm

import simplex3d.math._
import Read._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat4x2f
{
    // Column major order.
    def m00: Float; def m10: Float; def m20: Float; def m30: Float // column
    def m01: Float; def m11: Float; def m21: Float; def m31: Float // column

    def apply(c: Int) :ConstVec4f = {
        c match {
            case 0 => ConstVec4f(m00, m10, m20, m30)
            case 1 => ConstVec4f(m01, m11, m21, m31)
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def apply(c: Int, r: Int) :Float = {
        def error() :Float = {
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

    def unary_-() = Mat4x2f(
        -m00, -m10, -m20, -m30,
        -m01, -m11, -m21, -m31
    )
    def *(s: Float) = Mat4x2f(
        s*m00, s*m10, s*m20, s*m30,
        s*m01, s*m11, s*m21, s*m31
    )
    def /(s: Float) = { val inv = 1/s; Mat4x2f(
        inv*m00, inv*m10, inv*m20, inv*m30,
        inv*m01, inv*m11, inv*m21, inv*m31
    )}

    def +(m: AnyMat4x2f) = Mat4x2f(
        m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
        m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31
    )
    def -(m: AnyMat4x2f) = Mat4x2f(
        m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
        m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat4x2f) = Mat4x2f(
        m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
        m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31
    )
    private[math] def divideByComponent(s: Float) = Mat4x2f(
        s/m00, s/m10, s/m20, s/m30,
        s/m01, s/m11, s/m21, s/m31
    )

    def *(m: AnyMat2f) = Mat4x2f(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,
        m20*m.m00 + m21*m.m10,
        m30*m.m00 + m31*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,
        m20*m.m01 + m21*m.m11,
        m30*m.m01 + m31*m.m11
    )
    def *(m: AnyMat2x3f) = Mat4x3f(
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
    def *(m: AnyMat2x4f) = Mat4f(
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

    def *(u: AnyVec2f) = Vec4f(
        m00*u.x + m01*u.y,
        m10*u.x + m11*u.y,
        m20*u.x + m21*u.y,
        m30*u.x + m31*u.y
    )
    protected[math] def transposeMul(u: AnyVec4f) = Vec2f(
        m00*u.x + m10*u.y + m20*u.z + m30*u.w,
        m01*u.x + m11*u.y + m21*u.z + m31*u.w
    )

    def ==(m: AnyMat4x2f) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 && m30 == m.m30 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 && m31 == m.m31
    }

    def !=(m: AnyMat4x2f) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._

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

final class ConstMat4x2f private (
    val m00: Float, val m10: Float, val m20: Float, val m30: Float,
    val m01: Float, val m11: Float, val m21: Float, val m31: Float
) extends AnyMat4x2f

object ConstMat4x2f {

    def apply(s: Float) = new ConstMat4x2f(
        s, 0, 0, 0,
        0, s, 0, 0
    )

    def apply(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float
      ) = new ConstMat4x2f(
            m00, m10, m20, m30,
            m01, m11, m21, m31
      )

    def apply(args: ReadAny[Float]*) :ConstMat4x2f = {
        val mat = new Array[Float](8)
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

        new ConstMat4x2f(
            mat(0), mat(1), mat(2), mat(3),
            mat(4), mat(5), mat(6), mat(7)
        )
    }

    def apply(m: AnyMat2f) = new ConstMat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x3f) = new ConstMat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x4f) = new ConstMat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat3x2f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3x4f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat4x2f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4x3f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4f) = new ConstMat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    implicit def mutableToConst(m: Mat4x2f) = ConstMat4x2f(m)
}


final class Mat4x2f private (
    var m00: Float, var m10: Float, var m20: Float, var m30: Float,
    var m01: Float, var m11: Float, var m21: Float, var m31: Float
) extends AnyMat4x2f
{
    def *=(s: Float) {
        m00 *= s; m10 *= s; m20 *= s; m30 *= s;
        m01 *= s; m11 *= s; m21 *= s; m31 *= s
    }
    def /=(s: Float) { val inv = 1/s;
        m00 *= inv; m10 *= inv; m20 *= inv; m30 *= inv;
        m01 *= inv; m11 *= inv; m21 *= inv; m31 *= inv
    }

    def +=(m: AnyMat4x2f) {
        m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
        m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31
    }
    def -=(m: AnyMat4x2f) {
        m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
        m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31
    }

    def *=(m: AnyMat2f) {
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

    def :=(m: AnyMat4x2f) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
        m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31
    }

    def set(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20; this.m30 = m30;
        this.m01 = m01; this.m11 = m11; this.m21 = m21; this.m31 = m31
    }

    def update(c: Int, r: Int, s: Float) {
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

    def update(c: Int, v: AnyVec4f) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

}

object Mat4x2f {

    def apply(s: Float) = new Mat4x2f(
        s, 0, 0, 0,
        0, s, 0, 0
    )

    def apply(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float
      ) = new Mat4x2f(
            m00, m10, m20, m30,
            m01, m11, m21, m31
      )

    def apply(args: ReadAny[Float]*) :Mat4x2f = {
        val mat = new Array[Float](8)
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

        new Mat4x2f(
            mat(0), mat(1), mat(2), mat(3),
            mat(4), mat(5), mat(6), mat(7)
        )
    }

    def apply(m: AnyMat2f) = new Mat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x3f) = new Mat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat2x4f) = new Mat4x2f(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0
    )

    def apply(m: AnyMat3x2f) = new Mat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3f) = new Mat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat3x4f) = new Mat4x2f(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0
    )

    def apply(m: AnyMat4x2f) = new Mat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4x3f) = new Mat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )

    def apply(m: AnyMat4f) = new Mat4x2f(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31
    )
}
