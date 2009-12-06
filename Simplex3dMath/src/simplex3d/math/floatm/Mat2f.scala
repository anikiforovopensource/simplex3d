/*
 * Simplex3D, FloatMath module
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
sealed abstract class AnyMat2f
extends ConstRotationSubMat2f
{
    // Column major order.
    def m00: Float; def m10: Float // column
    def m01: Float; def m11: Float // column

    def apply(c: Int) :ConstVec2f = {
        c match {
            case 0 => ConstVec2f(m00, m10)
            case 1 => ConstVec2f(m01, m11)
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

    def unary_-() = Mat2f(
        -m00, -m10,
        -m01, -m11
    )
    def *(s: Float) = Mat2f(
        s*m00, s*m10,
        s*m01, s*m11
    )
    def /(s: Float) = { val inv = 1/s; Mat2f(
        inv*m00, inv*m10,
        inv*m01, inv*m11
    )}

    def +(m: AnyMat2f) = Mat2f(
        m00 + m.m00, m10 + m.m10,
        m01 + m.m01, m11 + m.m11
    )
    def -(m: AnyMat2f) = Mat2f(
        m00 - m.m00, m10 - m.m10,
        m01 - m.m01, m11 - m.m11
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2f) = Mat2f(
        m00/m.m00, m10/m.m10,
        m01/m.m01, m11/m.m11
    )
    private[math] def divideByComponent(s: Float) = Mat2f(
        s/m00, s/m10,
        s/m01, s/m11
    )

    def *(m: AnyMat2f) = Mat2f(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11
    )
    def *(m: AnyMat2x3f) = Mat2x3f(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m00*m.m02 + m01*m.m12,
        m10*m.m02 + m11*m.m12
    )
    def *(m: AnyMat2x4f) = Mat2x4f(
        m00*m.m00 + m01*m.m10,
        m10*m.m00 + m11*m.m10,

        m00*m.m01 + m01*m.m11,
        m10*m.m01 + m11*m.m11,

        m00*m.m02 + m01*m.m12,
        m10*m.m02 + m11*m.m12,

        m00*m.m03 + m01*m.m13,
        m10*m.m03 + m11*m.m13
    )

    def *(u: AnyVec2f) = Vec2f(
        m00*u.x + m01*u.y,
        m10*u.x + m11*u.y
    )
    protected[math] def transposeMul(u: AnyVec2f) = Vec2f(
        m00*u.x + m10*u.y,
        m01*u.x + m11*u.y
    )

    def ==(m: AnyMat2f) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 &&
            m01 == m.m01 && m11 == m.m11
    }

    def !=(m: AnyMat2f) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._

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

final class ConstMat2f private (
    val m00: Float, val m10: Float,
    val m01: Float, val m11: Float
) extends AnyMat2f

object ConstMat2f {

    def apply(s: Float) = new ConstMat2f(
        s, 0,
        0, s
    )

    def apply(
        m00: Float, m10: Float,
        m01: Float, m11: Float
      ) = new ConstMat2f(
            m00, m10,
            m01, m11
      )

    def apply(args: ReadAny[Float]*) :ConstMat2f = {
        val mat = new Array[Float](4)
        mat(0) = 1
        mat(3) = 1

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

        if (index < 4) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new ConstMat2f(
            mat(0), mat(1),
            mat(2), mat(3)
        )
    }

    def apply(m: AnyMat2f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x3f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x4f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x2f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x4f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x2f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x3f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4f) = new ConstMat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    implicit def mutableToConst(m: Mat2f) = ConstMat2f(m)
}


final class Mat2f private (
    var m00: Float, var m10: Float,
    var m01: Float, var m11: Float
) extends AnyMat2f with RotationSubMat2f
{
    def *=(s: Float) {
        m00 *= s; m10 *= s;
        m01 *= s; m11 *= s
    }
    def /=(s: Float) { val inv = 1/s;
        m00 *= inv; m10 *= inv;
        m01 *= inv; m11 *= inv
    }

    def +=(m: AnyMat2f) {
        m00 += m.m00; m10 += m.m10;
        m01 += m.m01; m11 += m.m11
    }
    def -=(m: AnyMat2f) {
        m00 -= m.m00; m10 -= m.m10;
        m01 -= m.m01; m11 -= m.m11
    }

    def *=(m: AnyMat2f) {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        m00 = a00; m10 = a10
        m01 = a01; m11 = a11
    }

    def :=(m: AnyMat2f) {
        m00 = m.m00; m10 = m.m10;
        m01 = m.m01; m11 = m.m11
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

    def update(c: Int, v: AnyVec2f) {
        c match {
            case 0 => m00 = v.x; m10 = v.y
            case 1 => m01 = v.x; m11 = v.y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

}

object Mat2f {

    def apply(s: Float) = new Mat2f(
        s, 0,
        0, s
    )

    def apply(
        m00: Float, m10: Float,
        m01: Float, m11: Float
      ) = new Mat2f(
            m00, m10,
            m01, m11
      )

    def apply(args: ReadAny[Float]*) :Mat2f = {
        val mat = new Array[Float](4)
        mat(0) = 1
        mat(3) = 1

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

        if (index < 4) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat2f(
            mat(0), mat(1),
            mat(2), mat(3)
        )
    }

    def apply(m: AnyMat2f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x3f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat2x4f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x2f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat3x4f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x2f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4x3f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )

    def apply(m: AnyMat4f) = new Mat2f(
        m.m00, m.m10,
        m.m01, m.m11
    )
}
