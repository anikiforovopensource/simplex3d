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
import Read._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat2x4d
extends ReadDoubleMat
{
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column
    def m02: Double; def m12: Double // column
    def m03: Double; def m13: Double // column

    def rows = 2
    def columns = 4
    def toArray(array: Array[Double], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10

        array(offset + 2) = m01
        array(offset + 3) = m11

        array(offset + 4) = m02
        array(offset + 5) = m12

        array(offset + 6) = m03
        array(offset + 7) = m13
    }

    def apply(c: Int) :ConstVec2d = {
        c match {
            case 0 => new ConstVec2d(m00, m10)
            case 1 => new ConstVec2d(m01, m11)
            case 2 => new ConstVec2d(m02, m12)
            case 3 => new ConstVec2d(m03, m13)
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
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
            case 3 =>
                r match {
                    case 0 => m03
                    case 1 => m13
                    case _ => error
                }
            case _ => error
        }
    }

    def unary_-() = Mat2x4d(
        -m00, -m10,
        -m01, -m11,
        -m02, -m12,
        -m03, -m13
    )
    def *(s: Double) = Mat2x4d(
        s*m00, s*m10,
        s*m01, s*m11,
        s*m02, s*m12,
        s*m03, s*m13
    )
    def /(s: Double) = { val inv = 1/s; Mat2x4d(
        inv*m00, inv*m10,
        inv*m01, inv*m11,
        inv*m02, inv*m12,
        inv*m03, inv*m13
    )}

    def +(m: AnyMat2x4d) = Mat2x4d(
        m00 + m.m00, m10 + m.m10,
        m01 + m.m01, m11 + m.m11,
        m02 + m.m02, m12 + m.m12,
        m03 + m.m03, m13 + m.m13
    )
    def -(m: AnyMat2x4d) = Mat2x4d(
        m00 - m.m00, m10 - m.m10,
        m01 - m.m01, m11 - m.m11,
        m02 - m.m02, m12 - m.m12,
        m03 - m.m03, m13 - m.m13
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2x4d) = Mat2x4d(
        m00/m.m00, m10/m.m10,
        m01/m.m01, m11/m.m11,
        m02/m.m02, m12/m.m12,
        m03/m.m03, m13/m.m13
    )
    private[math] def divideByComponent(s: Double) = Mat2x4d(
        s/m00, s/m10,
        s/m01, s/m11,
        s/m02, s/m12,
        s/m03, s/m13
    )

    def *(m: AnyMat4x2d) = Mat2d(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
    )
    def *(m: AnyMat4x3d) = Mat2x3d(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

        m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
        m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
    )
    def *(m: AnyMat4d) = Mat2x4d(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,

        m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
        m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,

        m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
        m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
    )

    def *(u: AnyVec4d) = Vec2d(
        m00*u.x + m01*u.y + m02*u.z + m03*u.w,
        m10*u.x + m11*u.y + m12*u.z + m13*u.w
    )
    protected[math] def transposeMul(u: AnyVec2d) = Vec4d(
        m00*u.x + m10*u.y,
        m01*u.x + m11*u.y,
        m02*u.x + m12*u.y,
        m03*u.x + m13*u.y
    )

    def ==(m: AnyMat2x4d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 &&
            m01 == m.m01 && m11 == m.m11 &&
            m02 == m.m02 && m12 == m.m12 &&
            m03 == m.m03 && m13 == m.m13
    }

    def !=(m: AnyMat2x4d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||

            isNaN(m02) || isInfinite(m02) ||
            isNaN(m12) || isInfinite(m12) ||

            isNaN(m03) || isInfinite(m03) ||
            isNaN(m13) || isInfinite(m13)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + "; " + 
            m01 + ", " + m11 + "; " + 
            m02 + ", " + m12 + "; " + 
            m03 + ", " + m13 +
        ")"
    }
}

final class ConstMat2x4d private[math] (
    val m00: Double, val m10: Double,
    val m01: Double, val m11: Double,
    val m02: Double, val m12: Double,
    val m03: Double, val m13: Double
) extends AnyMat2x4d

final class Mat2x4d private[math] (
    var m00: Double, var m10: Double,
    var m01: Double, var m11: Double,
    var m02: Double, var m12: Double,
    var m03: Double, var m13: Double
) extends AnyMat2x4d
{
    def *=(s: Double) {
        m00 *= s; m10 *= s;
        m01 *= s; m11 *= s;
        m02 *= s; m12 *= s;
        m03 *= s; m13 *= s
    }
    def /=(s: Double) { val inv = 1/s;
        m00 *= inv; m10 *= inv;
        m01 *= inv; m11 *= inv;
        m02 *= inv; m12 *= inv;
        m03 *= inv; m13 *= inv
    }

    def +=(m: AnyMat2x4d) {
        m00 += m.m00; m10 += m.m10;
        m01 += m.m01; m11 += m.m11;
        m02 += m.m02; m12 += m.m12;
        m03 += m.m03; m13 += m.m13
    }
    def -=(m: AnyMat2x4d) {
        m00 -= m.m00; m10 -= m.m10;
        m01 -= m.m01; m11 -= m.m11;
        m02 -= m.m02; m12 -= m.m12;
        m03 -= m.m03; m13 -= m.m13
    }

    def *=(m: AnyMat4d) {
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

    def :=(m: AnyMat2x4d) {
        m00 = m.m00; m10 = m.m10;
        m01 = m.m01; m11 = m.m11;
        m02 = m.m02; m12 = m.m12;
        m03 = m.m03; m13 = m.m13
    }

    def set(
        m00: Double, m10: Double,
        m01: Double, m11: Double,
        m02: Double, m12: Double,
        m03: Double, m13: Double
    ) {
        this.m00 = m00; this.m10 = m10;
        this.m01 = m01; this.m11 = m11;
        this.m02 = m02; this.m12 = m12;
        this.m03 = m03; this.m13 = m13
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
            case 3 =>
                r match {
                    case 0 => m03 = s
                    case 1 => m13 = s
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
            case 3 => m03 = v.x; m13 = v.y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

}

object Mat2x4d {

    val Zero = const(Mat2x4d(0))
    val Identity = const(Mat2x4d(1))

    def apply(s: Double) = new Mat2x4d(
        s, 0,
        0, s,
        0, 0,
        0, 0
    )

    def apply(
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

    def apply(c0: AnyVec2d, c1: AnyVec2d, c2: AnyVec2d, c3: AnyVec2d) = 
    new Mat2x4d(
        c0.x, c0.y,
        c1.x, c1.y,
        c2.x, c2.y,
        c3.x, c3.y
    )

    def apply(args: ReadAny[AnyVal]*) :Mat2x4d = {
        val mat = new Array[Double](8)
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

        if (index < 8) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat2x4d(
            mat(0), mat(1),
            mat(2), mat(3),
            mat(4), mat(5),
            mat(6), mat(7)
        )
    }

    def apply(m: ReadFloatMat) :Mat2x4d = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Float](rows*columns)
        m.toArray(array, 0)

        val n = apply(1)
        val endr = if (rows < 2) rows else 2
        val endc = if (columns < 4) columns else 4

        var c = 0; while (c < endc) {
            val offset = c*rows
            var r = 0; while (r < endr) {
                n(c, r) = array(offset + r)
                r += 1
            }
            c += 1
        }
        n
    }

    def apply(m: AnyMat2d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0,
        0, 0
    )

    def apply(m: AnyMat2x3d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        0, 0
    )

    def apply(m: AnyMat2x4d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        m.m03, m.m13
    )

    def apply(m: AnyMat3x2d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0,
        0, 0
    )

    def apply(m: AnyMat3d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        0, 0
    )

    def apply(m: AnyMat3x4d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        m.m03, m.m13
    )

    def apply(m: AnyMat4x2d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        0, 0,
        0, 0
    )

    def apply(m: AnyMat4x3d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        0, 0
    )

    def apply(m: AnyMat4d) = new Mat2x4d(
        m.m00, m.m10,
        m.m01, m.m11,
        m.m02, m.m12,
        m.m03, m.m13
    )
}
