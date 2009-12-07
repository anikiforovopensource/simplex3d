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
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.FloatMath._
import Read._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat2f
extends ConstRotationSubMat2f with ReadFloatMat
{
    // Column major order.
    def m00: Float; def m10: Float // column
    def m01: Float; def m11: Float // column

    def rows = 2
    def columns = 2
    def toArray(array: Array[Float], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10

        array(offset + 2) = m01
        array(offset + 3) = m11
    }

    def apply(c: Int) :ConstVec2f = {
        c match {
            case 0 => new ConstVec2f(m00, m10)
            case 1 => new ConstVec2f(m01, m11)
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

    private[math] def negate(result: Mat2f) = {
        result.m00 = -m00
        result.m10 = -m10

        result.m01 = -m01
        result.m11 = -m11

        result
    }
    private[math] def mul(s: Float, result: Mat2f) = {
        result.m00 = s*m00
        result.m10 = s*m10

        result.m01 = s*m01
        result.m11 = s*m11

        result
    }
    private[math] def div(s: Float, result: Mat2f) = {
        val inv = 1/s

        result.m00 = inv*m00
        result.m10 = inv*m10

        result.m01 = inv*m01
        result.m11 = inv*m11

        result
    }

    private[math] def add(m: AnyMat2f, result:Mat2f) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11

        result
    }
    private[math] def sub(m: AnyMat2f, result:Mat2f) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11

        result
    }

    private[math] def div(m: AnyMat2f, result:Mat2f) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11

        result
    }
    private[math] def divByComponent(s: Float, result:Mat2f) = {
        result.m00 = s / m00
        result.m10 = s / m10

        result.m01 = s / m01
        result.m11 = s / m11

        result
    }

    private[math] def mul(m: AnyMat2f, result: Mat2f) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11

        result
    }
    private[math] def mul(m: AnyMat2x3f, result: Mat2x3f) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        val a02 = m00*m.m02 + m01*m.m12
        val a12 = m10*m.m02 + m11*m.m12

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11
        result.m02 = a02; result.m12 = a12

        result
    }
    private[math] def mul(m: AnyMat2x4f, result: Mat2x4f) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        val a02 = m00*m.m02 + m01*m.m12
        val a12 = m10*m.m02 + m11*m.m12

        val a03 = m00*m.m03 + m01*m.m13
        val a13 = m10*m.m03 + m11*m.m13

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11
        result.m02 = a02; result.m12 = a12
        result.m03 = a03; result.m13 = a13

        result
    }

    private[math] def mul(x: Float, y: Float, result: Vec2f) = {
        result.x = m00*x + m01*y
        result.y = m10*x + m11*y

        result
    }
    private[math] def transposeMul(x: Float, y: Float, result: Vec2f) = {
        result.x = m00*x + m10*y
        result.y = m01*x + m11*y

        result
    }

    def unary_-() = negate(new Mat2f)
    def *(s: Float) = mul(s, new Mat2f)
    def /(s: Float) = div(s, new Mat2f)
    def +(m: AnyMat2f) = add(m, new Mat2f)
    def -(m: AnyMat2f) = sub(m, new Mat2f)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2f) = div(m, new Mat2f)

    def *(m: AnyMat2f) = mul(m, new Mat2f)
    def *(m: AnyMat2x3f) = mul(m, new Mat2x3f)
    def *(m: AnyMat2x4f) = mul(m, new Mat2x4f)

    def *(u: AnyVec2f) = mul(u.x, u.y, new Vec2f)

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

final class ConstMat2f private[math] (
    val m00: Float, val m10: Float,
    val m01: Float, val m11: Float
) extends AnyMat2f

final class Mat2f private[math] (
    var m00: Float, var m10: Float,
    var m01: Float, var m11: Float
) extends AnyMat2f with RotationSubMat2f
{
    private[math] def this() = this(
        1, 0,
        0, 1
    )

    def *=(s: Float) { mul(s, this) }
    def /=(s: Float) { div(s, this) }

    def +=(m: AnyMat2f) { add(m, this) }
    def -=(m: AnyMat2f) { sub(m, this) }

    def *=(m: AnyMat2f) { mul(m, this) }

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

    val Zero = const(Mat2f(0))
    val Identity = const(Mat2f(1))

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

    def apply(c0: AnyVec2f, c1: AnyVec2f) = 
    new Mat2f(
        c0.x, c0.y,
        c1.x, c1.y
    )

    def apply(args: ReadAny[AnyVal]*) :Mat2f = {
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

    def apply(m: ReadDoubleMat) :Mat2f = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Double](rows*columns)
        m.toArray(array, 0)

        val n = new Mat2f
        val endr = if (rows < 2) rows else 2
        val endc = if (columns < 2) columns else 2

        var c = 0; while (c < endc) {
            val offset = c*rows
            var r = 0; while (r < endr) {
                n(c, r) = float(array(offset + r))
                r += 1
            }
            c += 1
        }
        n
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
