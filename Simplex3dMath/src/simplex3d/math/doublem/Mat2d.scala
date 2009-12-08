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
sealed abstract class AnyMat2d
extends ConstRotationSubMat2d with ReadDoubleMat
{
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column

    def rows = 2
    def columns = 2
    def toArray(array: Array[Double], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10

        array(offset + 2) = m01
        array(offset + 3) = m11
    }

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

    private[math] def negate(result: Mat2d) = {
        result.m00 = -m00
        result.m10 = -m10

        result.m01 = -m01
        result.m11 = -m11

        result
    }
    private[math] def mul(s: Double, result: Mat2d) = {
        result.m00 = s*m00
        result.m10 = s*m10

        result.m01 = s*m01
        result.m11 = s*m11

        result
    }
    private[math] def div(s: Double, result: Mat2d) = {
        val inv = 1/s

        result.m00 = inv*m00
        result.m10 = inv*m10

        result.m01 = inv*m01
        result.m11 = inv*m11

        result
    }

    private[math] def add(m: AnyMat2d, result:Mat2d) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11

        result
    }
    private[math] def sub(m: AnyMat2d, result:Mat2d) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11

        result
    }

    private[math] def div(m: AnyMat2d, result:Mat2d) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11

        result
    }
    private[math] def divByComponent(s: Double, result:Mat2d) = {
        result.m00 = s / m00
        result.m10 = s / m10

        result.m01 = s / m01
        result.m11 = s / m11

        result
    }

    private[math] def mul(m: AnyMat2d, result: Mat2d) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11

        result
    }
    private[math] def mul(m: AnyMat2x3d, result: Mat2x3d) = {
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
    private[math] def mul(m: AnyMat2x4d, result: Mat2x4d) = {
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

    private[math] def mul(u: AnyVec2d, result: Vec2d) = {
        val x = m00*u.x + m01*u.y
        val y = m10*u.x + m11*u.y

        result.x = x; result.y = y

        result
    }
    private[math] def transposeMul(u: AnyVec2d, result: Vec2d) = {
        val x = m00*u.x + m10*u.y
        val y = m01*u.x + m11*u.y

        result.x = x; result.y = y

        result
    }

    def unary_-() = negate(new Mat2d)
    def *(s: Double) = mul(s, new Mat2d)
    def /(s: Double) = div(s, new Mat2d)
    def +(m: AnyMat2d) = add(m, new Mat2d)
    def -(m: AnyMat2d) = sub(m, new Mat2d)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2d) = div(m, new Mat2d)

    def *(m: AnyMat2d) = mul(m, new Mat2d)
    def *(m: AnyMat2x3d) = mul(m, new Mat2x3d)
    def *(m: AnyMat2x4d) = mul(m, new Mat2x4d)

    def *(u: AnyVec2d) = mul(u, new Vec2d)

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

final class Mat2d private[math] (
    var m00: Double, var m10: Double,
    var m01: Double, var m11: Double
) extends AnyMat2d with RotationSubMat2d
{
    private[math] def this() = this(
        1, 0,
        0, 1
    )

    def *=(s: Double) { mul(s, this) }
    def /=(s: Double) { div(s, this) }

    def +=(m: AnyMat2d) { add(m, this) }
    def -=(m: AnyMat2d) { sub(m, this) }

    def *=(m: AnyMat2d) { mul(m, this) }

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

    val Zero = const(Mat2d(0))
    val Identity = const(Mat2d(1))

    def apply(s: Double) = new Mat2d(
        s, 0,
        0, s
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

    def apply(args: ReadAny[AnyVal]*) :Mat2d = {
        val mat = new Array[Double](4)
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

        new Mat2d(
            mat(0), mat(1),
            mat(2), mat(3)
        )
    }

    def apply(m: ReadFloatMat) :Mat2d = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Float](rows*columns)
        m.toArray(array, 0)

        val n = new Mat2d
        val endr = if (rows < 2) rows else 2
        val endc = if (columns < 2) columns else 2

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
}
