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
sealed abstract class AnyMat3x2d
extends ConstRotationSubMat2d with ReadDoubleMat
{
    // Column major order.
    def m00: Double; def m10: Double; def m20: Double // column
    def m01: Double; def m11: Double; def m21: Double // column

    def rows = 3
    def columns = 2
    def toArray(array: Array[Double], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10
        array(offset + 2) = m20

        array(offset + 3) = m01
        array(offset + 4) = m11
        array(offset + 5) = m21
    }

    def apply(c: Int) :ConstVec3d = {
        c match {
            case 0 => new ConstVec3d(m00, m10, m20)
            case 1 => new ConstVec3d(m01, m11, m21)
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
                    case _ => error
                }
            case 1 =>
                r match {
                    case 0 => m01
                    case 1 => m11
                    case 2 => m21
                    case _ => error
                }
            case _ => error
        }
    }

    private[math] def negate(result: Mat3x2d) = {
        result.m00 = -m00
        result.m10 = -m10
        result.m20 = -m20

        result.m01 = -m01
        result.m11 = -m11
        result.m21 = -m21

        result
    }
    private[math] def mul(s: Double, result: Mat3x2d) = {
        result.m00 = s*m00
        result.m10 = s*m10
        result.m20 = s*m20

        result.m01 = s*m01
        result.m11 = s*m11
        result.m21 = s*m21

        result
    }
    private[math] def div(s: Double, result: Mat3x2d) = {
        val inv = 1/s

        result.m00 = inv*m00
        result.m10 = inv*m10
        result.m20 = inv*m20

        result.m01 = inv*m01
        result.m11 = inv*m11
        result.m21 = inv*m21

        result
    }

    private[math] def add(m: AnyMat3x2d, result:Mat3x2d) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10
        result.m20 = m20 + m.m20

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11
        result.m21 = m21 + m.m21

        result
    }
    private[math] def sub(m: AnyMat3x2d, result:Mat3x2d) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10
        result.m20 = m20 - m.m20

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11
        result.m21 = m21 - m.m21

        result
    }

    private[math] def div(m: AnyMat3x2d, result:Mat3x2d) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10
        result.m20 = m20 / m.m20

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11
        result.m21 = m21 / m.m21

        result
    }
    private[math] def divByComponent(s: Double, result:Mat3x2d) = {
        result.m00 = s / m00
        result.m10 = s / m10
        result.m20 = s / m20

        result.m01 = s / m01
        result.m11 = s / m11
        result.m21 = s / m21

        result
    }

    private[math] def mul(m: AnyMat2d, result: Mat3x2d) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10
        val a20 = m20*m.m00 + m21*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11
        val a21 = m20*m.m01 + m21*m.m11

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21

        result
    }
    private[math] def mul(m: AnyMat2x3d, result: Mat3d) = {
        result.m00 = m00*m.m00 + m01*m.m10
        result.m10 = m10*m.m00 + m11*m.m10
        result.m20 = m20*m.m00 + m21*m.m10

        result.m01 = m00*m.m01 + m01*m.m11
        result.m11 = m10*m.m01 + m11*m.m11
        result.m21 = m20*m.m01 + m21*m.m11

        result.m02 = m00*m.m02 + m01*m.m12
        result.m12 = m10*m.m02 + m11*m.m12
        result.m22 = m20*m.m02 + m21*m.m12

        result
    }
    private[math] def mul(m: AnyMat2x4d, result: Mat3x4d) = {
        result.m00 = m00*m.m00 + m01*m.m10
        result.m10 = m10*m.m00 + m11*m.m10
        result.m20 = m20*m.m00 + m21*m.m10

        result.m01 = m00*m.m01 + m01*m.m11
        result.m11 = m10*m.m01 + m11*m.m11
        result.m21 = m20*m.m01 + m21*m.m11

        result.m02 = m00*m.m02 + m01*m.m12
        result.m12 = m10*m.m02 + m11*m.m12
        result.m22 = m20*m.m02 + m21*m.m12

        result.m03 = m00*m.m03 + m01*m.m13
        result.m13 = m10*m.m03 + m11*m.m13
        result.m23 = m20*m.m03 + m21*m.m13

        result
    }

    private[math] def mul(x: Double, y: Double, result: Vec3d) = {
        result.x = m00*x + m01*y
        result.y = m10*x + m11*y
        result.z = m20*x + m21*y

        result
    }
    private[math] def transposeMul(x: Double, y: Double, z: Double, result: Vec2d) = {
        result.x = m00*x + m10*y + m20*z
        result.y = m01*x + m11*y + m21*z

        result
    }

    def unary_-() = negate(new Mat3x2d)
    def *(s: Double) = mul(s, new Mat3x2d)
    def /(s: Double) = div(s, new Mat3x2d)
    def +(m: AnyMat3x2d) = add(m, new Mat3x2d)
    def -(m: AnyMat3x2d) = sub(m, new Mat3x2d)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat3x2d) = div(m, new Mat3x2d)

    def *(m: AnyMat2d) = mul(m, new Mat3x2d)
    def *(m: AnyMat2x3d) = mul(m, new Mat3d)
    def *(m: AnyMat2x4d) = mul(m, new Mat3x4d)

    def *(u: AnyVec2d) = mul(u.x, u.y, new Vec3d)

    def ==(m: AnyMat3x2d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21
    }

    def !=(m: AnyMat3x2d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||
            isNaN(m20) || isInfinite(m20) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||
            isNaN(m21) || isInfinite(m21)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + "; " + 
            m01 + ", " + m11 + ", " + m21 +
        ")"
    }
}

final class ConstMat3x2d private[math] (
    val m00: Double, val m10: Double, val m20: Double,
    val m01: Double, val m11: Double, val m21: Double
) extends AnyMat3x2d

final class Mat3x2d private[math] (
    var m00: Double, var m10: Double, var m20: Double,
    var m01: Double, var m11: Double, var m21: Double
) extends AnyMat3x2d with RotationSubMat2d
{
    private[math] def this() = this(
        1, 0, 0,
        0, 1, 0
    )

    def *=(s: Double) { mul(s, this) }
    def /=(s: Double) { div(s, this) }

    def +=(m: AnyMat3x2d) { add(m, this) }
    def -=(m: AnyMat3x2d) { sub(m, this) }

    def *=(m: AnyMat2d) { mul(m, this) }

    def :=(m: AnyMat3x2d) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20;
        m01 = m.m01; m11 = m.m11; m21 = m.m21
    }

    def set(
        m00: Double, m10: Double, m20: Double,
        m01: Double, m11: Double, m21: Double
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20;
        this.m01 = m01; this.m11 = m11; this.m21 = m21
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
                    case _ => error
                }
            case 1 =>
                r match {
                    case 0 => m01 = s
                    case 1 => m11 = s
                    case 2 => m21 = s
                    case _ => error
                }
            case _ => error
        }
    }

    def update(c: Int, v: AnyVec3d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

}

object Mat3x2d {

    val Zero = const(Mat3x2d(0))
    val Identity = const(Mat3x2d(1))

    def apply(s: Double) = new Mat3x2d(
        s, 0, 0,
        0, s, 0
    )

    def apply(
        m00: Double, m10: Double, m20: Double,
        m01: Double, m11: Double, m21: Double
      ) = new Mat3x2d(
            m00, m10, m20,
            m01, m11, m21
      )

    def apply(c0: AnyVec3d, c1: AnyVec3d) = 
    new Mat3x2d(
        c0.x, c0.y, c0.z,
        c1.x, c1.y, c1.z
    )

    def apply(args: ReadAny[AnyVal]*) :Mat3x2d = {
        val mat = new Array[Double](6)
        mat(0) = 1
        mat(4) = 1

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

        if (index < 6) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat3x2d(
            mat(0), mat(1), mat(2),
            mat(3), mat(4), mat(5)
        )
    }

    def apply(m: ReadFloatMat) :Mat3x2d = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Float](rows*columns)
        m.toArray(array, 0)

        val n = new Mat3x2d
        val endr = if (rows < 3) rows else 3
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

    def apply(m: AnyMat2d) = new Mat3x2d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0
    )

    def apply(m: AnyMat2x3d) = new Mat3x2d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0
    )

    def apply(m: AnyMat2x4d) = new Mat3x2d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0
    )

    def apply(m: AnyMat3x2d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )

    def apply(m: AnyMat3d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )

    def apply(m: AnyMat3x4d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )

    def apply(m: AnyMat4x2d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )

    def apply(m: AnyMat4x3d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )

    def apply(m: AnyMat4d) = new Mat3x2d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21
    )
}
