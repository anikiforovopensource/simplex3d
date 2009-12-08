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
sealed abstract class AnyMat3d
extends ConstRotationSubMat3d with ConstRotationSubMat2d with ReadDoubleMat
{
    // Column major order.
    def m00: Double; def m10: Double; def m20: Double // column
    def m01: Double; def m11: Double; def m21: Double // column
    def m02: Double; def m12: Double; def m22: Double // column

    def rows = 3
    def columns = 3
    def toArray(array: Array[Double], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10
        array(offset + 2) = m20

        array(offset + 3) = m01
        array(offset + 4) = m11
        array(offset + 5) = m21

        array(offset + 6) = m02
        array(offset + 7) = m12
        array(offset + 8) = m22
    }

    def apply(c: Int) :ConstVec3d = {
        c match {
            case 0 => new ConstVec3d(m00, m10, m20)
            case 1 => new ConstVec3d(m01, m11, m21)
            case 2 => new ConstVec3d(m02, m12, m22)
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
            case 2 =>
                r match {
                    case 0 => m02
                    case 1 => m12
                    case 2 => m22
                    case _ => error
                }
            case _ => error
        }
    }

    private[math] def negate(result: Mat3d) = {
        result.m00 = -m00
        result.m10 = -m10
        result.m20 = -m20

        result.m01 = -m01
        result.m11 = -m11
        result.m21 = -m21

        result.m02 = -m02
        result.m12 = -m12
        result.m22 = -m22

        result
    }
    private[math] def mul(s: Double, result: Mat3d) = {
        result.m00 = s*m00
        result.m10 = s*m10
        result.m20 = s*m20

        result.m01 = s*m01
        result.m11 = s*m11
        result.m21 = s*m21

        result.m02 = s*m02
        result.m12 = s*m12
        result.m22 = s*m22

        result
    }
    private[math] def div(s: Double, result: Mat3d) = {
        val inv = 1/s

        result.m00 = inv*m00
        result.m10 = inv*m10
        result.m20 = inv*m20

        result.m01 = inv*m01
        result.m11 = inv*m11
        result.m21 = inv*m21

        result.m02 = inv*m02
        result.m12 = inv*m12
        result.m22 = inv*m22

        result
    }

    private[math] def add(m: AnyMat3d, result:Mat3d) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10
        result.m20 = m20 + m.m20

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11
        result.m21 = m21 + m.m21

        result.m02 = m02 + m.m02
        result.m12 = m12 + m.m12
        result.m22 = m22 + m.m22

        result
    }
    private[math] def sub(m: AnyMat3d, result:Mat3d) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10
        result.m20 = m20 - m.m20

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11
        result.m21 = m21 - m.m21

        result.m02 = m02 - m.m02
        result.m12 = m12 - m.m12
        result.m22 = m22 - m.m22

        result
    }

    private[math] def div(m: AnyMat3d, result:Mat3d) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10
        result.m20 = m20 / m.m20

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11
        result.m21 = m21 / m.m21

        result.m02 = m02 / m.m02
        result.m12 = m12 / m.m12
        result.m22 = m22 / m.m22

        result
    }
    private[math] def divByComponent(s: Double, result:Mat3d) = {
        result.m00 = s / m00
        result.m10 = s / m10
        result.m20 = s / m20

        result.m01 = s / m01
        result.m11 = s / m11
        result.m21 = s / m21

        result.m02 = s / m02
        result.m12 = s / m12
        result.m22 = s / m22

        result
    }

    private[math] def mul(m: AnyMat3x2d, result: Mat3x2d) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21

        result
    }
    private[math] def mul(m: AnyMat3d, result: Mat3d) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21
        result.m02 = a02; result.m12 = a12; result.m22 = a22

        result
    }
    private[math] def mul(m: AnyMat3x4d, result: Mat3x4d) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21
        result.m02 = a02; result.m12 = a12; result.m22 = a22
        result.m03 = a03; result.m13 = a13; result.m23 = a23

        result
    }

    private[math] def mul(u: AnyVec3d, result: Vec3d) = {
        val x = m00*u.x + m01*u.y + m02*u.z
        val y = m10*u.x + m11*u.y + m12*u.z
        val z = m20*u.x + m21*u.y + m22*u.z

        result.x = x; result.y = y; result.z = z

        result
    }
    private[math] def transposeMul(u: AnyVec3d, result: Vec3d) = {
        val x = m00*u.x + m10*u.y + m20*u.z
        val y = m01*u.x + m11*u.y + m21*u.z
        val z = m02*u.x + m12*u.y + m22*u.z

        result.x = x; result.y = y; result.z = z

        result
    }

    def unary_-() = negate(new Mat3d)
    def *(s: Double) = mul(s, new Mat3d)
    def /(s: Double) = div(s, new Mat3d)
    def +(m: AnyMat3d) = add(m, new Mat3d)
    def -(m: AnyMat3d) = sub(m, new Mat3d)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat3d) = div(m, new Mat3d)

    def *(m: AnyMat3x2d) = mul(m, new Mat3x2d)
    def *(m: AnyMat3d) = mul(m, new Mat3d)
    def *(m: AnyMat3x4d) = mul(m, new Mat3x4d)

    def *(u: AnyVec3d) = mul(u, new Vec3d)

    def ==(m: AnyMat3d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 &&
            m02 == m.m02 && m12 == m.m12 && m22 == m.m22
    }

    def !=(m: AnyMat3d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||
            isNaN(m20) || isInfinite(m20) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||
            isNaN(m21) || isInfinite(m21) ||

            isNaN(m02) || isInfinite(m02) ||
            isNaN(m12) || isInfinite(m12) ||
            isNaN(m22) || isInfinite(m22)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + "; " + 
            m01 + ", " + m11 + ", " + m21 + "; " + 
            m02 + ", " + m12 + ", " + m22 +
        ")"
    }
}

final class ConstMat3d private[math] (
    val m00: Double, val m10: Double, val m20: Double,
    val m01: Double, val m11: Double, val m21: Double,
    val m02: Double, val m12: Double, val m22: Double
) extends AnyMat3d

final class Mat3d private[math] (
    var m00: Double, var m10: Double, var m20: Double,
    var m01: Double, var m11: Double, var m21: Double,
    var m02: Double, var m12: Double, var m22: Double
) extends AnyMat3d with RotationSubMat3d with RotationSubMat2d
{
    private[math] def this() = this(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1
    )

    def *=(s: Double) { mul(s, this) }
    def /=(s: Double) { div(s, this) }

    def +=(m: AnyMat3d) { add(m, this) }
    def -=(m: AnyMat3d) { sub(m, this) }

    def *=(m: AnyMat3d) { mul(m, this) }

    def :=(m: AnyMat3d) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20;
        m01 = m.m01; m11 = m.m11; m21 = m.m21;
        m02 = m.m02; m12 = m.m12; m22 = m.m22
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
            case 2 =>
                r match {
                    case 0 => m02 = s
                    case 1 => m12 = s
                    case 2 => m22 = s
                    case _ => error
                }
            case _ => error
        }
    }

    def update(c: Int, v: AnyVec3d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z
            case 2 => m02 = v.x; m12 = v.y; m22 = v.z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

}

object Mat3d {

    val Zero = const(Mat3d(0))
    val Identity = const(Mat3d(1))

    def apply(s: Double) = new Mat3d(
        s, 0, 0,
        0, s, 0,
        0, 0, s
    )

    def apply(
        m00: Double, m10: Double, m20: Double,
        m01: Double, m11: Double, m21: Double,
        m02: Double, m12: Double, m22: Double
      ) = new Mat3d(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22
      )

    def apply(c0: AnyVec3d, c1: AnyVec3d, c2: AnyVec3d) = 
    new Mat3d(
        c0.x, c0.y, c0.z,
        c1.x, c1.y, c1.z,
        c2.x, c2.y, c2.z
    )

    def apply(args: ReadAny[AnyVal]*) :Mat3d = {
        val mat = new Array[Double](9)
        mat(0) = 1
        mat(4) = 1
        mat(8) = 1

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

        if (index < 9) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat3d(
            mat(0), mat(1), mat(2),
            mat(3), mat(4), mat(5),
            mat(6), mat(7), mat(8)
        )
    }

    def apply(m: ReadFloatMat) :Mat3d = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Float](rows*columns)
        m.toArray(array, 0)

        val n = new Mat3d
        val endr = if (rows < 3) rows else 3
        val endc = if (columns < 3) columns else 3

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

    def apply(m: AnyMat2d) = new Mat3d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        0, 0, 1
    )

    def apply(m: AnyMat2x3d) = new Mat3d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        m.m02, m.m12, 1
    )

    def apply(m: AnyMat2x4d) = new Mat3d(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        m.m02, m.m12, 1
    )

    def apply(m: AnyMat3x2d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        0, 0, 1
    )

    def apply(m: AnyMat3d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22
    )

    def apply(m: AnyMat3x4d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22
    )

    def apply(m: AnyMat4x2d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        0, 0, 1
    )

    def apply(m: AnyMat4x3d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22
    )

    def apply(m: AnyMat4d) = new Mat3d(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22
    )
}
