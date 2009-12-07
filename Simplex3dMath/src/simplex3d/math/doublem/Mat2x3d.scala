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
sealed abstract class AnyMat2x3d
extends ConstRotationSubMat2d with ReadDoubleMat
{
    // Column major order.
    def m00: Double; def m10: Double // column
    def m01: Double; def m11: Double // column
    def m02: Double; def m12: Double // column

    def rows = 2
    def columns = 3
    def toArray(array: Array[Double], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10

        array(offset + 2) = m01
        array(offset + 3) = m11

        array(offset + 4) = m02
        array(offset + 5) = m12
    }

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

    private[math] def negate(result: Mat2x3d) = {
        result.m00 = -m00
        result.m10 = -m10

        result.m01 = -m01
        result.m11 = -m11

        result.m02 = -m02
        result.m12 = -m12

        result
    }
    private[math] def mul(s: Double, result: Mat2x3d) = {
        result.m00 = s*m00
        result.m10 = s*m10

        result.m01 = s*m01
        result.m11 = s*m11

        result.m02 = s*m02
        result.m12 = s*m12

        result
    }
    private[math] def div(s: Double, result: Mat2x3d) = {
        val inv = 1/s

        result.m00 = inv*m00
        result.m10 = inv*m10

        result.m01 = inv*m01
        result.m11 = inv*m11

        result.m02 = inv*m02
        result.m12 = inv*m12

        result
    }

    private[math] def add(m: AnyMat2x3d, result:Mat2x3d) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11

        result.m02 = m02 + m.m02
        result.m12 = m12 + m.m12

        result
    }
    private[math] def sub(m: AnyMat2x3d, result:Mat2x3d) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11

        result.m02 = m02 - m.m02
        result.m12 = m12 - m.m12

        result
    }

    private[math] def div(m: AnyMat2x3d, result:Mat2x3d) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11

        result.m02 = m02 / m.m02
        result.m12 = m12 / m.m12

        result
    }
    private[math] def divByComponent(s: Double, result:Mat2x3d) = {
        result.m00 = s / m00
        result.m10 = s / m10

        result.m01 = s / m01
        result.m11 = s / m11

        result.m02 = s / m02
        result.m12 = s / m12

        result
    }

    private[math] def mul(m: AnyMat3x2d, result: Mat2d) = {
        result.m00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        result.m10 = m10*m.m00 + m11*m.m10 + m12*m.m20

        result.m01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        result.m11 = m10*m.m01 + m11*m.m11 + m12*m.m21

        result
    }
    private[math] def mul(m: AnyMat3d, result: Mat2x3d) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11
        result.m02 = a02; result.m12 = a12

        result
    }
    private[math] def mul(m: AnyMat3x4d, result: Mat2x4d) = {
        result.m00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        result.m10 = m10*m.m00 + m11*m.m10 + m12*m.m20

        result.m01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        result.m11 = m10*m.m01 + m11*m.m11 + m12*m.m21

        result.m02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        result.m12 = m10*m.m02 + m11*m.m12 + m12*m.m22

        result.m03 = m00*m.m03 + m01*m.m13 + m02*m.m23
        result.m13 = m10*m.m03 + m11*m.m13 + m12*m.m23

        result
    }

    private[math] def mul(x: Double, y: Double, z: Double, result: Vec2d) = {
        result.x = m00*x + m01*y + m02*z
        result.y = m10*x + m11*y + m12*z

        result
    }
    private[math] def transposeMul(x: Double, y: Double, result: Vec3d) = {
        result.x = m00*x + m10*y
        result.y = m01*x + m11*y
        result.z = m02*x + m12*y

        result
    }

    /**
     * This method will apply the matrix transformation to a point
     * (such as vertex or object location).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 1).
     */
    private[math] def transformPoint(x: Double, y: Double, result: Vec2d) = {
        result.x = m00*x + m01*y + m02
        result.y = m10*x + m11*y + m12

        result
    }
    /**
     * This method will apply the matrix transformation to a vector
     * (such as object speed).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 0).
     */
    private[math] def transformVector(x: Double, y: Double, result: Vec2d) = {
        result.x = m00*x + m01*y
        result.y = m10*x + m11*y

        result
    }

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    private[math] def mul(m: AnyMat2x3d, result: Mat2x3d) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        val a02 = m00*m.m02 + m01*m.m12 + m02
        val a12 = m10*m.m02 + m11*m.m12 + m12

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11
        result.m02 = a02; result.m12 = a12

        result
    }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    private[math] def mul(m: AnyMat2d, result: Mat2x3d) = {
        val a00 = m00*m.m00 + m01*m.m10
        val a10 = m10*m.m00 + m11*m.m10

        val a01 = m00*m.m01 + m01*m.m11
        val a11 = m10*m.m01 + m11*m.m11

        result.m00 = a00; result.m10 = a10
        result.m01 = a01; result.m11 = a11
        result.m02 = m02; result.m12 = m12

        result
    }

    def unary_-() = negate(new Mat2x3d)
    def *(s: Double) = mul(s, new Mat2x3d)
    def /(s: Double) = div(s, new Mat2x3d)
    def +(m: AnyMat2x3d) = add(m, new Mat2x3d)
    def -(m: AnyMat2x3d) = sub(m, new Mat2x3d)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat2x3d) = div(m, new Mat2x3d)

    def *(m: AnyMat3x2d) = mul(m, new Mat2d)
    def *(m: AnyMat3d) = mul(m, new Mat2x3d)
    def *(m: AnyMat3x4d) = mul(m, new Mat2x4d)

    def *(u: AnyVec3d) = mul(u.x, u.y, u.z, new Vec2d)

    /**
     * This method will apply the matrix transformation to a point
     * (such as vertex or object location).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 1).
     */
    def transformPoint(u: AnyVec2d) :Vec2d =
        transformPoint(u.x, u.y, new Vec2d)

    /**
     * This method will apply the matrix transformation to a vector
     * (such as object speed).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 0).
     */
    def transformVector(u: AnyVec2d) :Vec2d =
        transformVector(u.x, u.y, new Vec2d)

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *(m: AnyMat2x3d) = mul(m, new Mat2x3d)

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *(m: AnyMat2d) = mul(m, new Mat2x3d)

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

final class Mat2x3d private[math] (
    var m00: Double, var m10: Double,
    var m01: Double, var m11: Double,
    var m02: Double, var m12: Double
) extends AnyMat2x3d with RotationSubMat2d
{
    private[math] def this() = this(
        1, 0,
        0, 1,
        0, 0
    )

    def *=(s: Double) { mul(s, this) }
    def /=(s: Double) { div(s, this) }

    def +=(m: AnyMat2x3d) { add(m, this) }
    def -=(m: AnyMat2x3d) { sub(m, this) }

    def *=(m: AnyMat3d) { mul(m, this) }

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *=(m: AnyMat2x3d) { mul(m, this) }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat2x3(Mat3x3(this)*Mat3x3(m)).
     */
    def *=(m: AnyMat2d) { mul(m, this) }

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

    val Zero = const(Mat2x3d(0))
    val Identity = const(Mat2x3d(1))

    def apply(s: Double) = new Mat2x3d(
        s, 0,
        0, s,
        0, 0
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

    def apply(args: ReadAny[AnyVal]*) :Mat2x3d = {
        val mat = new Array[Double](6)
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

        if (index < 6) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat2x3d(
            mat(0), mat(1),
            mat(2), mat(3),
            mat(4), mat(5)
        )
    }

    def apply(m: ReadFloatMat) :Mat2x3d = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Float](rows*columns)
        m.toArray(array, 0)

        val n = new Mat2x3d
        val endr = if (rows < 2) rows else 2
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
}
