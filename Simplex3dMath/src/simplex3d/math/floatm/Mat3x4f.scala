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
sealed abstract class AnyMat3x4f
extends ConstRotationSubMat3f with ReadFloatMat
{
    // Column major order.
    def m00: Float; def m10: Float; def m20: Float // column
    def m01: Float; def m11: Float; def m21: Float // column
    def m02: Float; def m12: Float; def m22: Float // column
    def m03: Float; def m13: Float; def m23: Float // column

    def rows = 3
    def columns = 4
    def toArray(array: Array[Float], offset: Int) {
        array(offset + 0) = m00
        array(offset + 1) = m10
        array(offset + 2) = m20

        array(offset + 3) = m01
        array(offset + 4) = m11
        array(offset + 5) = m21

        array(offset + 6) = m02
        array(offset + 7) = m12
        array(offset + 8) = m22

        array(offset + 9) = m03
        array(offset + 10) = m13
        array(offset + 11) = m23
    }

    def apply(c: Int) :ConstVec3f = {
        c match {
            case 0 => new ConstVec3f(m00, m10, m20)
            case 1 => new ConstVec3f(m01, m11, m21)
            case 2 => new ConstVec3f(m02, m12, m22)
            case 3 => new ConstVec3f(m03, m13, m23)
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
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
            case 3 =>
                r match {
                    case 0 => m03
                    case 1 => m13
                    case 2 => m23
                    case _ => error
                }
            case _ => error
        }
    }

    private[math] def negate(result: Mat3x4f) = {
        result.m00 = -m00
        result.m10 = -m10
        result.m20 = -m20

        result.m01 = -m01
        result.m11 = -m11
        result.m21 = -m21

        result.m02 = -m02
        result.m12 = -m12
        result.m22 = -m22

        result.m03 = -m03
        result.m13 = -m13
        result.m23 = -m23

        result
    }
    private[math] def mul(s: Float, result: Mat3x4f) = {
        result.m00 = s*m00
        result.m10 = s*m10
        result.m20 = s*m20

        result.m01 = s*m01
        result.m11 = s*m11
        result.m21 = s*m21

        result.m02 = s*m02
        result.m12 = s*m12
        result.m22 = s*m22

        result.m03 = s*m03
        result.m13 = s*m13
        result.m23 = s*m23

        result
    }
    private[math] def div(s: Float, result: Mat3x4f) = {
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

        result.m03 = inv*m03
        result.m13 = inv*m13
        result.m23 = inv*m23

        result
    }

    private[math] def add(m: AnyMat3x4f, result:Mat3x4f) = {
        result.m00 = m00 + m.m00
        result.m10 = m10 + m.m10
        result.m20 = m20 + m.m20

        result.m01 = m01 + m.m01
        result.m11 = m11 + m.m11
        result.m21 = m21 + m.m21

        result.m02 = m02 + m.m02
        result.m12 = m12 + m.m12
        result.m22 = m22 + m.m22

        result.m03 = m03 + m.m03
        result.m13 = m13 + m.m13
        result.m23 = m23 + m.m23

        result
    }
    private[math] def sub(m: AnyMat3x4f, result:Mat3x4f) = {
        result.m00 = m00 - m.m00
        result.m10 = m10 - m.m10
        result.m20 = m20 - m.m20

        result.m01 = m01 - m.m01
        result.m11 = m11 - m.m11
        result.m21 = m21 - m.m21

        result.m02 = m02 - m.m02
        result.m12 = m12 - m.m12
        result.m22 = m22 - m.m22

        result.m03 = m03 - m.m03
        result.m13 = m13 - m.m13
        result.m23 = m23 - m.m23

        result
    }

    private[math] def div(m: AnyMat3x4f, result:Mat3x4f) = {
        result.m00 = m00 / m.m00
        result.m10 = m10 / m.m10
        result.m20 = m20 / m.m20

        result.m01 = m01 / m.m01
        result.m11 = m11 / m.m11
        result.m21 = m21 / m.m21

        result.m02 = m02 / m.m02
        result.m12 = m12 / m.m12
        result.m22 = m22 / m.m22

        result.m03 = m03 / m.m03
        result.m13 = m13 / m.m13
        result.m23 = m23 / m.m23

        result
    }
    private[math] def divByComponent(s: Float, result:Mat3x4f) = {
        result.m00 = s / m00
        result.m10 = s / m10
        result.m20 = s / m20

        result.m01 = s / m01
        result.m11 = s / m11
        result.m21 = s / m21

        result.m02 = s / m02
        result.m12 = s / m12
        result.m22 = s / m22

        result.m03 = s / m03
        result.m13 = s / m13
        result.m23 = s / m23

        result
    }

    private[math] def mul(m: AnyMat4x2f, result: Mat3x2f) = {
        result.m00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        result.m10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        result.m20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30

        result.m01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        result.m11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        result.m21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31

        result
    }
    private[math] def mul(m: AnyMat4x3f, result: Mat3f) = {
        result.m00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        result.m10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        result.m20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30

        result.m01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        result.m11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        result.m21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31

        result.m02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        result.m12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        result.m22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32

        result
    }
    private[math] def mul(m: AnyMat4f, result: Mat3x4f) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21
        result.m02 = a02; result.m12 = a12; result.m22 = a22
        result.m03 = a03; result.m13 = a13; result.m23 = a23

        result
    }

    private[math] def mul(u: AnyVec4f, result: Vec3f) = {
        result.x = m00*u.x + m01*u.y + m02*u.z + m03*u.w
        result.y = m10*u.x + m11*u.y + m12*u.z + m13*u.w
        result.z = m20*u.x + m21*u.y + m22*u.z + m23*u.w

        result
    }
    private[math] def transposeMul(u: AnyVec3f, result: Vec4f) = {
        result.x = m00*u.x + m10*u.y + m20*u.z
        result.y = m01*u.x + m11*u.y + m21*u.z
        result.z = m02*u.x + m12*u.y + m22*u.z
        result.w = m03*u.x + m13*u.y + m23*u.z

        result
    }

    /**
     * This method will apply the matrix transformation to a point
     * (such as vertex or object location).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 1).
     */
    private[math] def transformPoint(u: AnyVec3f, result: Vec3f) = {
        val x = m00*u.x + m01*u.y + m02*u.z + m03
        val y = m10*u.x + m11*u.y + m12*u.z + m13
        val z = m20*u.x + m21*u.y + m22*u.z + m23

        result.x = x; result.y = y; result.z = z

        result
    }

    /**
     * This method will apply the matrix transformation to a vector
     * (such as object speed).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 0).
     */
    private[math] def transformVector(u: AnyVec3f, result: Vec3f) = {
        val x = m00*u.x + m01*u.y + m02*u.z
        val y = m10*u.x + m11*u.y + m12*u.z
        val z = m20*u.x + m21*u.y + m22*u.z

        result.x = x; result.y = y; result.z = z

        result
    }

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    private[math] def mul(m: AnyMat3x4f, result: Mat3x4f) = {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23

        result.m00 = a00; result.m10 = a10; result.m20 = a20
        result.m01 = a01; result.m11 = a11; result.m21 = a21
        result.m02 = a02; result.m12 = a12; result.m22 = a22
        result.m03 = a03; result.m13 = a13; result.m23 = a23

        result
    }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    private[math] def mul(m: AnyMat3f, result: Mat3x4f) = {
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
        result.m03 = m03; result.m13 = m13; result.m23 = m23

        result
    }

    def unary_-() = negate(new Mat3x4f)
    def *(s: Float) = mul(s, new Mat3x4f)
    def /(s: Float) = div(s, new Mat3x4f)
    def +(m: AnyMat3x4f) = add(m, new Mat3x4f)
    def -(m: AnyMat3x4f) = sub(m, new Mat3x4f)

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat3x4f) = div(m, new Mat3x4f)

    def *(m: AnyMat4x2f) = mul(m, new Mat3x2f)
    def *(m: AnyMat4x3f) = mul(m, new Mat3f)
    def *(m: AnyMat4f) = mul(m, new Mat3x4f)

    def *(u: AnyVec4f) = mul(u, new Vec3f)

    /**
     * This method will apply the matrix transformation to a point
     * (such as vertex or object location).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 1).
     */
    def transformPoint(u: AnyVec3f) :Vec3f =
        transformPoint(u, new Vec3f)

    /**
     * This method will apply the matrix transformation to a vector
     * (such as object speed).<br/>
     *
     * Equivalent to regular multiplication with Vec(u, 0).
     */
    def transformVector(u: AnyVec3f) :Vec3f =
        transformVector(u, new Vec3f)

    /**
     * Combine two transformations. This method works similar to regular
     * multiplication but with a special handling of the translation column.
     * <br/>
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    def *(m: AnyMat3x4f) = mul(m, new Mat3x4f)

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    def *(m: AnyMat3f) = mul(m, new Mat3x4f)

    def ==(m: AnyMat3x4f) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 &&
            m02 == m.m02 && m12 == m.m12 && m22 == m.m22 &&
            m03 == m.m03 && m13 == m.m13 && m23 == m.m23
    }

    def !=(m: AnyMat3x4f) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._

        (
            isNaN(m00) || isInfinite(m00) ||
            isNaN(m10) || isInfinite(m10) ||
            isNaN(m20) || isInfinite(m20) ||

            isNaN(m01) || isInfinite(m01) ||
            isNaN(m11) || isInfinite(m11) ||
            isNaN(m21) || isInfinite(m21) ||

            isNaN(m02) || isInfinite(m02) ||
            isNaN(m12) || isInfinite(m12) ||
            isNaN(m22) || isInfinite(m22) ||

            isNaN(m03) || isInfinite(m03) ||
            isNaN(m13) || isInfinite(m13) ||
            isNaN(m23) || isInfinite(m23)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + "; " + 
            m01 + ", " + m11 + ", " + m21 + "; " + 
            m02 + ", " + m12 + ", " + m22 + "; " + 
            m03 + ", " + m13 + ", " + m23 +
        ")"
    }
}

final class ConstMat3x4f private[math] (
    val m00: Float, val m10: Float, val m20: Float,
    val m01: Float, val m11: Float, val m21: Float,
    val m02: Float, val m12: Float, val m22: Float,
    val m03: Float, val m13: Float, val m23: Float
) extends AnyMat3x4f

final class Mat3x4f private[math] (
    var m00: Float, var m10: Float, var m20: Float,
    var m01: Float, var m11: Float, var m21: Float,
    var m02: Float, var m12: Float, var m22: Float,
    var m03: Float, var m13: Float, var m23: Float
) extends AnyMat3x4f with RotationSubMat3f
{
    private[math] def this() = this(
        1, 0, 0,
        0, 1, 0,
        0, 0, 1,
        0, 0, 0
    )

    def *=(s: Float) { mul(s, this) }
    def /=(s: Float) { div(s, this) }

    def +=(m: AnyMat3x4f) { add(m, this) }
    def -=(m: AnyMat3x4f) { sub(m, this) }

    def *=(m: AnyMat4f) { mul(m, this) }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    def *=(m: AnyMat3x4f) { mul(m, this) }

    /**
     * Combine this transformation with rotation. This method works similar
     * to regular multiplication but with a special handling of
     * the translation column.<br/>
     *
     * Equaivalent to Mat3x4(Mat4x4(this)*Mat4x4(m)).
     */
    def *=(m: AnyMat3f) { mul(m, this) }

    def :=(m: AnyMat3x4f) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20;
        m01 = m.m01; m11 = m.m11; m21 = m.m21;
        m02 = m.m02; m12 = m.m12; m22 = m.m22;
        m03 = m.m03; m13 = m.m13; m23 = m.m23
    }

    def set(
        m00: Float, m10: Float, m20: Float,
        m01: Float, m11: Float, m21: Float,
        m02: Float, m12: Float, m22: Float,
        m03: Float, m13: Float, m23: Float
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20;
        this.m01 = m01; this.m11 = m11; this.m21 = m21;
        this.m02 = m02; this.m12 = m12; this.m22 = m22;
        this.m03 = m03; this.m13 = m13; this.m23 = m23
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
            case 3 =>
                r match {
                    case 0 => m03 = s
                    case 1 => m13 = s
                    case 2 => m23 = s
                    case _ => error
                }
            case _ => error
        }
    }

    def update(c: Int, v: AnyVec3f) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z
            case 2 => m02 = v.x; m12 = v.y; m22 = v.z
            case 3 => m03 = v.x; m13 = v.y; m23 = v.z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

}

object Mat3x4f {

    val Zero = const(Mat3x4f(0))
    val Identity = const(Mat3x4f(1))

    def apply(s: Float) = new Mat3x4f(
        s, 0, 0,
        0, s, 0,
        0, 0, s,
        0, 0, 0
    )

    def apply(
        m00: Float, m10: Float, m20: Float,
        m01: Float, m11: Float, m21: Float,
        m02: Float, m12: Float, m22: Float,
        m03: Float, m13: Float, m23: Float
      ) = new Mat3x4f(
            m00, m10, m20,
            m01, m11, m21,
            m02, m12, m22,
            m03, m13, m23
      )

    def apply(c0: AnyVec3f, c1: AnyVec3f, c2: AnyVec3f, c3: AnyVec3f) = 
    new Mat3x4f(
        c0.x, c0.y, c0.z,
        c1.x, c1.y, c1.z,
        c2.x, c2.y, c2.z,
        c3.x, c3.y, c3.z
    )

    def apply(args: ReadAny[AnyVal]*) :Mat3x4f = {
        val mat = new Array[Float](12)
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

        if (index < 12) throw new IllegalArgumentException(
            "Too few values for this matrix.")

        new Mat3x4f(
            mat(0), mat(1), mat(2),
            mat(3), mat(4), mat(5),
            mat(6), mat(7), mat(8),
            mat(9), mat(10), mat(11)
        )
    }

    def apply(m: ReadDoubleMat) :Mat3x4f = {
        val rows = m.rows
        val columns = m.columns
        val array = new Array[Double](rows*columns)
        m.toArray(array, 0)

        val n = new Mat3x4f
        val endr = if (rows < 3) rows else 3
        val endc = if (columns < 4) columns else 4

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

    def apply(m: AnyMat2f) = new Mat3x4f(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        0, 0, 1,
        0, 0, 0
    )

    def apply(m: AnyMat2x3f) = new Mat3x4f(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        m.m02, m.m12, 1,
        0, 0, 0
    )

    def apply(m: AnyMat2x4f) = new Mat3x4f(
        m.m00, m.m10, 0,
        m.m01, m.m11, 0,
        m.m02, m.m12, 1,
        m.m03, m.m13, 0
    )

    def apply(m: AnyMat3x2f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        0, 0, 1,
        0, 0, 0
    )

    def apply(m: AnyMat3f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22,
        0, 0, 0
    )

    def apply(m: AnyMat3x4f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22,
        m.m03, m.m13, m.m23
    )

    def apply(m: AnyMat4x2f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        0, 0, 1,
        0, 0, 0
    )

    def apply(m: AnyMat4x3f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22,
        0, 0, 0
    )

    def apply(m: AnyMat4f) = new Mat3x4f(
        m.m00, m.m10, m.m20,
        m.m01, m.m11, m.m21,
        m.m02, m.m12, m.m22,
        m.m03, m.m13, m.m23
    )
}
