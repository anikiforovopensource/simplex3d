/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009 Simplex3d Team
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
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


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat4f extends Read4x4
{
    // Column major order.
    def m00: Float; def m10: Float; def m20: Float; def m30: Float // column
    def m01: Float; def m11: Float; def m21: Float; def m31: Float // column
    def m02: Float; def m12: Float; def m22: Float; def m32: Float // column
    def m03: Float; def m13: Float; def m23: Float; def m33: Float // column

    private[math] def f00 = m00
    private[math] def f10 = m10
    private[math] def f20 = m20
    private[math] def f30 = m30

    private[math] def f01 = m01
    private[math] def f11 = m11
    private[math] def f21 = m21
    private[math] def f31 = m31

    private[math] def f02 = m02
    private[math] def f12 = m12
    private[math] def f22 = m22
    private[math] def f32 = m32

    private[math] def f03 = m03
    private[math] def f13 = m13
    private[math] def f23 = m23
    private[math] def f33 = m33


    private[math] def d00 = m00
    private[math] def d10 = m10
    private[math] def d20 = m20
    private[math] def d30 = m30

    private[math] def d01 = m01
    private[math] def d11 = m11
    private[math] def d21 = m21
    private[math] def d31 = m31

    private[math] def d02 = m02
    private[math] def d12 = m12
    private[math] def d22 = m22
    private[math] def d32 = m32

    private[math] def d03 = m03
    private[math] def d13 = m13
    private[math] def d23 = m23
    private[math] def d33 = m33


    def apply(c: Int) :ConstVec4f = {
        c match {
            case 0 => new ConstVec4f(m00, m10, m20, m30)
            case 1 => new ConstVec4f(m01, m11, m21, m31)
            case 2 => new ConstVec4f(m02, m12, m22, m32)
            case 3 => new ConstVec4f(m03, m13, m23, m33)
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
            case 2 =>
                r match {
                    case 0 => m02
                    case 1 => m12
                    case 2 => m22
                    case 3 => m32
                    case _ => error
                }
            case 3 =>
                r match {
                    case 0 => m03
                    case 1 => m13
                    case 2 => m23
                    case 3 => m33
                    case _ => error
                }
            case _ => error
        }
    }

    def unary_+() :this.type = this
    def unary_-() = new Mat4f(
        -m00, -m10, -m20, -m30,
        -m01, -m11, -m21, -m31,
        -m02, -m12, -m22, -m32,
        -m03, -m13, -m23, -m33
    )
    def *(s: Float) = new Mat4f(
        s*m00, s*m10, s*m20, s*m30,
        s*m01, s*m11, s*m21, s*m31,
        s*m02, s*m12, s*m22, s*m32,
        s*m03, s*m13, s*m23, s*m33
    )
    def /(s: Float) = { val inv = 1/s; new Mat4f(
        inv*m00, inv*m10, inv*m20, inv*m30,
        inv*m01, inv*m11, inv*m21, inv*m31,
        inv*m02, inv*m12, inv*m22, inv*m32,
        inv*m03, inv*m13, inv*m23, inv*m33
    )}

    def +(s: Float) = new Mat4f(
        m00 + s, m10 + s, m20 + s, m30 + s,
        m01 + s, m11 + s, m21 + s, m31 + s,
        m02 + s, m12 + s, m22 + s, m32 + s,
        m03 + s, m13 + s, m23 + s, m33 + s
    )
    def -(s: Float) = new Mat4f(
        m00 - s, m10 - s, m20 - s, m30 - s,
        m01 - s, m11 - s, m21 - s, m31 - s,
        m02 - s, m12 - s, m22 - s, m32 - s,
        m03 - s, m13 - s, m23 - s, m33 - s
    )

    def +(m: AnyMat4f) = new Mat4f(
        m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
        m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31,
        m02 + m.m02, m12 + m.m12, m22 + m.m22, m32 + m.m32,
        m03 + m.m03, m13 + m.m13, m23 + m.m23, m33 + m.m33
    )
    def -(m: AnyMat4f) = new Mat4f(
        m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
        m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31,
        m02 - m.m02, m12 - m.m12, m22 - m.m22, m32 - m.m32,
        m03 - m.m03, m13 - m.m13, m23 - m.m23, m33 - m.m33
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat4f) = new Mat4f(
        m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
        m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31,
        m02/m.m02, m12/m.m12, m22/m.m22, m32/m.m32,
        m03/m.m03, m13/m.m13, m23/m.m23, m33/m.m33
    )
    private[math] def divideByComponent(s: Float) = new Mat4f(
        s/m00, s/m10, s/m20, s/m30,
        s/m01, s/m11, s/m21, s/m31,
        s/m02, s/m12, s/m22, s/m32,
        s/m03, s/m13, s/m23, s/m33
    )

    def *(m: AnyMat4x2f) = new Mat4x2f(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
        m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
        m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
        m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
        m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31
    )
    def *(m: AnyMat4x3f) = new Mat4x3f(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
        m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
        m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
        m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
        m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31,

        m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
        m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
        m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32,
        m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32
    )
    def *(m: AnyMat4f) = new Mat4f(
        m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30,
        m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30,
        m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30,
        m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30,

        m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31,
        m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31,
        m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31,
        m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31,

        m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32,
        m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32,
        m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32,
        m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32,

        m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33,
        m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33,
        m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33,
        m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33
    )

    def *(u: AnyVec4f) = new Vec4f(
        m00*u.x + m01*u.y + m02*u.z + m03*u.w,
        m10*u.x + m11*u.y + m12*u.z + m13*u.w,
        m20*u.x + m21*u.y + m22*u.z + m23*u.w,
        m30*u.x + m31*u.y + m32*u.z + m33*u.w
    )
    private[math] def transposeMul(u: AnyVec4f) = new Vec4f(
        m00*u.x + m10*u.y + m20*u.z + m30*u.w,
        m01*u.x + m11*u.y + m21*u.z + m31*u.w,
        m02*u.x + m12*u.y + m22*u.z + m32*u.w,
        m03*u.x + m13*u.y + m23*u.z + m33*u.w
    )

    def ==(m: AnyMat4f) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 && m30 == m.m30 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 && m31 == m.m31 &&
            m02 == m.m02 && m12 == m.m12 && m22 == m.m22 && m32 == m.m32 &&
            m03 == m.m03 && m13 == m.m13 && m23 == m.m23 && m33 == m.m33
    }

    def !=(m: AnyMat4f) :Boolean = !(this == m)

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
            isNaN(m31) || isInfinite(m31) ||

            isNaN(m02) || isInfinite(m02) ||
            isNaN(m12) || isInfinite(m12) ||
            isNaN(m22) || isInfinite(m22) ||
            isNaN(m32) || isInfinite(m32) ||

            isNaN(m03) || isInfinite(m03) ||
            isNaN(m13) || isInfinite(m13) ||
            isNaN(m23) || isInfinite(m23) ||
            isNaN(m33) || isInfinite(m33)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case m: AnyMat4f => this == m
            case _ => false
        }
    }

    override def hashCode() :Int = {
        41 * (
          41 * (
            41 * (
              41 * (
                41 * (
                  41 * (
                    41 * (
                      41 * (
                        41 * (
                          41 * (
                            41 * (
                              41 * (
                                41 * (
                                  41 * (
                                    41 * (
                                      41 + m00.hashCode
                                    ) + m10.hashCode
                                  ) + m20.hashCode
                                ) + m30.hashCode
                              ) + m01.hashCode
                            ) + m11.hashCode
                          ) + m21.hashCode
                        ) + m31.hashCode
                      ) + m02.hashCode
                    ) + m12.hashCode
                  ) + m22.hashCode
                ) + m32.hashCode
              ) + m03.hashCode
            ) + m13.hashCode
          ) + m23.hashCode
        ) + m33.hashCode
    }

    override def toString() :String = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
            m01 + ", " + m11 + ", " + m21 + ", " + m31 + "; " + 
            m02 + ", " + m12 + ", " + m22 + ", " + m32 + "; " + 
            m03 + ", " + m13 + ", " + m23 + ", " + m33 +
        ")"
    }
}

final class ConstMat4f private[math] (
    val m00: Float, val m10: Float, val m20: Float, val m30: Float,
    val m01: Float, val m11: Float, val m21: Float, val m31: Float,
    val m02: Float, val m12: Float, val m22: Float, val m32: Float,
    val m03: Float, val m13: Float, val m23: Float, val m33: Float
) extends AnyMat4f

object ConstMat4f {

    def apply(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float,
        m02: Float, m12: Float, m22: Float, m32: Float,
        m03: Float, m13: Float, m23: Float, m33: Float
      ) = new ConstMat4f(
            m00, m10, m20, m30,
            m01, m11, m21, m31,
            m02, m12, m22, m32,
            m03, m13, m23, m33
      )

    def apply(c0: Read4, c1: Read4, c2: Read4, c3: Read4) = 
    new ConstMat4f(
        c0.fx, c0.fy, c0.fz, c0.fw,
        c1.fx, c1.fy, c1.fz, c1.fw,
        c2.fx, c2.fy, c2.fz, c2.fw,
        c3.fx, c3.fy, c3.fz, c3.fw
    )

    def apply(m: Read4x4) = new ConstMat4f(
        m.f00, m.f10, m.f20, m.f30,
        m.f01, m.f11, m.f21, m.f31,
        m.f02, m.f12, m.f22, m.f32,
        m.f03, m.f13, m.f23, m.f33
    )

    implicit def toConst(m: AnyMat4f) = ConstMat4f(m)
}


final class Mat4f private[math] (
    var m00: Float, var m10: Float, var m20: Float, var m30: Float,
    var m01: Float, var m11: Float, var m21: Float, var m31: Float,
    var m02: Float, var m12: Float, var m22: Float, var m32: Float,
    var m03: Float, var m13: Float, var m23: Float, var m33: Float
) extends AnyMat4f
{
    def *=(s: Float) {
        m00 *= s; m10 *= s; m20 *= s; m30 *= s;
        m01 *= s; m11 *= s; m21 *= s; m31 *= s;
        m02 *= s; m12 *= s; m22 *= s; m32 *= s;
        m03 *= s; m13 *= s; m23 *= s; m33 *= s
    }
    def /=(s: Float) { val inv = 1/s;
        m00 *= inv; m10 *= inv; m20 *= inv; m30 *= inv;
        m01 *= inv; m11 *= inv; m21 *= inv; m31 *= inv;
        m02 *= inv; m12 *= inv; m22 *= inv; m32 *= inv;
        m03 *= inv; m13 *= inv; m23 *= inv; m33 *= inv
    }

    def +=(s: Float) {
        m00 += s; m10 += s; m20 += s; m30 += s
        m01 += s; m11 += s; m21 += s; m31 += s
        m02 += s; m12 += s; m22 += s; m32 += s
        m03 += s; m13 += s; m23 += s; m33 += s
    }
    def -=(s: Float) {
        m00 -= s; m10 -= s; m20 -= s; m30 -= s
        m01 -= s; m11 -= s; m21 -= s; m31 -= s
        m02 -= s; m12 -= s; m22 -= s; m32 -= s
        m03 -= s; m13 -= s; m23 -= s; m33 -= s
    }

    def +=(m: AnyMat4f) {
        m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
        m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31;
        m02 += m.m02; m12 += m.m12; m22 += m.m22; m32 += m.m32;
        m03 += m.m03; m13 += m.m13; m23 += m.m23; m33 += m.m33
    }
    def -=(m: AnyMat4f) {
        m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
        m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31;
        m02 -= m.m02; m12 -= m.m12; m22 -= m.m22; m32 -= m.m32;
        m03 -= m.m03; m13 -= m.m13; m23 -= m.m23; m33 -= m.m33
    }

    def *=(m: AnyMat4f) {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20 + m03*m.m30
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20 + m13*m.m30
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20 + m23*m.m30
        val a30 = m30*m.m00 + m31*m.m10 + m32*m.m20 + m33*m.m30

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21 + m03*m.m31
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21 + m13*m.m31
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21 + m23*m.m31
        val a31 = m30*m.m01 + m31*m.m11 + m32*m.m21 + m33*m.m31

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22 + m03*m.m32
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22 + m13*m.m32
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22 + m23*m.m32
        val a32 = m30*m.m02 + m31*m.m12 + m32*m.m22 + m33*m.m32

        val a03 = m00*m.m03 + m01*m.m13 + m02*m.m23 + m03*m.m33
        val a13 = m10*m.m03 + m11*m.m13 + m12*m.m23 + m13*m.m33
        val a23 = m20*m.m03 + m21*m.m13 + m22*m.m23 + m23*m.m33
        val a33 = m30*m.m03 + m31*m.m13 + m32*m.m23 + m33*m.m33

        m00 = a00; m10 = a10; m20 = a20; m30 = a30
        m01 = a01; m11 = a11; m21 = a21; m31 = a31
        m02 = a02; m12 = a12; m22 = a22; m32 = a32
        m03 = a03; m13 = a13; m23 = a23; m33 = a33
    }
    /**
     * Component-wise devision.
     */
    def /=(m: AnyMat4f) {
        m00 /= m.m00; m10 /= m.m10; m20 /= m.m20; m30 /= m.m30
        m01 /= m.m01; m11 /= m.m11; m21 /= m.m21; m31 /= m.m31
        m02 /= m.m02; m12 /= m.m12; m22 /= m.m22; m32 /= m.m32
        m03 /= m.m03; m13 /= m.m13; m23 /= m.m23; m33 /= m.m33
    }

    def :=(m: AnyMat4f) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
        m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31;
        m02 = m.m02; m12 = m.m12; m22 = m.m22; m32 = m.m32;
        m03 = m.m03; m13 = m.m13; m23 = m.m23; m33 = m.m33
    }

    def set(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float,
        m02: Float, m12: Float, m22: Float, m32: Float,
        m03: Float, m13: Float, m23: Float, m33: Float
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20; this.m30 = m30;
        this.m01 = m01; this.m11 = m11; this.m21 = m21; this.m31 = m31;
        this.m02 = m02; this.m12 = m12; this.m22 = m22; this.m32 = m32;
        this.m03 = m03; this.m13 = m13; this.m23 = m23; this.m33 = m33
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
            case 2 =>
                r match {
                    case 0 => m02 = s
                    case 1 => m12 = s
                    case 2 => m22 = s
                    case 3 => m32 = s
                    case _ => error
                }
            case 3 =>
                r match {
                    case 0 => m03 = s
                    case 1 => m13 = s
                    case 2 => m23 = s
                    case 3 => m33 = s
                    case _ => error
                }
            case _ => error
        }
    }

    def update(c: Int, v: AnyVec2f) {
        c match {
            case 0 => m00 = v.x; m10 = v.y
            case 1 => m01 = v.x; m11 = v.y
            case 2 => m02 = v.x; m12 = v.y
            case 3 => m03 = v.x; m13 = v.y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
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

    def update(c: Int, v: AnyVec4f) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
            case 2 => m02 = v.x; m12 = v.y; m22 = v.z; m32 = v.w
            case 3 => m03 = v.x; m13 = v.y; m23 = v.z; m33 = v.w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }
}

object Mat4f {

    val Zero: ConstMat4f = Mat4f(0)
    val Identity: ConstMat4f = Mat4f(1)

    def apply(s: Float) = new Mat4f(
        s, 0, 0, 0,
        0, s, 0, 0,
        0, 0, s, 0,
        0, 0, 0, s
    )

    def apply(
        m00: Float, m10: Float, m20: Float, m30: Float,
        m01: Float, m11: Float, m21: Float, m31: Float,
        m02: Float, m12: Float, m22: Float, m32: Float,
        m03: Float, m13: Float, m23: Float, m33: Float
      ) = new Mat4f(
            m00, m10, m20, m30,
            m01, m11, m21, m31,
            m02, m12, m22, m32,
            m03, m13, m23, m33
      )

    def apply(c0: Read4, c1: Read4, c2: Read4, c3: Read4) = 
    new Mat4f(
        c0.fx, c0.fy, c0.fz, c0.fw,
        c1.fx, c1.fy, c1.fz, c1.fw,
        c2.fx, c2.fy, c2.fz, c2.fw,
        c3.fx, c3.fy, c3.fz, c3.fw
    )

    def apply(m: Read2x2) = new Mat4f(
        m.f00, m.f10, 0, 0,
        m.f01, m.f11, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    )

    def apply(m: Read2x3) = new Mat4f(
        m.f00, m.f10, 0, 0,
        m.f01, m.f11, 0, 0,
        m.f02, m.f12, 1, 0,
        0, 0, 0, 1
    )

    def apply(m: Read2x4) = new Mat4f(
        m.f00, m.f10, 0, 0,
        m.f01, m.f11, 0, 0,
        m.f02, m.f12, 1, 0,
        m.f03, m.f13, 0, 1
    )

    def apply(m: Read3x2) = new Mat4f(
        m.f00, m.f10, m.f20, 0,
        m.f01, m.f11, m.f21, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    )

    def apply(m: Read3x3) = new Mat4f(
        m.f00, m.f10, m.f20, 0,
        m.f01, m.f11, m.f21, 0,
        m.f02, m.f12, m.f22, 0,
        0, 0, 0, 1
    )

    def apply(m: Read3x4) = new Mat4f(
        m.f00, m.f10, m.f20, 0,
        m.f01, m.f11, m.f21, 0,
        m.f02, m.f12, m.f22, 0,
        m.f03, m.f13, m.f23, 1
    )

    def apply(m: Read4x2) = new Mat4f(
        m.f00, m.f10, m.f20, m.f30,
        m.f01, m.f11, m.f21, m.f31,
        0, 0, 1, 0,
        0, 0, 0, 1
    )

    def apply(m: Read4x3) = new Mat4f(
        m.f00, m.f10, m.f20, m.f30,
        m.f01, m.f11, m.f21, m.f31,
        m.f02, m.f12, m.f22, m.f32,
        0, 0, 0, 1
    )

    def apply(m: Read4x4) = new Mat4f(
        m.f00, m.f10, m.f20, m.f30,
        m.f01, m.f11, m.f21, m.f31,
        m.f02, m.f12, m.f22, m.f32,
        m.f03, m.f13, m.f23, m.f33
    )

    def unapply(m: AnyMat4f) = Some((m(0), m(1), m(2), m(3)))

    implicit def toMutable(m: AnyMat4f) = Mat4f(m)
}
