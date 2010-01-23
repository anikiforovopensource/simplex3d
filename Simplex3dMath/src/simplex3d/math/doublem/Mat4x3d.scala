/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyMat4x3d
{
    // Column major order.
    def m00: Double; def m10: Double; def m20: Double; def m30: Double // column
    def m01: Double; def m11: Double; def m21: Double; def m31: Double // column
    def m02: Double; def m12: Double; def m22: Double; def m32: Double // column

    def apply(c: Int) :ConstVec4d = {
        c match {
            case 0 => new ConstVec4d(m00, m10, m20, m30)
            case 1 => new ConstVec4d(m01, m11, m21, m31)
            case 2 => new ConstVec4d(m02, m12, m22, m32)
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
            case _ => error
        }
    }

    def unary_-() = new Mat4x3d(
        -m00, -m10, -m20, -m30,
        -m01, -m11, -m21, -m31,
        -m02, -m12, -m22, -m32
    )
    def *(s: Double) = new Mat4x3d(
        s*m00, s*m10, s*m20, s*m30,
        s*m01, s*m11, s*m21, s*m31,
        s*m02, s*m12, s*m22, s*m32
    )
    def /(s: Double) = { val inv = 1/s; new Mat4x3d(
        inv*m00, inv*m10, inv*m20, inv*m30,
        inv*m01, inv*m11, inv*m21, inv*m31,
        inv*m02, inv*m12, inv*m22, inv*m32
    )}

    def +(m: AnyMat4x3d) = new Mat4x3d(
        m00 + m.m00, m10 + m.m10, m20 + m.m20, m30 + m.m30,
        m01 + m.m01, m11 + m.m11, m21 + m.m21, m31 + m.m31,
        m02 + m.m02, m12 + m.m12, m22 + m.m22, m32 + m.m32
    )
    def -(m: AnyMat4x3d) = new Mat4x3d(
        m00 - m.m00, m10 - m.m10, m20 - m.m20, m30 - m.m30,
        m01 - m.m01, m11 - m.m11, m21 - m.m21, m31 - m.m31,
        m02 - m.m02, m12 - m.m12, m22 - m.m22, m32 - m.m32
    )

    /**
     * Component-wise devision.
     */
    def /(m: AnyMat4x3d) = new Mat4x3d(
        m00/m.m00, m10/m.m10, m20/m.m20, m30/m.m30,
        m01/m.m01, m11/m.m11, m21/m.m21, m31/m.m31,
        m02/m.m02, m12/m.m12, m22/m.m22, m32/m.m32
    )
    private[math] def divideByComponent(s: Double) = new Mat4x3d(
        s/m00, s/m10, s/m20, s/m30,
        s/m01, s/m11, s/m21, s/m31,
        s/m02, s/m12, s/m22, s/m32
    )

    def *(m: AnyMat3x2d) = new Mat4x2d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,
        m20*m.m00 + m21*m.m10 + m22*m.m20,
        m30*m.m00 + m31*m.m10 + m32*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21,
        m20*m.m01 + m21*m.m11 + m22*m.m21,
        m30*m.m01 + m31*m.m11 + m32*m.m21
    )
    def *(m: AnyMat3d) = new Mat4x3d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,
        m20*m.m00 + m21*m.m10 + m22*m.m20,
        m30*m.m00 + m31*m.m10 + m32*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21,
        m20*m.m01 + m21*m.m11 + m22*m.m21,
        m30*m.m01 + m31*m.m11 + m32*m.m21,

        m00*m.m02 + m01*m.m12 + m02*m.m22,
        m10*m.m02 + m11*m.m12 + m12*m.m22,
        m20*m.m02 + m21*m.m12 + m22*m.m22,
        m30*m.m02 + m31*m.m12 + m32*m.m22
    )
    def *(m: AnyMat3x4d) = new Mat4d(
        m00*m.m00 + m01*m.m10 + m02*m.m20,
        m10*m.m00 + m11*m.m10 + m12*m.m20,
        m20*m.m00 + m21*m.m10 + m22*m.m20,
        m30*m.m00 + m31*m.m10 + m32*m.m20,

        m00*m.m01 + m01*m.m11 + m02*m.m21,
        m10*m.m01 + m11*m.m11 + m12*m.m21,
        m20*m.m01 + m21*m.m11 + m22*m.m21,
        m30*m.m01 + m31*m.m11 + m32*m.m21,

        m00*m.m02 + m01*m.m12 + m02*m.m22,
        m10*m.m02 + m11*m.m12 + m12*m.m22,
        m20*m.m02 + m21*m.m12 + m22*m.m22,
        m30*m.m02 + m31*m.m12 + m32*m.m22,

        m00*m.m03 + m01*m.m13 + m02*m.m23,
        m10*m.m03 + m11*m.m13 + m12*m.m23,
        m20*m.m03 + m21*m.m13 + m22*m.m23,
        m30*m.m03 + m31*m.m13 + m32*m.m23
    )

    def *(u: AnyVec3d) = new Vec4d(
        m00*u.x + m01*u.y + m02*u.z,
        m10*u.x + m11*u.y + m12*u.z,
        m20*u.x + m21*u.y + m22*u.z,
        m30*u.x + m31*u.y + m32*u.z
    )
    private[math] def transposeMul(u: AnyVec4d) = new Vec3d(
        m00*u.x + m10*u.y + m20*u.z + m30*u.w,
        m01*u.x + m11*u.y + m21*u.z + m31*u.w,
        m02*u.x + m12*u.y + m22*u.z + m32*u.w
    )

    def ==(m: AnyMat4x3d) :Boolean = {
        if (m eq null) false
        else
            m00 == m.m00 && m10 == m.m10 && m20 == m.m20 && m30 == m.m30 &&
            m01 == m.m01 && m11 == m.m11 && m21 == m.m21 && m31 == m.m31 &&
            m02 == m.m02 && m12 == m.m12 && m22 == m.m22 && m32 == m.m32
    }

    def !=(m: AnyMat4x3d) :Boolean = !(this == m)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._

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
            isNaN(m32) || isInfinite(m32)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case m: AnyMat4x3d => this == m
            case _ => false
        }
    }

    override def hashCode :Int = {
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
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" +
            m00 + ", " + m10 + ", " + m20 + ", " + m30 + "; " + 
            m01 + ", " + m11 + ", " + m21 + ", " + m31 + "; " + 
            m02 + ", " + m12 + ", " + m22 + ", " + m32 +
        ")"
    }
}

final class ConstMat4x3d private[math] (
    val m00: Double, val m10: Double, val m20: Double, val m30: Double,
    val m01: Double, val m11: Double, val m21: Double, val m31: Double,
    val m02: Double, val m12: Double, val m22: Double, val m32: Double
) extends AnyMat4x3d

object ConstMat4x3d {

    def apply(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double,
        m02: Double, m12: Double, m22: Double, m32: Double
      ) = new ConstMat4x3d(
            m00, m10, m20, m30,
            m01, m11, m21, m31,
            m02, m12, m22, m32
      )

    def apply(c0: AnyVec4d, c1: AnyVec4d, c2: AnyVec4d) = 
    new ConstMat4x3d(
        c0.x, c0.y, c0.z, c0.w,
        c1.x, c1.y, c1.z, c1.w,
        c2.x, c2.y, c2.z, c2.w
    )

    def apply(m: AnyMat4x3d) = new ConstMat4x3d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31,
        m.m02, m.m12, m.m22, m.m32
    )

    implicit def toConst(m: Mat4x3d) = ConstMat4x3d(m)
}


final class Mat4x3d private[math] (
    var m00: Double, var m10: Double, var m20: Double, var m30: Double,
    var m01: Double, var m11: Double, var m21: Double, var m31: Double,
    var m02: Double, var m12: Double, var m22: Double, var m32: Double
) extends AnyMat4x3d
{
    def *=(s: Double) {
        m00 *= s; m10 *= s; m20 *= s; m30 *= s;
        m01 *= s; m11 *= s; m21 *= s; m31 *= s;
        m02 *= s; m12 *= s; m22 *= s; m32 *= s
    }
    def /=(s: Double) { val inv = 1/s;
        m00 *= inv; m10 *= inv; m20 *= inv; m30 *= inv;
        m01 *= inv; m11 *= inv; m21 *= inv; m31 *= inv;
        m02 *= inv; m12 *= inv; m22 *= inv; m32 *= inv
    }

    def +=(m: AnyMat4x3d) {
        m00 += m.m00; m10 += m.m10; m20 += m.m20; m30 += m.m30;
        m01 += m.m01; m11 += m.m11; m21 += m.m21; m31 += m.m31;
        m02 += m.m02; m12 += m.m12; m22 += m.m22; m32 += m.m32
    }
    def -=(m: AnyMat4x3d) {
        m00 -= m.m00; m10 -= m.m10; m20 -= m.m20; m30 -= m.m30;
        m01 -= m.m01; m11 -= m.m11; m21 -= m.m21; m31 -= m.m31;
        m02 -= m.m02; m12 -= m.m12; m22 -= m.m22; m32 -= m.m32
    }

    def *=(m: AnyMat3d) {
        val a00 = m00*m.m00 + m01*m.m10 + m02*m.m20
        val a10 = m10*m.m00 + m11*m.m10 + m12*m.m20
        val a20 = m20*m.m00 + m21*m.m10 + m22*m.m20
        val a30 = m30*m.m00 + m31*m.m10 + m32*m.m20

        val a01 = m00*m.m01 + m01*m.m11 + m02*m.m21
        val a11 = m10*m.m01 + m11*m.m11 + m12*m.m21
        val a21 = m20*m.m01 + m21*m.m11 + m22*m.m21
        val a31 = m30*m.m01 + m31*m.m11 + m32*m.m21

        val a02 = m00*m.m02 + m01*m.m12 + m02*m.m22
        val a12 = m10*m.m02 + m11*m.m12 + m12*m.m22
        val a22 = m20*m.m02 + m21*m.m12 + m22*m.m22
        val a32 = m30*m.m02 + m31*m.m12 + m32*m.m22

        m00 = a00; m10 = a10; m20 = a20; m30 = a30
        m01 = a01; m11 = a11; m21 = a21; m31 = a31
        m02 = a02; m12 = a12; m22 = a22; m32 = a32
    }

    def :=(m: AnyMat4x3d) {
        m00 = m.m00; m10 = m.m10; m20 = m.m20; m30 = m.m30;
        m01 = m.m01; m11 = m.m11; m21 = m.m21; m31 = m.m31;
        m02 = m.m02; m12 = m.m12; m22 = m.m22; m32 = m.m32
    }

    def set(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double,
        m02: Double, m12: Double, m22: Double, m32: Double
    ) {
        this.m00 = m00; this.m10 = m10; this.m20 = m20; this.m30 = m30;
        this.m01 = m01; this.m11 = m11; this.m21 = m21; this.m31 = m31;
        this.m02 = m02; this.m12 = m12; this.m22 = m22; this.m32 = m32
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

    def update(c: Int, v: AnyVec3d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z
            case 2 => m02 = v.x; m12 = v.y; m22 = v.z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def update(c: Int, v: AnyVec4d) {
        c match {
            case 0 => m00 = v.x; m10 = v.y; m20 = v.z; m30 = v.w
            case 1 => m01 = v.x; m11 = v.y; m21 = v.z; m31 = v.w
            case 2 => m02 = v.x; m12 = v.y; m22 = v.z; m32 = v.w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Mat4x3d {

    val Zero: ConstMat4x3d = Mat4x3d(0)
    val Identity: ConstMat4x3d = Mat4x3d(1)

    def apply(s: Double) = new Mat4x3d(
        s, 0, 0, 0,
        0, s, 0, 0,
        0, 0, s, 0
    )

    def apply(c0: Read4Float, c1: Read4Float, c2: Read4Float) = 
    new Mat4x3d(
        c0.x, c0.y, c0.z, c0.w,
        c1.x, c1.y, c1.z, c1.w,
        c2.x, c2.y, c2.z, c2.w
    )

    def apply(
        m00: Double, m10: Double, m20: Double, m30: Double,
        m01: Double, m11: Double, m21: Double, m31: Double,
        m02: Double, m12: Double, m22: Double, m32: Double
      ) = new Mat4x3d(
            m00, m10, m20, m30,
            m01, m11, m21, m31,
            m02, m12, m22, m32
      )

    def apply(c0: AnyVec4d, c1: AnyVec4d, c2: AnyVec4d) = 
    new Mat4x3d(
        c0.x, c0.y, c0.z, c0.w,
        c1.x, c1.y, c1.z, c1.w,
        c2.x, c2.y, c2.z, c2.w
    )

    def apply(m: AnyMat2d) = new Mat4x3d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0,
        0, 0, 1, 0
    )

    def apply(m: AnyMat2x3d) = new Mat4x3d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0,
        m.m02, m.m12, 1, 0
    )

    def apply(m: AnyMat2x4d) = new Mat4x3d(
        m.m00, m.m10, 0, 0,
        m.m01, m.m11, 0, 0,
        m.m02, m.m12, 1, 0
    )

    def apply(m: AnyMat3x2d) = new Mat4x3d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0,
        0, 0, 1, 0
    )

    def apply(m: AnyMat3d) = new Mat4x3d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0,
        m.m02, m.m12, m.m22, 0
    )

    def apply(m: AnyMat3x4d) = new Mat4x3d(
        m.m00, m.m10, m.m20, 0,
        m.m01, m.m11, m.m21, 0,
        m.m02, m.m12, m.m22, 0
    )

    def apply(m: AnyMat4x2d) = new Mat4x3d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31,
        0, 0, 1, 0
    )

    def apply(m: AnyMat4x3d) = new Mat4x3d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31,
        m.m02, m.m12, m.m22, m.m32
    )

    def apply(m: AnyMat4d) = new Mat4x3d(
        m.m00, m.m10, m.m20, m.m30,
        m.m01, m.m11, m.m21, m.m31,
        m.m02, m.m12, m.m22, m.m32
    )

    implicit def toMutable(m: ConstMat4x3d) = Mat4x3d(m)
}
