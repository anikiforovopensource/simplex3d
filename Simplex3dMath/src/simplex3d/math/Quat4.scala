/*
 * Simplex3D, Math module
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
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyQuat4 {
    def a: Float
    def b: Float
    def c: Float
    def d: Float

    def apply(i: Int) :Float = {
        i match {
            case 0 => a
            case 1 => b
            case 2 => c
            case 3 => d
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    /**
     * This methods negates every term of this quaternion.
     * Negating the quaternion produces another quaternion which represent
     * the same rotation. That is both q and -q represent exactly the
     * same rotation.
     */
    def unary_-() = Quat4(-a, -b, -c, -d)
    def *(s: Float) = Quat4(a*s, b*s, c*s, d*s)
    def /(s: Float) = { val inv = 1/s; Quat4(a*inv, b*inv, c*inv, d*inv) }
    private[math] def divideByComponent(s: Float) = Quat4(s/a, s/b, s/c, s/d)

    def +(q: Quat4) = Quat4(a + q.a, b + q.b, c + q.c, d + q.d)
    def -(q: Quat4) = Quat4(a - q.a, b - q.b, c - q.c, d - q.d)
    def *(q: Quat4) = Quat4(
        a*q.a - b*q.b - c*q.c - d*q.d,
        a*q.b + b*q.a + c*q.d - d*q.c,
        a*q.c - b*q.d + c*q.a + d*q.b,
        a*q.d + b*q.c - c*q.b + d*q.a
    )

    def *(u: AnyVec3) = {
        val t1 = a*b
        val t2 = a*c
        val t3 = a*d
        val t4 = -b*b
        val t5 = b*c
        val t6 = b*d
        val t7 = -c*c
        val t8 = c*d
        val t9 = -d*d

        Vec3(
            2*((t7 + t9)*u.x + (t5 - t3)*u.y + (t2 + t6)*u.z) + u.x,
            2*((t3 + t5)*u.x + (t4 + t9)*u.y + (t8 - t1)*u.z) + u.y,
            2*((t6 - t2)*u.x + (t1 + t8)*u.y + (t4 + t7)*u.z) + u.z
        )
    }

    def ==(q: AnyQuat4) :Boolean = {
        if (q eq null) false
        else a == q.a && b == q.b && c == q.c && d == q.d
    }

    def !=(q: AnyQuat4) :Boolean = !(this == q)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(a) || isInfinite(a) ||
            isNaN(b) || isInfinite(b) ||
            isNaN(c) || isInfinite(c) ||
            isNaN(d) || isInfinite(d)
        )
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" + a + ", " + b + ", " + c + ", " + d + ")"
    }
}

final class ConstQuat4 private (val a: Float, val b: Float,
                                val c: Float, val d: Float)
extends AnyQuat4

object ConstQuat4 {
    def apply() = new ConstQuat4(1, 0, 0, 0)

    def apply(a: Float, b: Float, c: Float, d: Float) =
        new ConstQuat4(a, b, c, d)

    def apply(q: AnyQuat4) = new ConstQuat4(q.a, q.b, q.c, q.d)
    def apply(u: AnyVec4) = new ConstQuat4(u.w, u.x, u.y, u.z)
    def apply(m: AnyMat2) = new ConstQuat4(m.m00, m.m10, m.m01, m.m11)

    implicit def mutableToConst(q: Quat4) = ConstQuat4(q)
}

final class Quat4 private (var a: Float, var b: Float,
                           var c: Float, var d: Float)
extends AnyQuat4
{
    def *=(s: Float) { a *= s; b *= s; c *= s; d *= s }
    def /=(s: Float) { val inv = 1/s; a *= inv; b *= inv; c *= inv; d *= inv }

    def +=(q: Quat4) { a += q.a; b += q.b; c += q.c; d += q.d }
    def -=(q: Quat4) { a -= q.a; b -= q.b; c -= q.c; d -= q.d }
    def *=(q: Quat4) {
        val na = a*q.a - b*q.b - c*q.c - d*q.d
        val nb = a*q.b + b*q.a + c*q.d - d*q.c
        val nc = a*q.c - b*q.d + c*q.a + d*q.b
        d = a*q.d + b*q.c - c*q.b + d*q.a

        a = na; b = nb; c = nc
    }
    
    def :=(q: AnyQuat4) { a = q.a; b = q.b; c = q.c; d = q.d }
    def set(a: Float, b: Float, c: Float, d: Float) {
        this.a = a; this.b = b; this.c = c; this.d = d
    }

    def update(i: Int, s: Float) {
        i match {
            case 0 => a = s
            case 1 => b = s
            case 2 => c = s
            case 3 => d = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }
}

object Quat4 {
    def apply() = new Quat4(1, 0, 0, 0)
    def apply(a: Float, b: Float, c: Float, d: Float) = new Quat4(a, b, c, d)
    def apply(q: AnyQuat4) = new Quat4(q.a, q.b, q.c, q.d)
    def apply(u: AnyVec4) = new Quat4(u.w, u.x, u.y, u.z)
    def apply(m: AnyMat2) = new Quat4(m.m00, m.m10, m.m01, m.m11)
}
