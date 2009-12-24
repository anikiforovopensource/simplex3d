/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyQuat4d {
    def a: Double
    def b: Double
    def c: Double
    def d: Double

    def apply(i: Int) :Double = {
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
    def unary_-() = new Quat4d(-a, -b, -c, -d)
    def *(s: Double) = new Quat4d(a * s, b * s, c * s, d * s)
    def /(s: Double) = { val inv = 1/s;
        new Quat4d(a * inv, b * inv, c * inv, d * inv)
    }
    private[math] def divideByComponent(s: Double) = {
        new Quat4d(s / a, s / b, s / c, s / d)
    }

    def +(q: Quat4d) = new Quat4d(a + q.a, b + q.b, c + q.c, d + q.d)
    def -(q: Quat4d) = new Quat4d(a - q.a, b - q.b, c - q.c, d - q.d)
    def *(q: Quat4d) = new Quat4d(
        a*q.a - b*q.b - c*q.c - d*q.d,
        a*q.b + b*q.a + c*q.d - d*q.c,
        a*q.c - b*q.d + c*q.a + d*q.b,
        a*q.d + b*q.c - c*q.b + d*q.a
    )

    def ==(q: AnyQuat4d) :Boolean = {
        if (q eq null) false
        else a == q.a && b == q.b && c == q.c && d == q.d
    }

    def !=(q: AnyQuat4d) :Boolean = !(this == q)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
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

final class ConstQuat4d private[math] (
    val a: Double, val b: Double, val c: Double, val d: Double
) extends AnyQuat4d

object ConstQuat4d {
    def apply(a: Double, b: Double, c: Double, d: Double) = {
        new ConstQuat4d(a, b, c, d)
    }
    def apply(u: AnyQuat4d) = new ConstQuat4d(u.a, u.b, u.c, u.d)
    def apply(u: AnyVec4d) = new ConstQuat4d(u.w, u.x, u.y, u.z)
    def apply(m: AnyMat2d) = new ConstQuat4d(m.m00, m.m10, m.m01, m.m11)

    implicit def mutableToConst(u: Quat4d) = new ConstQuat4d(u.a, u.b, u.c, u.d)
}


final class Quat4d private[math] (
    var a: Double, var b: Double, var c: Double, var d: Double
) extends AnyQuat4d
{
    def *=(s: Double) { a *= s; b *= s; c *= s; d *= s }
    def /=(s: Double) { val inv = 1/s; a *= inv; b *= inv; c *= inv; d *= inv }

    def +=(q: Quat4d) { a += q.a; b += q.b; c += q.c; d += q.d }
    def -=(q: Quat4d) { a -= q.a; b -= q.b; c -= q.c; d -= q.d }
    def *=(q: Quat4d) {
        val na = a*q.a - b*q.b - c*q.c - d*q.d
        val nb = a*q.b + b*q.a + c*q.d - d*q.c
        val nc = a*q.c - b*q.d + c*q.a + d*q.b
        d = a*q.d + b*q.c - c*q.b + d*q.a

        a = na; b = nb; c = nc
    }
    
    def :=(q: AnyQuat4d) { a = q.a; b = q.b; c = q.c; d = q.d }
    def set(a: Double, b: Double, c: Double, d: Double) {
        this.a = a; this.b = b; this.c = c; this.d = d
    }

    def update(i: Int, s: Double) {
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

object Quat4d {
    val Identity: ConstQuat4d = Quat4d()
    
    def apply() = new Quat4d(1, 0, 0, 0)
    def apply(a: Double, b: Double, c: Double, d: Double) = {
        new Quat4d(a, b, c, d)
    }
    def apply(q: AnyQuat4d) = new Quat4d(q.a, q.b, q.c, q.d)
    def apply(u: AnyVec4d) = new Quat4d(u.w, u.x, u.y, u.z)
    def apply(m: AnyMat2d) = new Quat4d(m.m00, m.m10, m.m01, m.m11)

    implicit def constToMutable(u: ConstQuat4d) = new Quat4d(u.a, u.b, u.c, u.d)
}
