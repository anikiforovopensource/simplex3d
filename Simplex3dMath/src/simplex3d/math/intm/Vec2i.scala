/*
 * Simplex3D, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2i extends Read2Int {

    def r = x
    def g = y


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = new Vec2i(-x, -y)
    def unary_~() = new Vec2i(~x, ~y)

    def *(s: Int) = new Vec2i(x * s, y * s)
    def /(s: Int) = new Vec2i(x / s, y / s)
    private[math] def divideByComponent(s: Int) = new Vec2i(s / x, s / y)
    def %(s: Int) = new Vec2i(x % s, y % s)
    private[math] def modByComponent(s: Int) = new Vec2i(s % x, s % y)
    def >>(s: Int) = new Vec2i( x >> s, y >> s)
    def >>>(s: Int) = new Vec2i( x >>> s, y >>> s)
    def <<(s: Int) = new Vec2i( x << s, y << s)
    def &(s: Int) = new Vec2i( x & s, y & s)
    def |(s: Int) = new Vec2i( x | s, y | s)
    def ^(s: Int) = new Vec2i( x ^ s, y ^ s)

    def +(u: AnyVec2i) = new Vec2i(x + u.x, y + u.y)
    def -(u: AnyVec2i) = new Vec2i(x - u.x, y - u.y)
    def *(u: AnyVec2i) = new Vec2i(x * u.x, y * u.y)
    def /(u: AnyVec2i) = new Vec2i(x / u.x, y / u.y)
    def %(u: AnyVec2i) = new Vec2i(x % u.x, y % u.y)
    def >>(u: AnyVec2i) = new Vec2i( x >> u.x, y >> u.y)
    def >>>(u: AnyVec2i) = new Vec2i( x >>> u.x, y >>> u.y)
    def <<(u: AnyVec2i) = new Vec2i( x << u.x, y << u.y)
    def &(u: AnyVec2i) = new Vec2i( x & u.x, y & u.y)
    def |(u: AnyVec2i) = new Vec2i( x | u.x, y | u.y)
    def ^(u: AnyVec2i) = new Vec2i( x ^ u.x, y ^ u.y)

    def ==(u: AnyVec2i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2i) :Boolean = !(this == u)

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }

    // Swizzling
    def xx: ConstVec2i = new ConstVec2i(x, x)
    def xy: ConstVec2i = new ConstVec2i(x, y)
    def yx: ConstVec2i = new ConstVec2i(y, x)
    def yy: ConstVec2i = new ConstVec2i(y, y)

    def xxx: ConstVec3i = new ConstVec3i(x, x, x)
    def xxy: ConstVec3i = new ConstVec3i(x, x, y)
    def xyx: ConstVec3i = new ConstVec3i(x, y, x)
    def xyy: ConstVec3i = new ConstVec3i(x, y, y)
    def yxx: ConstVec3i = new ConstVec3i(y, x, x)
    def yxy: ConstVec3i = new ConstVec3i(y, x, y)
    def yyx: ConstVec3i = new ConstVec3i(y, y, x)
    def yyy: ConstVec3i = new ConstVec3i(y, y, y)

    def xxxx: ConstVec4i = new ConstVec4i(x, x, x, x)
    def xxxy: ConstVec4i = new ConstVec4i(x, x, x, y)
    def xxyx: ConstVec4i = new ConstVec4i(x, x, y, x)
    def xxyy: ConstVec4i = new ConstVec4i(x, x, y, y)
    def xyxx: ConstVec4i = new ConstVec4i(x, y, x, x)
    def xyxy: ConstVec4i = new ConstVec4i(x, y, x, y)
    def xyyx: ConstVec4i = new ConstVec4i(x, y, y, x)
    def xyyy: ConstVec4i = new ConstVec4i(x, y, y, y)
    def yxxx: ConstVec4i = new ConstVec4i(y, x, x, x)
    def yxxy: ConstVec4i = new ConstVec4i(y, x, x, y)
    def yxyx: ConstVec4i = new ConstVec4i(y, x, y, x)
    def yxyy: ConstVec4i = new ConstVec4i(y, x, y, y)
    def yyxx: ConstVec4i = new ConstVec4i(y, y, x, x)
    def yyxy: ConstVec4i = new ConstVec4i(y, y, x, y)
    def yyyx: ConstVec4i = new ConstVec4i(y, y, y, x)
    def yyyy: ConstVec4i = new ConstVec4i(y, y, y, y)

    def rr = xx
    def rg = xy
    def gr = yx
    def gg = yy

    def rrr = xxx
    def rrg = xxy
    def rgr = xyx
    def rgg = xyy
    def grr = yxx
    def grg = yxy
    def ggr = yyx
    def ggg = yyy

    def rrrr = xxxx
    def rrrg = xxxy
    def rrgr = xxyx
    def rrgg = xxyy
    def rgrr = xyxx
    def rgrg = xyxy
    def rggr = xyyx
    def rggg = xyyy
    def grrr = yxxx
    def grrg = yxxy
    def grgr = yxyx
    def grgg = yxyy
    def ggrr = yyxx
    def ggrg = yyxy
    def gggr = yyyx
    def gggg = yyyy
}

final class ConstVec2i private[math] (val x: Int, val y: Int) extends AnyVec2i

object ConstVec2i {
    def apply(x: Int, y: Int) = new ConstVec2i(x, y)
    def apply(u: AnyVec2i) = new ConstVec2i(u.x, u.y)

    implicit def mutableToConst(u: Vec2i) = new ConstVec2i(u.x, u.y)
}


final class Vec2i private[math] (var x: Int, var y: Int) extends AnyVec2i {

    override def r = x
    override def g = y

    def r_=(r: Int) { x = r }
    def g_=(g: Int) { y = g }


    def *=(s: Int) { x *= s; y *= s }
    def /=(s: Int) { x /= s; y /= s }
    def %=(s: Int) { x %= s; y %= s }
    def >>=(s: Int) = { x >>= s; y >>= s }
    def >>>=(s: Int) = { x >>>= s; y >>>= s }
    def <<=(s: Int) = { x <<= s; y <<= s }
    def &=(s: Int) = { x &= s; y &= s }
    def |=(s: Int) = { x |= s; y |= s }
    def ^=(s: Int) = { x ^= s; y ^= s }

    def +=(u: AnyVec2i) { x += u.x; y += u.y }
    def -=(u: AnyVec2i) { x -= u.x; y -= u.y }
    def *=(u: AnyVec2i) { x *= u.x; y *= u.y }
    def /=(u: AnyVec2i) { x /= u.x; y /= u.y }
    def %=(u: AnyVec2i) { x %= u.x; y %= u.y }
    def >>=(u: AnyVec2i) = { x >>= u.x; y >>= u.y }
    def >>>=(u: AnyVec2i) = { x >>>= u.x; y >>>= u.y }
    def <<=(u: AnyVec2i) = { x <<= u.x; y <<= u.y }
    def &=(u: AnyVec2i) = { x &= u.x; y &= u.y }
    def |=(u: AnyVec2i) = { x |= u.x; y |= u.y }
    def ^=(u: AnyVec2i) = { x ^= u.x; y ^= u.y }

    def :=(u: AnyVec2i) { x = u.x; y = u.y }
    def set(x: Int, y: Int) { this.x = x; this.y = y }

    def update(i: Int, s: Int) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2i = new ConstVec2i(x, y)
    override def yx: ConstVec2i = new ConstVec2i(y, x)

    override def rg = xy
    override def gr = yx

    def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
    def yx_=(u: AnyVec2i) { var t = u.y; y = u.x; x = t }

    def rg_=(u: AnyVec2i) { xy_=(u) }
    def gr_=(u: AnyVec2i) { yx_=(u) }
}

object Vec2i {
    val Origin = new ConstVec2i(0, 0)
    val UnitX = new ConstVec2i(1, 0)
    val UnitY = new ConstVec2i(0, 1)

    def apply(s: Int) = new Vec2i(s, s)
    def apply(x: Int, y: Int) = new Vec2i(x, y)
    def apply(u: AnyVec2i) = new Vec2i(u.x, u.y)
    def apply(u: AnyVec3i) = new Vec2i(u.x, u.y)
    def apply(u: AnyVec4i) = new Vec2i(u.x, u.y)
    def apply(u: Read2Float) = new Vec2i(int(u.x), int(u.y))
    def apply(u: Read3Float) = new Vec2i(int(u.x), int(u.y))
    def apply(u: Read4Float) = new Vec2i(int(u.x), int(u.y))
    def apply(u: Read2Double) = new Vec2i(int(u.x), int(u.y))
    def apply(u: Read3Double) = new Vec2i(int(u.x), int(u.y))
    def apply(u: Read4Double) = new Vec2i(int(u.x), int(u.y))

    implicit def constToMutable(u: ConstVec2i) = Vec2i(u)
}
