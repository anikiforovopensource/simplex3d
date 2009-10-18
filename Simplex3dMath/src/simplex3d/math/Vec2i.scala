/*
 * Simplex3D, Math package
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * CLASSPATH EXCEPTION FOR UNMODIFIED WORK:
 * Linking this library statically or dynamically with other modules is making
 * a combined work based on this library. Thus, the terms and conditions of
 * the GNU General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce
 * an executable, regardless of the license terms of these independent modules,
 * and to copy and distribute the resulting executable under terms of your
 * choice, provided that you also meet, for each linked independent module,
 * the terms and conditions of the license of that module. An independent module
 * is a module which is not derived from or based on this library. If you modify
 * this library in any way, then this exception is null and void and no longer
 * applies, in this case delete this exception statement from your version.
 */

package simplex3d.math

import simplex3d.math.VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2i extends Read2[Int] {

    def x: Int
    def y: Int


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = Vec2i(-x, -y)
    def unary_~() = Vec2i(~x, ~y)

    def *(s: Int) = Vec2i(x*s, y*s)
    def /(s: Int) = { val inv = 1/s; Vec2i(x*inv, y*inv) }
    def %(s: Int) = Vec2i(x % s, y % s)
    def >>(s: Int) = Vec2i( x >> s, y >> s)
    def >>>(s: Int) = Vec2i( x >>> s, y >>> s)
    def <<(s: Int) = Vec2i( x << s, y << s)
    def &(s: Int) = Vec2i( x & s, y & s)
    def |(s: Int) = Vec2i( x | s, y | s)
    def ^(s: Int) = Vec2i( x ^ s, y ^ s)

    def +(u: AnyVec2i) = Vec2i(x + u.x, y + u.y)
    def -(u: AnyVec2i) = Vec2i(x - u.x, y - u.y)
    def *(u: AnyVec2i) = Vec2i(x * u.x, y * u.y)
    def /(u: AnyVec2i) = Vec2i(x / u.x, y / u.y)
    def %(u: AnyVec2i) = Vec2i(x % u.x, y % u.y)
    def >>(u: AnyVec2i) = Vec2i( x >> u.x, y >> u.y)
    def >>>(u: AnyVec2i) = Vec2i( x >>> u.x, y >>> u.y)
    def <<(u: AnyVec2i) = Vec2i( x << u.x, y << u.y)
    def &(u: AnyVec2i) = Vec2i( x & u.x, y & u.y)
    def |(u: AnyVec2i) = Vec2i( x | u.x, y | u.y)
    def ^(u: AnyVec2i) = Vec2i( x ^ u.x, y ^ u.y)

    def ==(u: AnyVec2i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2i) :Boolean = !(this == u)

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2i private (val x: Int, val y: Int) extends AnyVec2i

object ConstVec2i {
    def apply(s: Int) = new ConstVec2i(s, s)
    def apply(x: Int, y: Int) = new ConstVec2i(x, y)
    def apply(u: AnyVec2i) = new ConstVec2i(u.x, u.y)
    def apply(u: AnyVec3i) = new ConstVec2i(u.x, u.y)
    def apply(u: AnyVec4i) = new ConstVec2i(u.x, u.y)
    def apply(u: AnyVec2) = new ConstVec2i(int(u.x), int(u.y))
    def apply(u: AnyVec3) = new ConstVec2i(int(u.x), int(u.y))
    def apply(u: AnyVec4) = new ConstVec2i(int(u.x), int(u.y))

    implicit def mutableToConst(u: Vec2i) = ConstVec2i(u)
    implicit def constVec2iToSwizzled(u: ConstVec2i) = new ConstVec2iSwizzled(u)
}

final class Vec2i private (var x: Int, var y: Int) extends AnyVec2i {
    def *=(s: Int) { x *= s; y *= s }
    def /=(s: Int) { val inv = 1/s; x *= inv; y *= inv }
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
}

object Vec2i {
    val Zero = ConstVec2i(0)
    val One = ConstVec2i(1)
    val UnitX = ConstVec2i(1, 0)
    val UnitY = ConstVec2i(0, 1)

    def apply(s: Int) = new Vec2i(s, s)
    def apply(x: Int, y: Int) = new Vec2i(x, y)
    def apply(u: AnyVec2i) = new Vec2i(u.x, u.y)
    def apply(u: AnyVec3i) = new Vec2i(u.x, u.y)
    def apply(u: AnyVec4i) = new Vec2i(u.x, u.y)
    def apply(u: AnyVec2) = new Vec2i(int(u.x), int(u.y))
    def apply(u: AnyVec3) = new Vec2i(int(u.x), int(u.y))
    def apply(u: AnyVec4) = new Vec2i(int(u.x), int(u.y))

    implicit def vec2iToSwizzled(u: Vec2i) = new Vec2iSwizzled(u)
}

class ConstVec2iSwizzled(u: AnyVec2i) extends VeciSwizzled
with Swizzle2Read[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    def x = u.x
    def y = u.y
}

class Vec2iSwizzled(u: Vec2i) extends ConstVec2iSwizzled(u)
with Swizzle2Write[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    def x_=(x: Int) { u.x = x }
    def y_=(y: Int) { u.y = y }
}
