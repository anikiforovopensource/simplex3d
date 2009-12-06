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


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3i extends Read3Int {

    def x: Int
    def y: Int
    def z: Int

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = Vec3i(-x, -y, -z)
    def unary_~() = Vec3i(~x, ~y, ~z)

    def *(s: Int) = Vec3i(x*s, y*s, z*s)
    def /(s: Int) = Vec3i(x/s, y/s, z/s)
    private[math] def divideByComponent(s: Int) = Vec3i(s/x, s/y, s/z)
    def %(s: Int) = Vec3i(x % s, y % s, z % s)
    def >>(s: Int) = Vec3i( x >> s, y >> s, z >> s)
    def >>>(s: Int) = Vec3i( x >>> s, y >>> s, z >>> s)
    def <<(s: Int) = Vec3i( x << s, y << s, z << s)
    def &(s: Int) = Vec3i( x & s, y & s, z & s)
    def |(s: Int) = Vec3i( x | s, y | s, z | s)
    def ^(s: Int) = Vec3i( x ^ s, y ^ s, z ^ s)

    def +(u: AnyVec3i) = Vec3i(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3i) = Vec3i(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3i) = Vec3i(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3i) = Vec3i(x / u.x, y / u.y, z / u.z)
    def %(u: AnyVec3i) = Vec3i(x % u.x, y % u.y, z % u.z)
    def >>(u: AnyVec3i) = Vec3i( x >> u.x, y >> u.y, z >> u.z)
    def >>>(u: AnyVec3i) = Vec3i( x >>> u.x, y >>> u.y, z >>> u.z)
    def <<(u: AnyVec3i) = Vec3i( x << u.x, y << u.y, z << u.z)
    def &(u: AnyVec3i) = Vec3i( x & u.x, y & u.y, z & u.z)
    def |(u: AnyVec3i) = Vec3i( x | u.x, y | u.y, z | u.z)
    def ^(u: AnyVec3i) = Vec3i( x ^ u.x, y ^ u.y, z ^ u.z)

    def ==(u: AnyVec3i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3i) :Boolean = !(this == u)

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

}

final class ConstVec3i private (val x: Int, val y: Int, val z: Int)
extends AnyVec3i

object ConstVec3i {
    def apply(s: Int) = new ConstVec3i(s, s, s)
    def apply(x: Int, y: Int, z: Int) = new ConstVec3i(x, y, z)
    def apply(u: AnyVec3i) = new ConstVec3i(u.x, u.y, u.z)
    def apply(u: AnyVec4i) = new ConstVec3i(u.x, u.y, u.z)
    def apply(xy: AnyVec2i, z: Int) = new ConstVec3i(xy.x, xy.y, z)
    def apply(x: Int, yz: AnyVec2i) = new ConstVec3i(x, yz.x, yz.y)
    def apply(u: Read3Float) = new ConstVec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Float) = new ConstVec3i(int(u.x), int(u.y), int(u.z))
    def apply(xy: Read2Float, z: Int) = {
        new ConstVec3i(int(xy.x), int(xy.y), z)
    }
    def apply(x: Int, yz: Read2Float) = {
        new ConstVec3i(x, int(yz.x), int(yz.y))
    }
    def apply(u: Read3Double) = new ConstVec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Double) = new ConstVec3i(int(u.x), int(u.y), int(u.z))
    def apply(xy: Read2Double, z: Int) = {
        new ConstVec3i(int(xy.x), int(xy.y), z)
    }
    def apply(x: Int, yz: Read2Double) = {
        new ConstVec3i(x, int(yz.x), int(yz.y))
    }

    implicit def mutableToConst(u: Vec3i) = ConstVec3i(u)
    implicit def constVec3iToSwizzled(u: ConstVec3i) = new ConstVec3iSwizzled(u)
}

final class Vec3i private (var x: Int, var y: Int, var z: Int)
extends AnyVec3i
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Int) { x = r }
    def g_=(g: Int) { y = g }
    def b_=(b: Int) { z = b }

    def s_=(s: Int) { x = s }
    def t_=(t: Int) { y = t }
    def p_=(p: Int) { z = p }


    def *=(s: Int) { x *= s; y *= s; z *= s }
    def /=(s: Int) { val inv = 1/s; x *= inv; y *= inv; z *= inv }
    def %=(s: Int) { x %= s; y %= s; z %= s }
    def >>=(s: Int) = { x >>= s; y >>= s; z >>= s }
    def >>>=(s: Int) = { x >>>= s; y >>>= s; z >>>= s }
    def <<=(s: Int) = { x <<= s; y <<= s; z <<= s }
    def &=(s: Int) = { x &= s; y &= s; z &= s }
    def |=(s: Int) = { x |= s; y |= s; z |= s }
    def ^=(s: Int) = { x ^= s; y ^= s; z ^= s }

    def +=(u: AnyVec3i) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3i) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3i) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3i) { x /= u.x; y /= u.y; z /= u.z }
    def %=(u: AnyVec3i) { x %= u.x; y %= u.y; z %= u.z }
    def >>=(u: AnyVec3i) = { x >>= u.x; y >>= u.y; z >>= u.z }
    def >>>=(u: AnyVec3i) = { x >>>= u.x; y >>>= u.y; z >>>= u.z }
    def <<=(u: AnyVec3i) = { x <<= u.x; y <<= u.y; z <<= u.z }
    def &=(u: AnyVec3i) = { x &= u.x; y &= u.y; z &= u.z }
    def |=(u: AnyVec3i) = { x |= u.x; y |= u.y; z |= u.z }
    def ^=(u: AnyVec3i) = { x ^= u.x; y ^= u.y; z ^= u.z }

    def :=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
    def set(x: Int, y: Int, z: Int) { this.x = x; this.y = y; this.z = z }

    def update(i: Int, s: Int) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Vec3i {
    val Origin = ConstVec3i(0)
    val UnitX = ConstVec3i(1, 0, 0)
    val UnitY = ConstVec3i(0, 1, 0)
    val UnitZ = ConstVec3i(0, 0, 1)

    def apply(s: Int) = new Vec3i(s, s, s)
    def apply(x: Int, y: Int, z: Int) = new Vec3i(x, y, z)
    def apply(u: AnyVec3i) = new Vec3i(u.x, u.y, u.z)
    def apply(u: AnyVec4i) = new Vec3i(u.x, u.y, u.z)
    def apply(xy: AnyVec2i, z: Int) = new Vec3i(xy.x, xy.y, z)
    def apply(x: Int, yz: AnyVec2i) = new Vec3i(x, yz.x, yz.y)
    def apply(u: Read3Float) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Float) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(xy: Read2Float, z: Int) = new Vec3i(int(xy.x), int(xy.y), z)
    def apply(x: Int, yz: Read2Float) = new Vec3i(x, int(yz.x), int(yz.y))
    def apply(u: Read3Double) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Double) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(xy: Read2Double, z: Int) = new Vec3i(int(xy.x), int(xy.y), z)
    def apply(x: Int, yz: Read2Double) = new Vec3i(x, int(yz.x), int(yz.y))

    implicit def constToMutable(u: ConstVec3i) = Vec3i(u)
    implicit def vec3iToSwizzled(u: Vec3i) = new Vec3iSwizzled(u)
}

private[math] class ConstVec3iSwizzled(u: AnyVec3i)
extends ConstVec2iSwizzled(null)
with Swizzle3Read[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    override def x = u.x
    override def y = u.y
    def z = u.z
}

private[math] class Vec3iSwizzled(u: Vec3i) extends ConstVec3iSwizzled(u)
with Swizzle3Write[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    def x_=(x: Int) { u.x = x }
    def y_=(y: Int) { u.y = y }
    def z_=(z: Int) { u.z = z }
}
