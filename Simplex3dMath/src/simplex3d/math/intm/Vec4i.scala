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
sealed abstract class AnyVec4i extends Read4Int {

    def x: Int
    def y: Int
    def z: Int
    def w: Int

    def r = x
    def g = y
    def b = z
    def a = w

    def s = x
    def t = y
    def p = z
    def q = w


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = Vec4i(-x, -y, -z, -w)
    def unary_~() = Vec4i(~x, ~y, ~z, ~w)

    def *(s: Int) = Vec4i(x*s, y*s, z*s, w*s)
    def /(s: Int) = Vec4i(x/s, y/s, z/s, w/s)
    private[math] def divByComponent(s: Int) = Vec4i(s/x, s/y, s/z, s/w)
    def %(s: Int) = Vec4i(x % s, y % s, z % s, w % s)
    def >>(s: Int) = Vec4i( x >> s, y >> s, z >> s, w >> s)
    def >>>(s: Int) = Vec4i( x >>> s, y >>> s, z >>> s, w >>> s)
    def <<(s: Int) = Vec4i( x << s, y << s, z << s, w << s)
    def &(s: Int) = Vec4i( x & s, y & s, z & s, w & s)
    def |(s: Int) = Vec4i( x | s, y | s, z | s, w | s)
    def ^(s: Int) = Vec4i( x ^ s, y ^ s, z ^ s, w ^ s)

    def +(u: AnyVec4i) = Vec4i(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4i) = Vec4i(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4i) = Vec4i(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4i) = Vec4i(x / u.x, y / u.y, z / u.z, w / u.w)
    def %(u: AnyVec4i) = Vec4i(x % u.x, y % u.y, z % u.z, w % u.w)
    def >>(u: AnyVec4i) = Vec4i( x >> u.x, y >> u.y, z >> u.z, w >> u.w)
    def >>>(u: AnyVec4i) = Vec4i( x >>> u.x, y >>> u.y, z >>> u.z, w >>> u.w)
    def <<(u: AnyVec4i) = Vec4i( x << u.x, y << u.y, z << u.z, w << u.w)
    def &(u: AnyVec4i) = Vec4i( x & u.x, y & u.y, z & u.z, w & u.w)
    def |(u: AnyVec4i) = Vec4i( x | u.x, y | u.y, z | u.z, w | u.w)
    def ^(u: AnyVec4i) = Vec4i( x ^ u.x, y ^ u.y, z ^ u.z, w ^ u.w)

    def ==(u: AnyVec4i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4i) :Boolean = !(this == u)
    
    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }
}

final class ConstVec4i private[math] (
    val x: Int, val y: Int, val z: Int, val w: Int)
extends AnyVec4i


final class Vec4i private[math] (
    var x: Int, var y: Int, var z: Int, var w: Int)
extends AnyVec4i
{
    private[math] def this() = this(0, 0, 0, 0)
    
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    override def s = x
    override def t = y
    override def p = z
    override def q = w

    def r_=(r: Int) { x = r }
    def g_=(g: Int) { y = g }
    def b_=(b: Int) { z = b }
    def a_=(a: Int) { w = a }

    def s_=(s: Int) { x = s }
    def t_=(t: Int) { y = t }
    def p_=(p: Int) { z = p }
    def q_=(q: Int) { w = q }


    def *=(s: Int) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Int) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }
    def %=(s: Int) { x %= s; y %= s; z %= s; w %= s }
    def >>=(s: Int) = { x >>= s; y >>= s; z >>= s; w >>= s }
    def >>>=(s: Int) = { x >>>= s; y >>>= s; z >>>= s; w >>>= s }
    def <<=(s: Int) = { x <<= s; y <<= s; z <<= s; w <<= s }
    def &=(s: Int) = { x &= s; y &= s; z &= s; w &= s }
    def |=(s: Int) = { x |= s; y |= s; z |= s; w |= s }
    def ^=(s: Int) = { x ^= s; y ^= s; z ^= s; w ^= s }

    def +=(u: AnyVec4i) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4i) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4i) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4i) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }
    def %=(u: AnyVec4i) { x %= u.x; y %= u.y; z %= u.z; w %= u.w }
    def >>=(u: AnyVec4i) = { x >>= u.x; y >>= u.y; z >>= u.z; w >>= u.w }
    def >>>=(u: AnyVec4i) = { x >>>= u.x; y >>>= u.y; z >>>= u.z; w >>>= u.w }
    def <<=(u: AnyVec4i) = { x <<= u.x; y <<= u.y; z <<= u.z; w <<= u.w }
    def &=(u: AnyVec4i) = { x &= u.x; y &= u.y; z &= u.z; w &= u.w }
    def |=(u: AnyVec4i) = { x |= u.x; y |= u.y; z |= u.z; w |= u.w }
    def ^=(u: AnyVec4i) = { x ^= u.x; y ^= u.y; z ^= u.z; w ^= u.w }

    def :=(u: AnyVec4i) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Int, y: Int, z: Int, w: Int) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Int) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case 3 => w = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }
}

object Vec4i {
    val Origin = consti(Vec4i(0))
    val UnitX = consti(Vec4i(1, 0, 0, 0))
    val UnitY = consti(Vec4i(0, 1, 0, 0))
    val UnitZ = consti(Vec4i(0, 0, 1, 0))
    val UnitW = consti(Vec4i(0, 0, 0, 1))

    def apply(s: Int) =
        new Vec4i(s, s, s, s)

    def apply(x: Int, y: Int, z: Int, w: Int) =
        new Vec4i(x, y, z, w)

    def apply(u: AnyVec4i) =
        new Vec4i(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2i, z: Int, w: Int) =
        new Vec4i(xy.x, xy.y, z, w)

    def apply(x: Int, yz: AnyVec2i, w: Int) =
        new Vec4i(x, yz.x, yz.y, w)

    def apply(x: Int, y: Int, zw: AnyVec2i) =
        new Vec4i(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2i, zw: AnyVec2i) =
        new Vec4i(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3i, w: Int) =
        new Vec4i(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Int, yzw: AnyVec3i) =
        new Vec4i(x, yzw.x, yzw.y, yzw.z)

    def apply(u: Read4Float) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(xy: Read2Float, z: Int, w: Int) =
        new Vec4i(int(xy.x), int(xy.y), z, w)

    def apply(x: Int, yz: Read2Float, w: Int) =
        new Vec4i(x, int(yz.x), int(yz.y), w)

    def apply(x: Int, y: Int, zw: Read2Float) =
        new Vec4i(x, y, int(zw.x), int(zw.y))

    def apply(xyz: Read3Float, w: Int) =
        new Vec4i(int(xyz.x), int(xyz.y), int(xyz.z), w)

    def apply(x: Int, yzw: Read3Float) =
        new Vec4i(x, int(yzw.x), int(yzw.y), int(yzw.z))

    def apply(u: Read4Double) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(xy: Read2Double, z: Int, w: Int) =
        new Vec4i(int(xy.x), int(xy.y), z, w)

    def apply(x: Int, yz: Read2Double, w: Int) =
        new Vec4i(x, int(yz.x), int(yz.y), w)

    def apply(x: Int, y: Int, zw: Read2Double) =
        new Vec4i(x, y, int(zw.x), int(zw.y))

    def apply(xyz: Read3Double, w: Int) =
        new Vec4i(int(xyz.x), int(xyz.y), int(xyz.z), w)

    def apply(x: Int, yzw: Read3Double) =
        new Vec4i(x, int(yzw.x), int(yzw.y), int(yzw.z))

    def apply(xy: Read2[AnyVal], zw: Read2[AnyVal]) = {
        var x = 0
        var y = 0
        xy match {
            case r: Read2Int => x = r.x; y = r.y
            case r: Read2Float => x = int(r.x); y = int(r.y)
            case r: Read2Double => x = int(r.x); y = int(r.y)
            case _ => throw new IllegalArgumentException("Unexpected type.")
        }
        zw match {
            case r: Read2Int => new Vec4i(x, y, r.x, r.y)
            case r: Read2Float => new Vec4i(x, y, int(r.x), int(r.y))
            case r: Read2Double => new Vec4i(x, y, int(r.x), int(r.y))
            case _ => throw new IllegalArgumentException("Unexpected type.")
        }
    }

    implicit def constToMutable(u: ConstVec4i) = Vec4i(u)
    implicit def vec4iToSwizzled(u: Vec4i) = new Vec4iSwizzled(u)
}

private[math] class ConstVec4iSwizzled(u: AnyVec4i)
extends ConstVec3iSwizzled(null)
with Swizzle4Read[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    override def x = u.x
    override def y = u.y
    override def z = u.z
    def w = u.w
}

private[math] class Vec4iSwizzled(u: Vec4i) extends ConstVec4iSwizzled(u)
with Swizzle4Write[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    def x_=(x: Int) { u.x = x }
    def y_=(y: Int) { u.y = y }
    def z_=(z: Int) { u.z = z }
    def w_=(w: Int) { u.w = w }
}
