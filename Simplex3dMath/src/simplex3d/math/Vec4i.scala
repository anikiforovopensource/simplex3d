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

import VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4i extends Read4[Int] {

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
    private[math] def divideByComponent(s: Int) = Vec4i(s/x, s/y, s/z, s/w)
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

final class ConstVec4i private (val x: Int, val y: Int,
                               val z: Int, val w: Int)
extends AnyVec4i

object ConstVec4i {
    def apply(s: Int) =
        new ConstVec4i(s, s, s, s)

    def apply(x: Int, y: Int, z: Int, w: Int) =
        new ConstVec4i(x, y, z, w)

    def apply(u: AnyVec4i) =
        new ConstVec4i(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2i, z: Int, w: Int) =
        new ConstVec4i(xy.x, xy.y, z, w)

    def apply(x: Int, yz: AnyVec2i, w: Int) =
        new ConstVec4i(x, yz.x, yz.y, w)

    def apply(x: Int, y: Int, zw: AnyVec2i) =
        new ConstVec4i(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2i, zw: AnyVec2i) =
        new ConstVec4i(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3i, w: Int) =
        new ConstVec4i(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Int, yzw: AnyVec3i) =
        new ConstVec4i(x, yzw.x, yzw.y, yzw.z)

    def apply(u: AnyVec4) =
        new ConstVec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(xy: AnyVec2, z: Int, w: Int) =
        new ConstVec4i(int(xy.x), int(xy.y), z, w)

    def apply(x: Int, yz: AnyVec2, w: Int) =
        new ConstVec4i(x, int(yz.x), int(yz.y), w)

    def apply(x: Int, y: Int, zw: AnyVec2) =
        new ConstVec4i(x, y, int(zw.x), int(zw.y))

    def apply(xy: AnyVec2, zw: AnyVec2) =
        new ConstVec4i(int(xy.x), int(xy.y), int(zw.x), int(zw.y))

    def apply(xyz: AnyVec3, w: Int) =
        new ConstVec4i(int(xyz.x), int(xyz.y), int(xyz.z), w)

    def apply(x: Int, yzw: AnyVec3) =
        new ConstVec4i(x, int(yzw.x), int(yzw.y), int(yzw.z))

    def apply(m: AnyMat2) =
        new ConstVec4i(int(m.m00), int(m.m10), int(m.m01), int(m.m11))
        
    implicit def mutableToConst(u: Vec4i) = ConstVec4i(u)
    implicit def constVec4iToSwizzled(u: ConstVec4i) = new ConstVec4iSwizzled(u)
}

final class Vec4i private (var x: Int, var y: Int,
                          var z: Int, var w: Int)
extends AnyVec4i
{
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
    val Origin = ConstVec4i(0)
    val UnitX = ConstVec4i(1, 0, 0, 0)
    val UnitY = ConstVec4i(0, 1, 0, 0)
    val UnitZ = ConstVec4i(0, 0, 1, 0)
    val UnitW = ConstVec4i(0, 0, 0, 1)

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

    def apply(u: AnyVec4) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(xy: AnyVec2, z: Int, w: Int) =
        new Vec4i(int(xy.x), int(xy.y), z, w)

    def apply(x: Int, yz: AnyVec2, w: Int) =
        new Vec4i(x, int(yz.x), int(yz.y), w)

    def apply(x: Int, y: Int, zw: AnyVec2) =
        new Vec4i(x, y, int(zw.x), int(zw.y))

    def apply(xy: AnyVec2, zw: AnyVec2) =
        new Vec4i(int(xy.x), int(xy.y), int(zw.x), int(zw.y))

    def apply(xyz: AnyVec3, w: Int) =
        new Vec4i(int(xyz.x), int(xyz.y), int(xyz.z), w)

    def apply(x: Int, yzw: AnyVec3) =
        new Vec4i(x, int(yzw.x), int(yzw.y), int(yzw.z))

    def apply(m: AnyMat2) =
        new Vec4i(int(m.m00), int(m.m10), int(m.m01), int(m.m11))

    implicit def vec4iToSwizzled(u: Vec4i) = new Vec4iSwizzled(u)
}

private[math] class ConstVec4iSwizzled(u: AnyVec4i) extends IntVecFactory
with Swizzle4Read[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    def x = u.x
    def y = u.y
    def z = u.z
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
