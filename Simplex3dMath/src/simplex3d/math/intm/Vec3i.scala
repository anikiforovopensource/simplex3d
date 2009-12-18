/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3i extends Read3Int {

    def r = x
    def g = y
    def b = z


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = new Vec3i(-x, -y, -z)
    def unary_~() = new Vec3i(~x, ~y, ~z)

    def *(s: Int) = new Vec3i(x * s, y * s, z * s)
    def /(s: Int) = new Vec3i(x / s, y / s, z / s)
    private[math] def divideByComponent(s: Int) = new Vec3i(s / x, s / y, s / z)
    def %(s: Int) = new Vec3i(x % s, y % s, z % s)
    private[math] def modByComponent(s: Int) = new Vec3i(s % x, s % y, s % z)
    def >>(s: Int) = new Vec3i( x >> s, y >> s, z >> s)
    def >>>(s: Int) = new Vec3i( x >>> s, y >>> s, z >>> s)
    def <<(s: Int) = new Vec3i( x << s, y << s, z << s)
    def &(s: Int) = new Vec3i( x & s, y & s, z & s)
    def |(s: Int) = new Vec3i( x | s, y | s, z | s)
    def ^(s: Int) = new Vec3i( x ^ s, y ^ s, z ^ s)

    def +(u: AnyVec3i) = new Vec3i(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3i) = new Vec3i(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3i) = new Vec3i(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3i) = new Vec3i(x / u.x, y / u.y, z / u.z)
    def %(u: AnyVec3i) = new Vec3i(x % u.x, y % u.y, z % u.z)
    def >>(u: AnyVec3i) = new Vec3i( x >> u.x, y >> u.y, z >> u.z)
    def >>>(u: AnyVec3i) = new Vec3i( x >>> u.x, y >>> u.y, z >>> u.z)
    def <<(u: AnyVec3i) = new Vec3i( x << u.x, y << u.y, z << u.z)
    def &(u: AnyVec3i) = new Vec3i( x & u.x, y & u.y, z & u.z)
    def |(u: AnyVec3i) = new Vec3i( x | u.x, y | u.y, z | u.z)
    def ^(u: AnyVec3i) = new Vec3i( x ^ u.x, y ^ u.y, z ^ u.z)

    def ==(u: AnyVec3i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3i) :Boolean = !(this == u)

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

    // Swizzling
    def xx: ConstVec2i = new ConstVec2i(x, x)
    def xy: ConstVec2i = new ConstVec2i(x, y)
    def xz: ConstVec2i = new ConstVec2i(x, z)
    def yx: ConstVec2i = new ConstVec2i(y, x)
    def yy: ConstVec2i = new ConstVec2i(y, y)
    def yz: ConstVec2i = new ConstVec2i(y, z)
    def zx: ConstVec2i = new ConstVec2i(z, x)
    def zy: ConstVec2i = new ConstVec2i(z, y)
    def zz: ConstVec2i = new ConstVec2i(z, z)

    def xxx: ConstVec3i = new ConstVec3i(x, x, x)
    def xxy: ConstVec3i = new ConstVec3i(x, x, y)
    def xxz: ConstVec3i = new ConstVec3i(x, x, z)
    def xyx: ConstVec3i = new ConstVec3i(x, y, x)
    def xyy: ConstVec3i = new ConstVec3i(x, y, y)
    def xyz: ConstVec3i = new ConstVec3i(x, y, z)
    def xzx: ConstVec3i = new ConstVec3i(x, z, x)
    def xzy: ConstVec3i = new ConstVec3i(x, z, y)
    def xzz: ConstVec3i = new ConstVec3i(x, z, z)
    def yxx: ConstVec3i = new ConstVec3i(y, x, x)
    def yxy: ConstVec3i = new ConstVec3i(y, x, y)
    def yxz: ConstVec3i = new ConstVec3i(y, x, z)
    def yyx: ConstVec3i = new ConstVec3i(y, y, x)
    def yyy: ConstVec3i = new ConstVec3i(y, y, y)
    def yyz: ConstVec3i = new ConstVec3i(y, y, z)
    def yzx: ConstVec3i = new ConstVec3i(y, z, x)
    def yzy: ConstVec3i = new ConstVec3i(y, z, y)
    def yzz: ConstVec3i = new ConstVec3i(y, z, z)
    def zxx: ConstVec3i = new ConstVec3i(z, x, x)
    def zxy: ConstVec3i = new ConstVec3i(z, x, y)
    def zxz: ConstVec3i = new ConstVec3i(z, x, z)
    def zyx: ConstVec3i = new ConstVec3i(z, y, x)
    def zyy: ConstVec3i = new ConstVec3i(z, y, y)
    def zyz: ConstVec3i = new ConstVec3i(z, y, z)
    def zzx: ConstVec3i = new ConstVec3i(z, z, x)
    def zzy: ConstVec3i = new ConstVec3i(z, z, y)
    def zzz: ConstVec3i = new ConstVec3i(z, z, z)

    def xxxx: ConstVec4i = new ConstVec4i(x, x, x, x)
    def xxxy: ConstVec4i = new ConstVec4i(x, x, x, y)
    def xxxz: ConstVec4i = new ConstVec4i(x, x, x, z)
    def xxyx: ConstVec4i = new ConstVec4i(x, x, y, x)
    def xxyy: ConstVec4i = new ConstVec4i(x, x, y, y)
    def xxyz: ConstVec4i = new ConstVec4i(x, x, y, z)
    def xxzx: ConstVec4i = new ConstVec4i(x, x, z, x)
    def xxzy: ConstVec4i = new ConstVec4i(x, x, z, y)
    def xxzz: ConstVec4i = new ConstVec4i(x, x, z, z)
    def xyxx: ConstVec4i = new ConstVec4i(x, y, x, x)
    def xyxy: ConstVec4i = new ConstVec4i(x, y, x, y)
    def xyxz: ConstVec4i = new ConstVec4i(x, y, x, z)
    def xyyx: ConstVec4i = new ConstVec4i(x, y, y, x)
    def xyyy: ConstVec4i = new ConstVec4i(x, y, y, y)
    def xyyz: ConstVec4i = new ConstVec4i(x, y, y, z)
    def xyzx: ConstVec4i = new ConstVec4i(x, y, z, x)
    def xyzy: ConstVec4i = new ConstVec4i(x, y, z, y)
    def xyzz: ConstVec4i = new ConstVec4i(x, y, z, z)
    def xzxx: ConstVec4i = new ConstVec4i(x, z, x, x)
    def xzxy: ConstVec4i = new ConstVec4i(x, z, x, y)
    def xzxz: ConstVec4i = new ConstVec4i(x, z, x, z)
    def xzyx: ConstVec4i = new ConstVec4i(x, z, y, x)
    def xzyy: ConstVec4i = new ConstVec4i(x, z, y, y)
    def xzyz: ConstVec4i = new ConstVec4i(x, z, y, z)
    def xzzx: ConstVec4i = new ConstVec4i(x, z, z, x)
    def xzzy: ConstVec4i = new ConstVec4i(x, z, z, y)
    def xzzz: ConstVec4i = new ConstVec4i(x, z, z, z)
    def yxxx: ConstVec4i = new ConstVec4i(y, x, x, x)
    def yxxy: ConstVec4i = new ConstVec4i(y, x, x, y)
    def yxxz: ConstVec4i = new ConstVec4i(y, x, x, z)
    def yxyx: ConstVec4i = new ConstVec4i(y, x, y, x)
    def yxyy: ConstVec4i = new ConstVec4i(y, x, y, y)
    def yxyz: ConstVec4i = new ConstVec4i(y, x, y, z)
    def yxzx: ConstVec4i = new ConstVec4i(y, x, z, x)
    def yxzy: ConstVec4i = new ConstVec4i(y, x, z, y)
    def yxzz: ConstVec4i = new ConstVec4i(y, x, z, z)
    def yyxx: ConstVec4i = new ConstVec4i(y, y, x, x)
    def yyxy: ConstVec4i = new ConstVec4i(y, y, x, y)
    def yyxz: ConstVec4i = new ConstVec4i(y, y, x, z)
    def yyyx: ConstVec4i = new ConstVec4i(y, y, y, x)
    def yyyy: ConstVec4i = new ConstVec4i(y, y, y, y)
    def yyyz: ConstVec4i = new ConstVec4i(y, y, y, z)
    def yyzx: ConstVec4i = new ConstVec4i(y, y, z, x)
    def yyzy: ConstVec4i = new ConstVec4i(y, y, z, y)
    def yyzz: ConstVec4i = new ConstVec4i(y, y, z, z)
    def yzxx: ConstVec4i = new ConstVec4i(y, z, x, x)
    def yzxy: ConstVec4i = new ConstVec4i(y, z, x, y)
    def yzxz: ConstVec4i = new ConstVec4i(y, z, x, z)
    def yzyx: ConstVec4i = new ConstVec4i(y, z, y, x)
    def yzyy: ConstVec4i = new ConstVec4i(y, z, y, y)
    def yzyz: ConstVec4i = new ConstVec4i(y, z, y, z)
    def yzzx: ConstVec4i = new ConstVec4i(y, z, z, x)
    def yzzy: ConstVec4i = new ConstVec4i(y, z, z, y)
    def yzzz: ConstVec4i = new ConstVec4i(y, z, z, z)
    def zxxx: ConstVec4i = new ConstVec4i(z, x, x, x)
    def zxxy: ConstVec4i = new ConstVec4i(z, x, x, y)
    def zxxz: ConstVec4i = new ConstVec4i(z, x, x, z)
    def zxyx: ConstVec4i = new ConstVec4i(z, x, y, x)
    def zxyy: ConstVec4i = new ConstVec4i(z, x, y, y)
    def zxyz: ConstVec4i = new ConstVec4i(z, x, y, z)
    def zxzx: ConstVec4i = new ConstVec4i(z, x, z, x)
    def zxzy: ConstVec4i = new ConstVec4i(z, x, z, y)
    def zxzz: ConstVec4i = new ConstVec4i(z, x, z, z)
    def zyxx: ConstVec4i = new ConstVec4i(z, y, x, x)
    def zyxy: ConstVec4i = new ConstVec4i(z, y, x, y)
    def zyxz: ConstVec4i = new ConstVec4i(z, y, x, z)
    def zyyx: ConstVec4i = new ConstVec4i(z, y, y, x)
    def zyyy: ConstVec4i = new ConstVec4i(z, y, y, y)
    def zyyz: ConstVec4i = new ConstVec4i(z, y, y, z)
    def zyzx: ConstVec4i = new ConstVec4i(z, y, z, x)
    def zyzy: ConstVec4i = new ConstVec4i(z, y, z, y)
    def zyzz: ConstVec4i = new ConstVec4i(z, y, z, z)
    def zzxx: ConstVec4i = new ConstVec4i(z, z, x, x)
    def zzxy: ConstVec4i = new ConstVec4i(z, z, x, y)
    def zzxz: ConstVec4i = new ConstVec4i(z, z, x, z)
    def zzyx: ConstVec4i = new ConstVec4i(z, z, y, x)
    def zzyy: ConstVec4i = new ConstVec4i(z, z, y, y)
    def zzyz: ConstVec4i = new ConstVec4i(z, z, y, z)
    def zzzx: ConstVec4i = new ConstVec4i(z, z, z, x)
    def zzzy: ConstVec4i = new ConstVec4i(z, z, z, y)
    def zzzz: ConstVec4i = new ConstVec4i(z, z, z, z)

    def rr = xx
    def rg = xy
    def rb = xz
    def gr = yx
    def gg = yy
    def gb = yz
    def br = zx
    def bg = zy
    def bb = zz

    def rrr = xxx
    def rrg = xxy
    def rrb = xxz
    def rgr = xyx
    def rgg = xyy
    def rgb = xyz
    def rbr = xzx
    def rbg = xzy
    def rbb = xzz
    def grr = yxx
    def grg = yxy
    def grb = yxz
    def ggr = yyx
    def ggg = yyy
    def ggb = yyz
    def gbr = yzx
    def gbg = yzy
    def gbb = yzz
    def brr = zxx
    def brg = zxy
    def brb = zxz
    def bgr = zyx
    def bgg = zyy
    def bgb = zyz
    def bbr = zzx
    def bbg = zzy
    def bbb = zzz

    def rrrr = xxxx
    def rrrg = xxxy
    def rrrb = xxxz
    def rrgr = xxyx
    def rrgg = xxyy
    def rrgb = xxyz
    def rrbr = xxzx
    def rrbg = xxzy
    def rrbb = xxzz
    def rgrr = xyxx
    def rgrg = xyxy
    def rgrb = xyxz
    def rggr = xyyx
    def rggg = xyyy
    def rggb = xyyz
    def rgbr = xyzx
    def rgbg = xyzy
    def rgbb = xyzz
    def rbrr = xzxx
    def rbrg = xzxy
    def rbrb = xzxz
    def rbgr = xzyx
    def rbgg = xzyy
    def rbgb = xzyz
    def rbbr = xzzx
    def rbbg = xzzy
    def rbbb = xzzz
    def grrr = yxxx
    def grrg = yxxy
    def grrb = yxxz
    def grgr = yxyx
    def grgg = yxyy
    def grgb = yxyz
    def grbr = yxzx
    def grbg = yxzy
    def grbb = yxzz
    def ggrr = yyxx
    def ggrg = yyxy
    def ggrb = yyxz
    def gggr = yyyx
    def gggg = yyyy
    def gggb = yyyz
    def ggbr = yyzx
    def ggbg = yyzy
    def ggbb = yyzz
    def gbrr = yzxx
    def gbrg = yzxy
    def gbrb = yzxz
    def gbgr = yzyx
    def gbgg = yzyy
    def gbgb = yzyz
    def gbbr = yzzx
    def gbbg = yzzy
    def gbbb = yzzz
    def brrr = zxxx
    def brrg = zxxy
    def brrb = zxxz
    def brgr = zxyx
    def brgg = zxyy
    def brgb = zxyz
    def brbr = zxzx
    def brbg = zxzy
    def brbb = zxzz
    def bgrr = zyxx
    def bgrg = zyxy
    def bgrb = zyxz
    def bggr = zyyx
    def bggg = zyyy
    def bggb = zyyz
    def bgbr = zyzx
    def bgbg = zyzy
    def bgbb = zyzz
    def bbrr = zzxx
    def bbrg = zzxy
    def bbrb = zzxz
    def bbgr = zzyx
    def bbgg = zzyy
    def bbgb = zzyz
    def bbbr = zzzx
    def bbbg = zzzy
    def bbbb = zzzz
}

final class ConstVec3i private[math] (val x: Int, val y: Int, val z: Int)
extends AnyVec3i

object ConstVec3i {
    def apply(x: Int, y: Int, z: Int) = new ConstVec3i(x, y, z)
    def apply(u: AnyVec3i) = new ConstVec3i(u.x, u.y, u.z)

    implicit def mutableToConst(u: Vec3i) = new ConstVec3i(u.x, u.y, u.z)
}


final class Vec3i private[math] (var x: Int, var y: Int, var z: Int)
extends AnyVec3i
{
    override def r = x
    override def g = y
    override def b = z

    def r_=(r: Int) { x = r }
    def g_=(g: Int) { y = g }
    def b_=(b: Int) { z = b }


    def *=(s: Int) { x *= s; y *= s; z *= s }
    def /=(s: Int) { x /= s; y /= s; z /= s }
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

    // Swizzling
    override def xy: ConstVec2i = new ConstVec2i(x, y)
    override def xz: ConstVec2i = new ConstVec2i(x, z)
    override def yx: ConstVec2i = new ConstVec2i(y, x)
    override def yz: ConstVec2i = new ConstVec2i(y, z)
    override def zx: ConstVec2i = new ConstVec2i(z, x)
    override def zy: ConstVec2i = new ConstVec2i(z, y)

    override def xyz: ConstVec3i = new ConstVec3i(x, y, z)
    override def xzy: ConstVec3i = new ConstVec3i(x, z, y)
    override def yxz: ConstVec3i = new ConstVec3i(y, x, z)
    override def yzx: ConstVec3i = new ConstVec3i(y, z, x)
    override def zxy: ConstVec3i = new ConstVec3i(z, x, y)
    override def zyx: ConstVec3i = new ConstVec3i(z, y, x)

    override def rg = xy
    override def rb = xz
    override def gr = yx
    override def gb = yz
    override def br = zx
    override def bg = zy

    override def rgb = xyz
    override def rbg = xzy
    override def grb = yxz
    override def gbr = yzx
    override def brg = zxy
    override def bgr = zyx


    def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2i) { x = u.x; z = u.y }
    def yx_=(u: AnyVec2i) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2i) { y = u.x; z = u.y }
    def zx_=(u: AnyVec2i) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2i) { z = u.x; y = u.y }

    def xyz_=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
    def xzy_=(u: AnyVec3i) { x = u.x; var t = u.z; z = u.y; y = t }
    def yxz_=(u: AnyVec3i) { var t = u.y; y = u.x; x = t; z = u.z }
    def yzx_=(u: AnyVec3i) { var t = u.y; y = u.x; x = u.z; z = t }
    def zxy_=(u: AnyVec3i) { var t = u.z; z = u.x; x = u.y; y = t }
    def zyx_=(u: AnyVec3i) { var t = u.z; z = u.x; x = t; y = u.y }

    def rg_=(u: AnyVec2i) { xy_=(u) }
    def rb_=(u: AnyVec2i) { xz_=(u) }
    def gr_=(u: AnyVec2i) { yx_=(u) }
    def gb_=(u: AnyVec2i) { yz_=(u) }
    def br_=(u: AnyVec2i) { zx_=(u) }
    def bg_=(u: AnyVec2i) { zy_=(u) }

    def rgb_=(u: AnyVec3i) { xyz_=(u) }
    def rbg_=(u: AnyVec3i) { xzy_=(u) }
    def grb_=(u: AnyVec3i) { yxz_=(u) }
    def gbr_=(u: AnyVec3i) { yzx_=(u) }
    def brg_=(u: AnyVec3i) { zxy_=(u) }
    def bgr_=(u: AnyVec3i) { zyx_=(u) }
}

object Vec3i {
    val Origin = new ConstVec3i(0, 0, 0)
    val UnitX = new ConstVec3i(1, 0, 0)
    val UnitY = new ConstVec3i(0, 1, 0)
    val UnitZ = new ConstVec3i(0, 0, 1)

    def apply(s: Int) = new Vec3i(s, s, s)
    def apply(x: Int, y: Int, z: Int) = new Vec3i(x, y, z)
    def apply(u: AnyVec3i) = new Vec3i(u.x, u.y, u.z)
    def apply(u: AnyVec4i) = new Vec3i(u.x, u.y, u.z)
    def apply(xy: AnyVec2i, z: Int) = new Vec3i(xy.x, xy.y, z)
    def apply(x: Int, yz: AnyVec2i) = new Vec3i(x, yz.x, yz.y)
    def apply(u: Read3Float) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Float) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read3Double) = new Vec3i(int(u.x), int(u.y), int(u.z))
    def apply(u: Read4Double) = new Vec3i(int(u.x), int(u.y), int(u.z))

    implicit def constToMutable(u: ConstVec3i) = Vec3i(u)
}
