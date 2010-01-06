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
sealed abstract class AnyVec3f extends Read3Float {

    def r = x
    def g = y
    def b = z

    
    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = new Vec3f(-x, -y, -z)
    def *(s: Float) = new Vec3f(x * s, y * s, z * s)
    def /(s: Float) = { val inv = 1/s; new Vec3f(x * inv, y * inv, z * inv) }
    private[math] def divideByComponent(s: Float) = {
        new Vec3f(s / x, s / y, s / z)
    }

    def +(u: AnyVec3f) = new Vec3f(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3f) = new Vec3f(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3f) = new Vec3f(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3f) = new Vec3f(x / u.x, y / u.y, z / u.z)

    def *(m: AnyMat3x2f) :Vec2f = m.transposeMul(this)
    def *(m: AnyMat3f) :Vec3f = m.transposeMul(this)
    def *(m: AnyMat3x4f) :Vec4f = m.transposeMul(this)

    def ==(u: AnyVec3f) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3f) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec3f => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        41 * (
            41 * (
                41 + x.hashCode
            ) + y.hashCode
        ) + z.hashCode
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

    // Swizzling
    def xx: ConstVec2f = new ConstVec2f(x, x)
    def xy: ConstVec2f = new ConstVec2f(x, y)
    def xz: ConstVec2f = new ConstVec2f(x, z)
    def yx: ConstVec2f = new ConstVec2f(y, x)
    def yy: ConstVec2f = new ConstVec2f(y, y)
    def yz: ConstVec2f = new ConstVec2f(y, z)
    def zx: ConstVec2f = new ConstVec2f(z, x)
    def zy: ConstVec2f = new ConstVec2f(z, y)
    def zz: ConstVec2f = new ConstVec2f(z, z)

    def xxx: ConstVec3f = new ConstVec3f(x, x, x)
    def xxy: ConstVec3f = new ConstVec3f(x, x, y)
    def xxz: ConstVec3f = new ConstVec3f(x, x, z)
    def xyx: ConstVec3f = new ConstVec3f(x, y, x)
    def xyy: ConstVec3f = new ConstVec3f(x, y, y)
    def xyz: ConstVec3f = new ConstVec3f(x, y, z)
    def xzx: ConstVec3f = new ConstVec3f(x, z, x)
    def xzy: ConstVec3f = new ConstVec3f(x, z, y)
    def xzz: ConstVec3f = new ConstVec3f(x, z, z)
    def yxx: ConstVec3f = new ConstVec3f(y, x, x)
    def yxy: ConstVec3f = new ConstVec3f(y, x, y)
    def yxz: ConstVec3f = new ConstVec3f(y, x, z)
    def yyx: ConstVec3f = new ConstVec3f(y, y, x)
    def yyy: ConstVec3f = new ConstVec3f(y, y, y)
    def yyz: ConstVec3f = new ConstVec3f(y, y, z)
    def yzx: ConstVec3f = new ConstVec3f(y, z, x)
    def yzy: ConstVec3f = new ConstVec3f(y, z, y)
    def yzz: ConstVec3f = new ConstVec3f(y, z, z)
    def zxx: ConstVec3f = new ConstVec3f(z, x, x)
    def zxy: ConstVec3f = new ConstVec3f(z, x, y)
    def zxz: ConstVec3f = new ConstVec3f(z, x, z)
    def zyx: ConstVec3f = new ConstVec3f(z, y, x)
    def zyy: ConstVec3f = new ConstVec3f(z, y, y)
    def zyz: ConstVec3f = new ConstVec3f(z, y, z)
    def zzx: ConstVec3f = new ConstVec3f(z, z, x)
    def zzy: ConstVec3f = new ConstVec3f(z, z, y)
    def zzz: ConstVec3f = new ConstVec3f(z, z, z)

    def xxxx: ConstVec4f = new ConstVec4f(x, x, x, x)
    def xxxy: ConstVec4f = new ConstVec4f(x, x, x, y)
    def xxxz: ConstVec4f = new ConstVec4f(x, x, x, z)
    def xxyx: ConstVec4f = new ConstVec4f(x, x, y, x)
    def xxyy: ConstVec4f = new ConstVec4f(x, x, y, y)
    def xxyz: ConstVec4f = new ConstVec4f(x, x, y, z)
    def xxzx: ConstVec4f = new ConstVec4f(x, x, z, x)
    def xxzy: ConstVec4f = new ConstVec4f(x, x, z, y)
    def xxzz: ConstVec4f = new ConstVec4f(x, x, z, z)
    def xyxx: ConstVec4f = new ConstVec4f(x, y, x, x)
    def xyxy: ConstVec4f = new ConstVec4f(x, y, x, y)
    def xyxz: ConstVec4f = new ConstVec4f(x, y, x, z)
    def xyyx: ConstVec4f = new ConstVec4f(x, y, y, x)
    def xyyy: ConstVec4f = new ConstVec4f(x, y, y, y)
    def xyyz: ConstVec4f = new ConstVec4f(x, y, y, z)
    def xyzx: ConstVec4f = new ConstVec4f(x, y, z, x)
    def xyzy: ConstVec4f = new ConstVec4f(x, y, z, y)
    def xyzz: ConstVec4f = new ConstVec4f(x, y, z, z)
    def xzxx: ConstVec4f = new ConstVec4f(x, z, x, x)
    def xzxy: ConstVec4f = new ConstVec4f(x, z, x, y)
    def xzxz: ConstVec4f = new ConstVec4f(x, z, x, z)
    def xzyx: ConstVec4f = new ConstVec4f(x, z, y, x)
    def xzyy: ConstVec4f = new ConstVec4f(x, z, y, y)
    def xzyz: ConstVec4f = new ConstVec4f(x, z, y, z)
    def xzzx: ConstVec4f = new ConstVec4f(x, z, z, x)
    def xzzy: ConstVec4f = new ConstVec4f(x, z, z, y)
    def xzzz: ConstVec4f = new ConstVec4f(x, z, z, z)
    def yxxx: ConstVec4f = new ConstVec4f(y, x, x, x)
    def yxxy: ConstVec4f = new ConstVec4f(y, x, x, y)
    def yxxz: ConstVec4f = new ConstVec4f(y, x, x, z)
    def yxyx: ConstVec4f = new ConstVec4f(y, x, y, x)
    def yxyy: ConstVec4f = new ConstVec4f(y, x, y, y)
    def yxyz: ConstVec4f = new ConstVec4f(y, x, y, z)
    def yxzx: ConstVec4f = new ConstVec4f(y, x, z, x)
    def yxzy: ConstVec4f = new ConstVec4f(y, x, z, y)
    def yxzz: ConstVec4f = new ConstVec4f(y, x, z, z)
    def yyxx: ConstVec4f = new ConstVec4f(y, y, x, x)
    def yyxy: ConstVec4f = new ConstVec4f(y, y, x, y)
    def yyxz: ConstVec4f = new ConstVec4f(y, y, x, z)
    def yyyx: ConstVec4f = new ConstVec4f(y, y, y, x)
    def yyyy: ConstVec4f = new ConstVec4f(y, y, y, y)
    def yyyz: ConstVec4f = new ConstVec4f(y, y, y, z)
    def yyzx: ConstVec4f = new ConstVec4f(y, y, z, x)
    def yyzy: ConstVec4f = new ConstVec4f(y, y, z, y)
    def yyzz: ConstVec4f = new ConstVec4f(y, y, z, z)
    def yzxx: ConstVec4f = new ConstVec4f(y, z, x, x)
    def yzxy: ConstVec4f = new ConstVec4f(y, z, x, y)
    def yzxz: ConstVec4f = new ConstVec4f(y, z, x, z)
    def yzyx: ConstVec4f = new ConstVec4f(y, z, y, x)
    def yzyy: ConstVec4f = new ConstVec4f(y, z, y, y)
    def yzyz: ConstVec4f = new ConstVec4f(y, z, y, z)
    def yzzx: ConstVec4f = new ConstVec4f(y, z, z, x)
    def yzzy: ConstVec4f = new ConstVec4f(y, z, z, y)
    def yzzz: ConstVec4f = new ConstVec4f(y, z, z, z)
    def zxxx: ConstVec4f = new ConstVec4f(z, x, x, x)
    def zxxy: ConstVec4f = new ConstVec4f(z, x, x, y)
    def zxxz: ConstVec4f = new ConstVec4f(z, x, x, z)
    def zxyx: ConstVec4f = new ConstVec4f(z, x, y, x)
    def zxyy: ConstVec4f = new ConstVec4f(z, x, y, y)
    def zxyz: ConstVec4f = new ConstVec4f(z, x, y, z)
    def zxzx: ConstVec4f = new ConstVec4f(z, x, z, x)
    def zxzy: ConstVec4f = new ConstVec4f(z, x, z, y)
    def zxzz: ConstVec4f = new ConstVec4f(z, x, z, z)
    def zyxx: ConstVec4f = new ConstVec4f(z, y, x, x)
    def zyxy: ConstVec4f = new ConstVec4f(z, y, x, y)
    def zyxz: ConstVec4f = new ConstVec4f(z, y, x, z)
    def zyyx: ConstVec4f = new ConstVec4f(z, y, y, x)
    def zyyy: ConstVec4f = new ConstVec4f(z, y, y, y)
    def zyyz: ConstVec4f = new ConstVec4f(z, y, y, z)
    def zyzx: ConstVec4f = new ConstVec4f(z, y, z, x)
    def zyzy: ConstVec4f = new ConstVec4f(z, y, z, y)
    def zyzz: ConstVec4f = new ConstVec4f(z, y, z, z)
    def zzxx: ConstVec4f = new ConstVec4f(z, z, x, x)
    def zzxy: ConstVec4f = new ConstVec4f(z, z, x, y)
    def zzxz: ConstVec4f = new ConstVec4f(z, z, x, z)
    def zzyx: ConstVec4f = new ConstVec4f(z, z, y, x)
    def zzyy: ConstVec4f = new ConstVec4f(z, z, y, y)
    def zzyz: ConstVec4f = new ConstVec4f(z, z, y, z)
    def zzzx: ConstVec4f = new ConstVec4f(z, z, z, x)
    def zzzy: ConstVec4f = new ConstVec4f(z, z, z, y)
    def zzzz: ConstVec4f = new ConstVec4f(z, z, z, z)

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

final class ConstVec3f private[math] (val x: Float, val y: Float, val z: Float)
extends AnyVec3f

object ConstVec3f {
    def apply(x: Float, y: Float, z: Float) = new ConstVec3f(x, y, z)
    def apply(u: AnyVec3f) = new ConstVec3f(u.x, u.y, u.z)

    implicit def mutableToConst(u: Vec3f) = new ConstVec3f(u.x, u.y, u.z)
}


final class Vec3f private[math] (var x: Float, var y: Float, var z: Float)
extends AnyVec3f
{
    override def r = x
    override def g = y
    override def b = z

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }
    def b_=(b: Float) { z = b }


    def *=(s: Float) { x *= s; y *= s; z *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(u: AnyVec3f) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3f) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3f) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3f) { x /= u.x; y /= u.y; z /= u.z }

    def *=(m: AnyMat3f) { this := m.transposeMul(this) }

    def :=(u: AnyVec3f) { x = u.x; y = u.y; z = u.z }
    def set(x: Float, y: Float, z: Float) { this.x = x; this.y = y; this.z = z }

    def update(i: Int, s: Float) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2f = new ConstVec2f(x, y)
    override def xz: ConstVec2f = new ConstVec2f(x, z)
    override def yx: ConstVec2f = new ConstVec2f(y, x)
    override def yz: ConstVec2f = new ConstVec2f(y, z)
    override def zx: ConstVec2f = new ConstVec2f(z, x)
    override def zy: ConstVec2f = new ConstVec2f(z, y)

    override def xyz: ConstVec3f = new ConstVec3f(x, y, z)
    override def xzy: ConstVec3f = new ConstVec3f(x, z, y)
    override def yxz: ConstVec3f = new ConstVec3f(y, x, z)
    override def yzx: ConstVec3f = new ConstVec3f(y, z, x)
    override def zxy: ConstVec3f = new ConstVec3f(z, x, y)
    override def zyx: ConstVec3f = new ConstVec3f(z, y, x)

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


    def xy_=(u: AnyVec2f) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2f) { x = u.x; z = u.y }
    def yx_=(u: AnyVec2f) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2f) { y = u.x; z = u.y }
    def zx_=(u: AnyVec2f) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2f) { z = u.x; y = u.y }

    def xyz_=(u: AnyVec3f) { x = u.x; y = u.y; z = u.z }
    def xzy_=(u: AnyVec3f) { x = u.x; var t = u.z; z = u.y; y = t }
    def yxz_=(u: AnyVec3f) { var t = u.y; y = u.x; x = t; z = u.z }
    def yzx_=(u: AnyVec3f) { var t = u.y; y = u.x; x = u.z; z = t }
    def zxy_=(u: AnyVec3f) { var t = u.z; z = u.x; x = u.y; y = t }
    def zyx_=(u: AnyVec3f) { var t = u.z; z = u.x; x = t; y = u.y }

    def rg_=(u: AnyVec2f) { xy_=(u) }
    def rb_=(u: AnyVec2f) { xz_=(u) }
    def gr_=(u: AnyVec2f) { yx_=(u) }
    def gb_=(u: AnyVec2f) { yz_=(u) }
    def br_=(u: AnyVec2f) { zx_=(u) }
    def bg_=(u: AnyVec2f) { zy_=(u) }

    def rgb_=(u: AnyVec3f) { xyz_=(u) }
    def rbg_=(u: AnyVec3f) { xzy_=(u) }
    def grb_=(u: AnyVec3f) { yxz_=(u) }
    def gbr_=(u: AnyVec3f) { yzx_=(u) }
    def brg_=(u: AnyVec3f) { zxy_=(u) }
    def bgr_=(u: AnyVec3f) { zyx_=(u) }
}

object Vec3f {
    val Origin = new ConstVec3f(0, 0, 0)
    val UnitX = new ConstVec3f(1, 0, 0)
    val UnitY = new ConstVec3f(0, 1, 0)
    val UnitZ = new ConstVec3f(0, 0, 1)

    def apply(s: Float) = new Vec3f(s, s, s)
    def apply(x: Float, y: Float, z: Float) = new Vec3f(x, y, z)
    def apply(u: AnyVec3f) = new Vec3f(u.x, u.y, u.z)
    def apply(u: AnyVec4f) = new Vec3f(u.x, u.y, u.z)
    def apply(xy: AnyVec2f, z: Float) = new Vec3f(xy.x, xy.y, z)
    def apply(x: Float, yz: AnyVec2f) = new Vec3f(x, yz.x, yz.y)
    def apply(u: Read3Int) = new Vec3f(u.x, u.y, u.z)
    def apply(u: Read4Int) = new Vec3f(u.x, u.y, u.z)

    def apply(u: Read3Double) =
        new Vec3f(float(u.x), float(u.y), float(u.z))

    def apply(u: Read4Double) =
        new Vec3f(float(u.x), float(u.y), float(u.z))

    implicit def constToMutable(u: ConstVec3f) = Vec3f(u)
}
