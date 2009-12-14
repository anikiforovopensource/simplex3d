/*
 * Simplex3D, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3d extends Read3Double {

    def r = x
    def g = y
    def b = z

    
    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_-() = new Vec3d(-x, -y, -z)
    def *(s: Double) = new Vec3d(x * s, y * s, z * s)
    def /(s: Double) = { val inv = 1/s; new Vec3d(x * inv, y * inv, z * inv) }
    private[math] def divideByComponent(s: Double) = {
        new Vec3d(s / x, s / y, s / z)
    }

    def +(u: AnyVec3d) = new Vec3d(x + u.x, y + u.y, z + u.z)
    def -(u: AnyVec3d) = new Vec3d(x - u.x, y - u.y, z - u.z)
    def *(u: AnyVec3d) = new Vec3d(x * u.x, y * u.y, z * u.z)
    def /(u: AnyVec3d) = new Vec3d(x / u.x, y / u.y, z / u.z)

    def *(m: AnyMat3x2d) :Vec2d = m.transposeMul(this)
    def *(m: AnyMat3d) :Vec3d = m.transposeMul(this)
    def *(m: AnyMat3x4d) :Vec4d = m.transposeMul(this)

    def ==(u: AnyVec3d) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3d) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z)
        )
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

    // Swizzling
    def xx: ConstVec2d = new ConstVec2d(x, x)
    def xy: ConstVec2d = new ConstVec2d(x, y)
    def xz: ConstVec2d = new ConstVec2d(x, z)
    def yx: ConstVec2d = new ConstVec2d(y, x)
    def yy: ConstVec2d = new ConstVec2d(y, y)
    def yz: ConstVec2d = new ConstVec2d(y, z)
    def zx: ConstVec2d = new ConstVec2d(z, x)
    def zy: ConstVec2d = new ConstVec2d(z, y)
    def zz: ConstVec2d = new ConstVec2d(z, z)

    def xxx: ConstVec3d = new ConstVec3d(x, x, x)
    def xxy: ConstVec3d = new ConstVec3d(x, x, y)
    def xxz: ConstVec3d = new ConstVec3d(x, x, z)
    def xyx: ConstVec3d = new ConstVec3d(x, y, x)
    def xyy: ConstVec3d = new ConstVec3d(x, y, y)
    def xyz: ConstVec3d = new ConstVec3d(x, y, z)
    def xzx: ConstVec3d = new ConstVec3d(x, z, x)
    def xzy: ConstVec3d = new ConstVec3d(x, z, y)
    def xzz: ConstVec3d = new ConstVec3d(x, z, z)
    def yxx: ConstVec3d = new ConstVec3d(y, x, x)
    def yxy: ConstVec3d = new ConstVec3d(y, x, y)
    def yxz: ConstVec3d = new ConstVec3d(y, x, z)
    def yyx: ConstVec3d = new ConstVec3d(y, y, x)
    def yyy: ConstVec3d = new ConstVec3d(y, y, y)
    def yyz: ConstVec3d = new ConstVec3d(y, y, z)
    def yzx: ConstVec3d = new ConstVec3d(y, z, x)
    def yzy: ConstVec3d = new ConstVec3d(y, z, y)
    def yzz: ConstVec3d = new ConstVec3d(y, z, z)
    def zxx: ConstVec3d = new ConstVec3d(z, x, x)
    def zxy: ConstVec3d = new ConstVec3d(z, x, y)
    def zxz: ConstVec3d = new ConstVec3d(z, x, z)
    def zyx: ConstVec3d = new ConstVec3d(z, y, x)
    def zyy: ConstVec3d = new ConstVec3d(z, y, y)
    def zyz: ConstVec3d = new ConstVec3d(z, y, z)
    def zzx: ConstVec3d = new ConstVec3d(z, z, x)
    def zzy: ConstVec3d = new ConstVec3d(z, z, y)
    def zzz: ConstVec3d = new ConstVec3d(z, z, z)

    def xxxx: ConstVec4d = new ConstVec4d(x, x, x, x)
    def xxxy: ConstVec4d = new ConstVec4d(x, x, x, y)
    def xxxz: ConstVec4d = new ConstVec4d(x, x, x, z)
    def xxyx: ConstVec4d = new ConstVec4d(x, x, y, x)
    def xxyy: ConstVec4d = new ConstVec4d(x, x, y, y)
    def xxyz: ConstVec4d = new ConstVec4d(x, x, y, z)
    def xxzx: ConstVec4d = new ConstVec4d(x, x, z, x)
    def xxzy: ConstVec4d = new ConstVec4d(x, x, z, y)
    def xxzz: ConstVec4d = new ConstVec4d(x, x, z, z)
    def xyxx: ConstVec4d = new ConstVec4d(x, y, x, x)
    def xyxy: ConstVec4d = new ConstVec4d(x, y, x, y)
    def xyxz: ConstVec4d = new ConstVec4d(x, y, x, z)
    def xyyx: ConstVec4d = new ConstVec4d(x, y, y, x)
    def xyyy: ConstVec4d = new ConstVec4d(x, y, y, y)
    def xyyz: ConstVec4d = new ConstVec4d(x, y, y, z)
    def xyzx: ConstVec4d = new ConstVec4d(x, y, z, x)
    def xyzy: ConstVec4d = new ConstVec4d(x, y, z, y)
    def xyzz: ConstVec4d = new ConstVec4d(x, y, z, z)
    def xzxx: ConstVec4d = new ConstVec4d(x, z, x, x)
    def xzxy: ConstVec4d = new ConstVec4d(x, z, x, y)
    def xzxz: ConstVec4d = new ConstVec4d(x, z, x, z)
    def xzyx: ConstVec4d = new ConstVec4d(x, z, y, x)
    def xzyy: ConstVec4d = new ConstVec4d(x, z, y, y)
    def xzyz: ConstVec4d = new ConstVec4d(x, z, y, z)
    def xzzx: ConstVec4d = new ConstVec4d(x, z, z, x)
    def xzzy: ConstVec4d = new ConstVec4d(x, z, z, y)
    def xzzz: ConstVec4d = new ConstVec4d(x, z, z, z)
    def yxxx: ConstVec4d = new ConstVec4d(y, x, x, x)
    def yxxy: ConstVec4d = new ConstVec4d(y, x, x, y)
    def yxxz: ConstVec4d = new ConstVec4d(y, x, x, z)
    def yxyx: ConstVec4d = new ConstVec4d(y, x, y, x)
    def yxyy: ConstVec4d = new ConstVec4d(y, x, y, y)
    def yxyz: ConstVec4d = new ConstVec4d(y, x, y, z)
    def yxzx: ConstVec4d = new ConstVec4d(y, x, z, x)
    def yxzy: ConstVec4d = new ConstVec4d(y, x, z, y)
    def yxzz: ConstVec4d = new ConstVec4d(y, x, z, z)
    def yyxx: ConstVec4d = new ConstVec4d(y, y, x, x)
    def yyxy: ConstVec4d = new ConstVec4d(y, y, x, y)
    def yyxz: ConstVec4d = new ConstVec4d(y, y, x, z)
    def yyyx: ConstVec4d = new ConstVec4d(y, y, y, x)
    def yyyy: ConstVec4d = new ConstVec4d(y, y, y, y)
    def yyyz: ConstVec4d = new ConstVec4d(y, y, y, z)
    def yyzx: ConstVec4d = new ConstVec4d(y, y, z, x)
    def yyzy: ConstVec4d = new ConstVec4d(y, y, z, y)
    def yyzz: ConstVec4d = new ConstVec4d(y, y, z, z)
    def yzxx: ConstVec4d = new ConstVec4d(y, z, x, x)
    def yzxy: ConstVec4d = new ConstVec4d(y, z, x, y)
    def yzxz: ConstVec4d = new ConstVec4d(y, z, x, z)
    def yzyx: ConstVec4d = new ConstVec4d(y, z, y, x)
    def yzyy: ConstVec4d = new ConstVec4d(y, z, y, y)
    def yzyz: ConstVec4d = new ConstVec4d(y, z, y, z)
    def yzzx: ConstVec4d = new ConstVec4d(y, z, z, x)
    def yzzy: ConstVec4d = new ConstVec4d(y, z, z, y)
    def yzzz: ConstVec4d = new ConstVec4d(y, z, z, z)
    def zxxx: ConstVec4d = new ConstVec4d(z, x, x, x)
    def zxxy: ConstVec4d = new ConstVec4d(z, x, x, y)
    def zxxz: ConstVec4d = new ConstVec4d(z, x, x, z)
    def zxyx: ConstVec4d = new ConstVec4d(z, x, y, x)
    def zxyy: ConstVec4d = new ConstVec4d(z, x, y, y)
    def zxyz: ConstVec4d = new ConstVec4d(z, x, y, z)
    def zxzx: ConstVec4d = new ConstVec4d(z, x, z, x)
    def zxzy: ConstVec4d = new ConstVec4d(z, x, z, y)
    def zxzz: ConstVec4d = new ConstVec4d(z, x, z, z)
    def zyxx: ConstVec4d = new ConstVec4d(z, y, x, x)
    def zyxy: ConstVec4d = new ConstVec4d(z, y, x, y)
    def zyxz: ConstVec4d = new ConstVec4d(z, y, x, z)
    def zyyx: ConstVec4d = new ConstVec4d(z, y, y, x)
    def zyyy: ConstVec4d = new ConstVec4d(z, y, y, y)
    def zyyz: ConstVec4d = new ConstVec4d(z, y, y, z)
    def zyzx: ConstVec4d = new ConstVec4d(z, y, z, x)
    def zyzy: ConstVec4d = new ConstVec4d(z, y, z, y)
    def zyzz: ConstVec4d = new ConstVec4d(z, y, z, z)
    def zzxx: ConstVec4d = new ConstVec4d(z, z, x, x)
    def zzxy: ConstVec4d = new ConstVec4d(z, z, x, y)
    def zzxz: ConstVec4d = new ConstVec4d(z, z, x, z)
    def zzyx: ConstVec4d = new ConstVec4d(z, z, y, x)
    def zzyy: ConstVec4d = new ConstVec4d(z, z, y, y)
    def zzyz: ConstVec4d = new ConstVec4d(z, z, y, z)
    def zzzx: ConstVec4d = new ConstVec4d(z, z, z, x)
    def zzzy: ConstVec4d = new ConstVec4d(z, z, z, y)
    def zzzz: ConstVec4d = new ConstVec4d(z, z, z, z)

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

final class ConstVec3d private[math] (
    val x: Double, val y: Double, val z: Double
) extends AnyVec3d

object ConstVec3d {
    def apply(x: Double, y: Double, z: Double) = new ConstVec3d(x, y, z)
    def apply(u: AnyVec3d) = new ConstVec3d(u.x, u.y, u.z)

    implicit def mutableToConst(u: Vec3d) = new ConstVec3d(u.x, u.y, u.z)
}


final class Vec3d private[math] (
    var x: Double, var y: Double, var z: Double
) extends AnyVec3d
{
    override def r = x
    override def g = y
    override def b = z

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }
    def b_=(b: Double) { z = b }


    def *=(s: Double) { x *= s; y *= s; z *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(u: AnyVec3d) { x += u.x; y += u.y; z += u.z }
    def -=(u: AnyVec3d) { x -= u.x; y -= u.y; z -= u.z }
    def *=(u: AnyVec3d) { x *= u.x; y *= u.y; z *= u.z }
    def /=(u: AnyVec3d) { x /= u.x; y /= u.y; z /= u.z }

    def *=(m: AnyMat3d) { this := m.transposeMul(this) }

    def :=(u: AnyVec3d) { x = u.x; y = u.y; z = u.z }
    def set(x: Double, y: Double, z: Double) { this.x = x; this.y = y; this.z = z }

    def update(i: Int, s: Double) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2d = new ConstVec2d(x, y)
    override def xz: ConstVec2d = new ConstVec2d(x, z)
    override def yx: ConstVec2d = new ConstVec2d(y, x)
    override def yz: ConstVec2d = new ConstVec2d(y, z)
    override def zx: ConstVec2d = new ConstVec2d(z, x)
    override def zy: ConstVec2d = new ConstVec2d(z, y)

    override def xyz: ConstVec3d = new ConstVec3d(x, y, z)
    override def xzy: ConstVec3d = new ConstVec3d(x, z, y)
    override def yxz: ConstVec3d = new ConstVec3d(y, x, z)
    override def yzx: ConstVec3d = new ConstVec3d(y, z, x)
    override def zxy: ConstVec3d = new ConstVec3d(z, x, y)
    override def zyx: ConstVec3d = new ConstVec3d(z, y, x)

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


    def xy_=(u: AnyVec2d) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2d) { x = u.x; z = u.y }
    def yx_=(u: AnyVec2d) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2d) { y = u.x; z = u.y }
    def zx_=(u: AnyVec2d) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2d) { z = u.x; y = u.y }

    def xyz_=(u: AnyVec3d) { x = u.x; y = u.y; z = u.z }
    def xzy_=(u: AnyVec3d) { x = u.x; var t = u.z; z = u.y; y = t }
    def yxz_=(u: AnyVec3d) { var t = u.y; y = u.x; x = t; z = u.z }
    def yzx_=(u: AnyVec3d) { var t = u.y; y = u.x; x = u.z; z = t }
    def zxy_=(u: AnyVec3d) { var t = u.z; z = u.x; x = u.y; y = t }
    def zyx_=(u: AnyVec3d) { var t = u.z; z = u.x; x = t; y = u.y }

    def rg_=(u: AnyVec2d) { xy_=(u) }
    def rb_=(u: AnyVec2d) { xz_=(u) }
    def gr_=(u: AnyVec2d) { yx_=(u) }
    def gb_=(u: AnyVec2d) { yz_=(u) }
    def br_=(u: AnyVec2d) { zx_=(u) }
    def bg_=(u: AnyVec2d) { zy_=(u) }

    def rgb_=(u: AnyVec3d) { xyz_=(u) }
    def rbg_=(u: AnyVec3d) { xzy_=(u) }
    def grb_=(u: AnyVec3d) { yxz_=(u) }
    def gbr_=(u: AnyVec3d) { yzx_=(u) }
    def brg_=(u: AnyVec3d) { zxy_=(u) }
    def bgr_=(u: AnyVec3d) { zyx_=(u) }
}

object Vec3d {
    val Origin = new ConstVec3d(0, 0, 0)
    val UnitX = new ConstVec3d(1, 0, 0)
    val UnitY = new ConstVec3d(0, 1, 0)
    val UnitZ = new ConstVec3d(0, 0, 1)

    def apply(s: Double) = new Vec3d(s, s, s)
    def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)
    def apply(u: AnyVec3d) = new Vec3d(u.x, u.y, u.z)
    def apply(u: AnyVec4d) = new Vec3d(u.x, u.y, u.z)
    def apply(xy: AnyVec2d, z: Double) = new Vec3d(xy.x, xy.y, z)
    def apply(x: Double, yz: AnyVec2d) = new Vec3d(x, yz.x, yz.y)
    def apply(u: Read3Int) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read4Int) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read3Float) = new Vec3d(u.x, u.y, u.z)
    def apply(u: Read4Float) = new Vec3d(u.x, u.y, u.z)

    implicit def constToMutable(u: ConstVec3d) = Vec3d(u)
}
