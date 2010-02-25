/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3d extends Read3[Double] {

    private[math] type R2 = ConstVec2d
    private[math] type R3 = ConstVec3d
    private[math] type R4 = ConstVec4d

    protected def make2(x: Double, y: Double) =
        new ConstVec2d(x, y)
    protected def make3(x: Double, y: Double, z: Double) =
        new ConstVec3d(x, y, z)
    protected def make4(x: Double, y: Double, z: Double, w: Double) =
        new ConstVec4d(x, y, z, w)

    private[math] def bx: Boolean = bool(x)
    private[math] def by: Boolean = bool(y)
    private[math] def bz: Boolean = bool(z)

    private[math] def ix: Int = int(x)
    private[math] def iy: Int = int(y)
    private[math] def iz: Int = int(z)

    private[math] def fx: Float = float(x)
    private[math] def fy: Float = float(y)
    private[math] def fz: Float = float(z)

    private[math] def dx: Double = x
    private[math] def dy: Double = y
    private[math] def dz: Double = z


    def x: Double
    def y: Double
    def z: Double

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z

    
    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def unary_+() :this.type = this
    def unary_-() = new Vec3d(-x, -y, -z)
    def *(s: Double) = new Vec3d(x * s, y * s, z * s)
    def /(s: Double) = { val inv = 1/s; new Vec3d(x * inv, y * inv, z * inv) }

    def +(s: Double) = new Vec3d(x + s, y + s, z + s)
    def -(s: Double) = new Vec3d(x - s, y - s, z - s)

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

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec3d => this == u
            case _ => false
        }
    }

    override def hashCode() :Int = {
        41 * (
            41 * (
                41 + x.hashCode
            ) + y.hashCode
        ) + z.hashCode
    }

    override def toString() :String = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }
}

final class ConstVec3d private[math] (
    val x: Double, val y: Double, val z: Double
) extends AnyVec3d

object ConstVec3d {
    def apply(x: Double, y: Double, z: Double) = new ConstVec3d(x, y, z)
    def apply(u: Read3[_]) = new ConstVec3d(u.dx, u.dy, u.dz)

    implicit def toConst(u: AnyVec3d) = new ConstVec3d(u.x, u.y, u.z)
}


final class Vec3d private[math] (
    var x: Double, var y: Double, var z: Double
) extends AnyVec3d
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }
    def b_=(b: Double) { z = b }

    def s_=(s: Double) { x = s }
    def t_=(t: Double) { y = t }
    def p_=(p: Double) { z = p }


    def *=(s: Double) { x *= s; y *= s; z *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv }

    def +=(s: Double) { x += s; y += s; z += s }
    def -=(s: Double) { x -= s; y -= s; z -= s }

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

    override def st = xy
    override def sp = xz
    override def ts = yx
    override def tp = yz
    override def ps = zx
    override def pt = zy

    override def stp = xyz
    override def spt = xzy
    override def tsp = yxz
    override def tps = yzx
    override def pst = zxy
    override def pts = zyx


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

    def st_=(u: AnyVec2d) { xy_=(u) }
    def sp_=(u: AnyVec2d) { xz_=(u) }
    def ts_=(u: AnyVec2d) { yx_=(u) }
    def tp_=(u: AnyVec2d) { yz_=(u) }
    def ps_=(u: AnyVec2d) { zx_=(u) }
    def pt_=(u: AnyVec2d) { zy_=(u) }

    def stp_=(u: AnyVec3d) { xyz_=(u) }
    def spt_=(u: AnyVec3d) { xzy_=(u) }
    def tsp_=(u: AnyVec3d) { yxz_=(u) }
    def tps_=(u: AnyVec3d) { yzx_=(u) }
    def pst_=(u: AnyVec3d) { zxy_=(u) }
    def pts_=(u: AnyVec3d) { zyx_=(u) }
}

object Vec3d {
    val Zero = new ConstVec3d(0, 0, 0)
    val UnitX = new ConstVec3d(1, 0, 0)
    val UnitY = new ConstVec3d(0, 1, 0)
    val UnitZ = new ConstVec3d(0, 0, 1)
    val One = new ConstVec3d(1, 1, 1)

    def apply(s: Double) = new Vec3d(s, s, s)
    def apply(x: Double, y: Double, z: Double) = new Vec3d(x, y, z)
    def apply(u: Read3[_]) = new Vec3d(u.dx, u.dy, u.dz)
    def apply(u: Read4[_]) = new Vec3d(u.dx, u.dy, u.dz)
    def apply(xy: Read2[_], z: Double) = new Vec3d(xy.dx, xy.dy, z)
    def apply(x: Double, yz: Read2[_]) = new Vec3d(x, yz.dx, yz.dy)

    def unapply(u: AnyVec3d) = Some((u.x, u.y, u.z))

    implicit def toMutable(u: AnyVec3d) = new Vec3d(u.x, u.y, u.z)
}
