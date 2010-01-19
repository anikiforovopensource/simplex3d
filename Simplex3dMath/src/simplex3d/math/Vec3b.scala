/*
 * Simplex3d, BaseMath module
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

package simplex3d.math

import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec3b extends Swizzle3Read[Boolean] {

    type R2 = ConstVec2b; type R3 = ConstVec3b; type R4 = ConstVec4b
    protected def make2(x: Boolean, y: Boolean) =
        new ConstVec2b(x, y)
    protected def make3(x: Boolean, y: Boolean, z: Boolean) =
        new ConstVec3b(x, y, z)
    protected def make4(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
        new ConstVec4b(x, y, z, w)


    def x: Boolean
    def y: Boolean
    def z: Boolean

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z


    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def ==(u: AnyVec3b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3b) :Boolean = !(this == u)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec3b => this == u
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

}

final class ConstVec3b private[math] (
    val x: Boolean, val y: Boolean, val z: Boolean)
extends AnyVec3b

object ConstVec3b {
    def apply(x: Boolean, y: Boolean, z: Boolean) = new ConstVec3b(x, y, z)
    def apply(u: AnyVec3b) = new ConstVec3b(u.x, u.y, u.z)

    implicit def mutableToConst(u: Vec3b) = new ConstVec3b(u.x, u.y, u.z)
}


final class Vec3b private[math] (var x: Boolean, var y: Boolean, var z: Boolean)
extends AnyVec3b
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }
    def b_=(b: Boolean) { z = b }

    def s_=(s: Boolean) { x = s }
    def t_=(t: Boolean) { y = t }
    def p_=(p: Boolean) { z = p }


    def :=(u: AnyVec3b) { x = u.x; y = u.y; z = u.z }
    def set(x: Boolean, y: Boolean, z: Boolean) {
        this.x = x; this.y = y; this.z = z
    }

    def update(i: Int, s: Boolean) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2b = new ConstVec2b(x, y)
    override def xz: ConstVec2b = new ConstVec2b(x, z)
    override def yx: ConstVec2b = new ConstVec2b(y, x)
    override def yz: ConstVec2b = new ConstVec2b(y, z)
    override def zx: ConstVec2b = new ConstVec2b(z, x)
    override def zy: ConstVec2b = new ConstVec2b(z, y)

    override def xyz: ConstVec3b = new ConstVec3b(x, y, z)
    override def xzy: ConstVec3b = new ConstVec3b(x, z, y)
    override def yxz: ConstVec3b = new ConstVec3b(y, x, z)
    override def yzx: ConstVec3b = new ConstVec3b(y, z, x)
    override def zxy: ConstVec3b = new ConstVec3b(z, x, y)
    override def zyx: ConstVec3b = new ConstVec3b(z, y, x)

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


    def xy_=(u: AnyVec2b) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2b) { x = u.x; z = u.y }
    def yx_=(u: AnyVec2b) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2b) { y = u.x; z = u.y }
    def zx_=(u: AnyVec2b) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2b) { z = u.x; y = u.y }

    def xyz_=(u: AnyVec3b) { x = u.x; y = u.y; z = u.z }
    def xzy_=(u: AnyVec3b) { x = u.x; var t = u.z; z = u.y; y = t }
    def yxz_=(u: AnyVec3b) { var t = u.y; y = u.x; x = t; z = u.z }
    def yzx_=(u: AnyVec3b) { var t = u.y; y = u.x; x = u.z; z = t }
    def zxy_=(u: AnyVec3b) { var t = u.z; z = u.x; x = u.y; y = t }
    def zyx_=(u: AnyVec3b) { var t = u.z; z = u.x; x = t; y = u.y }

    def rg_=(u: AnyVec2b) { xy_=(u) }
    def rb_=(u: AnyVec2b) { xz_=(u) }
    def gr_=(u: AnyVec2b) { yx_=(u) }
    def gb_=(u: AnyVec2b) { yz_=(u) }
    def br_=(u: AnyVec2b) { zx_=(u) }
    def bg_=(u: AnyVec2b) { zy_=(u) }

    def rgb_=(u: AnyVec3b) { xyz_=(u) }
    def rbg_=(u: AnyVec3b) { xzy_=(u) }
    def grb_=(u: AnyVec3b) { yxz_=(u) }
    def gbr_=(u: AnyVec3b) { yzx_=(u) }
    def brg_=(u: AnyVec3b) { zxy_=(u) }
    def bgr_=(u: AnyVec3b) { zyx_=(u) }

    def st_=(u: AnyVec2b) { xy_=(u) }
    def sp_=(u: AnyVec2b) { xz_=(u) }
    def ts_=(u: AnyVec2b) { yx_=(u) }
    def tp_=(u: AnyVec2b) { yz_=(u) }
    def ps_=(u: AnyVec2b) { zx_=(u) }
    def pt_=(u: AnyVec2b) { zy_=(u) }

    def stp_=(u: AnyVec3b) { xyz_=(u) }
    def spt_=(u: AnyVec3b) { xzy_=(u) }
    def tsp_=(u: AnyVec3b) { yxz_=(u) }
    def tps_=(u: AnyVec3b) { yzx_=(u) }
    def pst_=(u: AnyVec3b) { zxy_=(u) }
    def pts_=(u: AnyVec3b) { zyx_=(u) }
}

object Vec3b {
    val True = new ConstVec3b(true, true, true)
    val False = new ConstVec3b(false, false, false)

    def apply(s: Boolean) = new Vec3b(s, s, s)
    def apply(x: Boolean, y: Boolean, z: Boolean) = new Vec3b(x, y, z)
    def apply(u: AnyVec3b) = new Vec3b(u.x, u.y, u.z)
    def apply(u: AnyVec4b) = new Vec3b(u.x, u.y, u.z)
    def apply(xy: AnyVec2b, z: Boolean) = new Vec3b(xy.x, xy.y, z)
    def apply(x: Boolean, yz: AnyVec2b) = new Vec3b(x, yz.x, yz.y)

    def apply(u: Read3Int) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))
    def apply(u: Read4Int) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))

    def apply(u: Read3Float) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))
    def apply(u: Read4Float) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))

    def apply(u: Read3Double) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))
    def apply(u: Read4Double) = new Vec3b(bool(u.x), bool(u.y), bool(u.z))

    implicit def constToMutable(u: ConstVec3b) = Vec3b(u)
}
