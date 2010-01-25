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
sealed abstract class AnyVec4b extends Read4 {

    private[math] type T = Boolean
    private[math] type R2 = ConstVec2b
    private[math] type R3 = ConstVec3b
    private[math] type R4 = ConstVec4b
    
    protected def make2(x: Boolean, y: Boolean) =
        new ConstVec2b(x, y)
    protected def make3(x: Boolean, y: Boolean, z: Boolean) =
        new ConstVec3b(x, y, z)
    protected def make4(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
        new ConstVec4b(x, y, z, w)

    private[math] def bx: Boolean = x
    private[math] def by: Boolean = y
    private[math] def bz: Boolean = z
    private[math] def bw: Boolean = w

    private[math] def ix: Int = int(x)
    private[math] def iy: Int = int(y)
    private[math] def iz: Int = int(z)
    private[math] def iw: Int = int(w)

    private[math] def fx: Float = float(x)
    private[math] def fy: Float = float(y)
    private[math] def fz: Float = float(z)
    private[math] def fw: Float = float(w)

    private[math] def dx: Double = double(x)
    private[math] def dy: Double = double(y)
    private[math] def dz: Double = double(z)
    private[math] def dw: Double = double(w)


    def x: Boolean
    def y: Boolean
    def z: Boolean
    def w: Boolean

    def r = x
    def g = y
    def b = z
    def a = w

    def s = x
    def t = y
    def p = z
    def q = w


    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def ==(u: AnyVec4b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4b) :Boolean = !(this == u)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec4b => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        41 * (
            41 * (
                41 * (
                    41 + x.hashCode
                ) + y.hashCode
            ) + z.hashCode
        ) + w.hashCode
    }
    
    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }
}

final class ConstVec4b private[math] (
    val x: Boolean, val y: Boolean, val z: Boolean, val w: Boolean)
extends AnyVec4b

object ConstVec4b {
    def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) = {
        new ConstVec4b(x, y, z, w)
    }
    def apply(u: Read4) = new ConstVec4b(u.bx, u.by, u.bz, u.bw)

    implicit def toConst(u: Vec4b) = new ConstVec4b(u.x, u.y, u.z, u.w)
}


final class Vec4b private[math] (
    var x: Boolean, var y: Boolean, var z: Boolean, var w: Boolean)
extends AnyVec4b
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    override def s = x
    override def t = y
    override def p = z
    override def q = w

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }
    def b_=(b: Boolean) { z = b }
    def a_=(a: Boolean) { w = a }

    def s_=(s: Boolean) { x = s }
    def t_=(t: Boolean) { y = t }
    def p_=(p: Boolean) { z = p }
    def q_=(q: Boolean) { w = q }


    def :=(u: AnyVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Boolean) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case 3 => w = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2b = new ConstVec2b(x, y)
    override def xz: ConstVec2b = new ConstVec2b(x, z)
    override def xw: ConstVec2b = new ConstVec2b(x, w)
    override def yx: ConstVec2b = new ConstVec2b(y, x)
    override def yz: ConstVec2b = new ConstVec2b(y, z)
    override def yw: ConstVec2b = new ConstVec2b(y, w)
    override def zx: ConstVec2b = new ConstVec2b(z, x)
    override def zy: ConstVec2b = new ConstVec2b(z, y)
    override def zw: ConstVec2b = new ConstVec2b(z, w)
    override def wx: ConstVec2b = new ConstVec2b(w, x)
    override def wy: ConstVec2b = new ConstVec2b(w, y)
    override def wz: ConstVec2b = new ConstVec2b(w, z)

    override def xyz: ConstVec3b = new ConstVec3b(x, y, z)
    override def xyw: ConstVec3b = new ConstVec3b(x, y, w)
    override def xzy: ConstVec3b = new ConstVec3b(x, z, y)
    override def xzw: ConstVec3b = new ConstVec3b(x, z, w)
    override def xwy: ConstVec3b = new ConstVec3b(x, w, y)
    override def xwz: ConstVec3b = new ConstVec3b(x, w, z)
    override def yxz: ConstVec3b = new ConstVec3b(y, x, z)
    override def yxw: ConstVec3b = new ConstVec3b(y, x, w)
    override def yzx: ConstVec3b = new ConstVec3b(y, z, x)
    override def yzw: ConstVec3b = new ConstVec3b(y, z, w)
    override def ywx: ConstVec3b = new ConstVec3b(y, w, x)
    override def ywz: ConstVec3b = new ConstVec3b(y, w, z)
    override def zxy: ConstVec3b = new ConstVec3b(z, x, y)
    override def zxw: ConstVec3b = new ConstVec3b(z, x, w)
    override def zyx: ConstVec3b = new ConstVec3b(z, y, x)
    override def zyw: ConstVec3b = new ConstVec3b(z, y, w)
    override def zwx: ConstVec3b = new ConstVec3b(z, w, x)
    override def zwy: ConstVec3b = new ConstVec3b(z, w, y)
    override def wxy: ConstVec3b = new ConstVec3b(w, x, y)
    override def wxz: ConstVec3b = new ConstVec3b(w, x, z)
    override def wyx: ConstVec3b = new ConstVec3b(w, y, x)
    override def wyz: ConstVec3b = new ConstVec3b(w, y, z)
    override def wzx: ConstVec3b = new ConstVec3b(w, z, x)
    override def wzy: ConstVec3b = new ConstVec3b(w, z, y)

    override def xyzw: ConstVec4b = new ConstVec4b(x, y, z, w)
    override def xywz: ConstVec4b = new ConstVec4b(x, y, w, z)
    override def xzyw: ConstVec4b = new ConstVec4b(x, z, y, w)
    override def xzwy: ConstVec4b = new ConstVec4b(x, z, w, y)
    override def xwyz: ConstVec4b = new ConstVec4b(x, w, y, z)
    override def xwzy: ConstVec4b = new ConstVec4b(x, w, z, y)
    override def yxzw: ConstVec4b = new ConstVec4b(y, x, z, w)
    override def yxwz: ConstVec4b = new ConstVec4b(y, x, w, z)
    override def yzxw: ConstVec4b = new ConstVec4b(y, z, x, w)
    override def yzwx: ConstVec4b = new ConstVec4b(y, z, w, x)
    override def ywxz: ConstVec4b = new ConstVec4b(y, w, x, z)
    override def ywzx: ConstVec4b = new ConstVec4b(y, w, z, x)
    override def zxyw: ConstVec4b = new ConstVec4b(z, x, y, w)
    override def zxwy: ConstVec4b = new ConstVec4b(z, x, w, y)
    override def zyxw: ConstVec4b = new ConstVec4b(z, y, x, w)
    override def zywx: ConstVec4b = new ConstVec4b(z, y, w, x)
    override def zwxy: ConstVec4b = new ConstVec4b(z, w, x, y)
    override def zwyx: ConstVec4b = new ConstVec4b(z, w, y, x)
    override def wxyz: ConstVec4b = new ConstVec4b(w, x, y, z)
    override def wxzy: ConstVec4b = new ConstVec4b(w, x, z, y)
    override def wyxz: ConstVec4b = new ConstVec4b(w, y, x, z)
    override def wyzx: ConstVec4b = new ConstVec4b(w, y, z, x)
    override def wzxy: ConstVec4b = new ConstVec4b(w, z, x, y)
    override def wzyx: ConstVec4b = new ConstVec4b(w, z, y, x)

    override def rg = xy
    override def rb = xz
    override def ra = xw
    override def gr = yx
    override def gb = yz
    override def ga = yw
    override def br = zx
    override def bg = zy
    override def ba = zw
    override def ar = wx
    override def ag = wy
    override def ab = wz

    override def rgb = xyz
    override def rga = xyw
    override def rbg = xzy
    override def rba = xzw
    override def rag = xwy
    override def rab = xwz
    override def grb = yxz
    override def gra = yxw
    override def gbr = yzx
    override def gba = yzw
    override def gar = ywx
    override def gab = ywz
    override def brg = zxy
    override def bra = zxw
    override def bgr = zyx
    override def bga = zyw
    override def bar = zwx
    override def bag = zwy
    override def arg = wxy
    override def arb = wxz
    override def agr = wyx
    override def agb = wyz
    override def abr = wzx
    override def abg = wzy

    override def rgba = xyzw
    override def rgab = xywz
    override def rbga = xzyw
    override def rbag = xzwy
    override def ragb = xwyz
    override def rabg = xwzy
    override def grba = yxzw
    override def grab = yxwz
    override def gbra = yzxw
    override def gbar = yzwx
    override def garb = ywxz
    override def gabr = ywzx
    override def brga = zxyw
    override def brag = zxwy
    override def bgra = zyxw
    override def bgar = zywx
    override def barg = zwxy
    override def bagr = zwyx
    override def argb = wxyz
    override def arbg = wxzy
    override def agrb = wyxz
    override def agbr = wyzx
    override def abrg = wzxy
    override def abgr = wzyx

    override def st = xy
    override def sp = xz
    override def sq = xw
    override def ts = yx
    override def tp = yz
    override def tq = yw
    override def ps = zx
    override def pt = zy
    override def pq = zw
    override def qs = wx
    override def qt = wy
    override def qp = wz

    override def stp = xyz
    override def stq = xyw
    override def spt = xzy
    override def spq = xzw
    override def sqt = xwy
    override def sqp = xwz
    override def tsp = yxz
    override def tsq = yxw
    override def tps = yzx
    override def tpq = yzw
    override def tqs = ywx
    override def tqp = ywz
    override def pst = zxy
    override def psq = zxw
    override def pts = zyx
    override def ptq = zyw
    override def pqs = zwx
    override def pqt = zwy
    override def qst = wxy
    override def qsp = wxz
    override def qts = wyx
    override def qtp = wyz
    override def qps = wzx
    override def qpt = wzy

    override def stpq = xyzw
    override def stqp = xywz
    override def sptq = xzyw
    override def spqt = xzwy
    override def sqtp = xwyz
    override def sqpt = xwzy
    override def tspq = yxzw
    override def tsqp = yxwz
    override def tpsq = yzxw
    override def tpqs = yzwx
    override def tqsp = ywxz
    override def tqps = ywzx
    override def pstq = zxyw
    override def psqt = zxwy
    override def ptsq = zyxw
    override def ptqs = zywx
    override def pqst = zwxy
    override def pqts = zwyx
    override def qstp = wxyz
    override def qspt = wxzy
    override def qtsp = wyxz
    override def qtps = wyzx
    override def qpst = wzxy
    override def qpts = wzyx


    def xy_=(u: AnyVec2b) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2b) { x = u.x; z = u.y }
    def xw_=(u: AnyVec2b) { x = u.x; w = u.y }
    def yx_=(u: AnyVec2b) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2b) { y = u.x; z = u.y }
    def yw_=(u: AnyVec2b) { y = u.x; w = u.y }
    def zx_=(u: AnyVec2b) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2b) { z = u.x; y = u.y }
    def zw_=(u: AnyVec2b) { z = u.x; w = u.y }
    def wx_=(u: AnyVec2b) { w = u.x; x = u.y }
    def wy_=(u: AnyVec2b) { w = u.x; y = u.y }
    def wz_=(u: AnyVec2b) { w = u.x; z = u.y }

    def xyz_=(u: AnyVec3b) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: AnyVec3b) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: AnyVec3b) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: AnyVec3b) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: AnyVec3b) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: AnyVec3b) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: AnyVec3b) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: AnyVec3b) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: AnyVec3b) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: AnyVec3b) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: AnyVec3b) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: AnyVec3b) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: AnyVec3b) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: AnyVec3b) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: AnyVec3b) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: AnyVec3b) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: AnyVec3b) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: AnyVec3b) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: AnyVec3b) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: AnyVec3b) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: AnyVec3b) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: AnyVec3b) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: AnyVec3b) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: AnyVec3b) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: AnyVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: AnyVec4b) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: AnyVec4b) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: AnyVec4b) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: AnyVec4b) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: AnyVec4b) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: AnyVec4b) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: AnyVec4b) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z;z=t}
    def yzxw_=(u: AnyVec4b) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: AnyVec4b) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: AnyVec4b) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: AnyVec4b) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: AnyVec4b) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: AnyVec4b) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: AnyVec4b) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: AnyVec4b) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: AnyVec4b) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y;y=t}
    def zwyx_=(u: AnyVec4b) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: AnyVec4b) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: AnyVec4b) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: AnyVec4b) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: AnyVec4b) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: AnyVec4b) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: AnyVec4b) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y;y=t}

    def rg_=(u: AnyVec2b) { xy_=(u) }
    def rb_=(u: AnyVec2b) { xz_=(u) }
    def ra_=(u: AnyVec2b) { xw_=(u) }
    def gr_=(u: AnyVec2b) { yx_=(u) }
    def gb_=(u: AnyVec2b) { yz_=(u) }
    def ga_=(u: AnyVec2b) { yw_=(u) }
    def br_=(u: AnyVec2b) { zx_=(u) }
    def bg_=(u: AnyVec2b) { zy_=(u) }
    def ba_=(u: AnyVec2b) { zw_=(u) }
    def ar_=(u: AnyVec2b) { wx_=(u) }
    def ag_=(u: AnyVec2b) { wy_=(u) }
    def ab_=(u: AnyVec2b) { wz_=(u) }

    def rgb_=(u: AnyVec3b) { xyz_=(u) }
    def rga_=(u: AnyVec3b) { xyw_=(u) }
    def rbg_=(u: AnyVec3b) { xzy_=(u) }
    def rba_=(u: AnyVec3b) { xzw_=(u) }
    def rag_=(u: AnyVec3b) { xwy_=(u) }
    def rab_=(u: AnyVec3b) { xwz_=(u) }
    def grb_=(u: AnyVec3b) { yxz_=(u) }
    def gra_=(u: AnyVec3b) { yxw_=(u) }
    def gbr_=(u: AnyVec3b) { yzx_=(u) }
    def gba_=(u: AnyVec3b) { yzw_=(u) }
    def gar_=(u: AnyVec3b) { ywx_=(u) }
    def gab_=(u: AnyVec3b) { ywz_=(u) }
    def brg_=(u: AnyVec3b) { zxy_=(u) }
    def bra_=(u: AnyVec3b) { zxw_=(u) }
    def bgr_=(u: AnyVec3b) { zyx_=(u) }
    def bga_=(u: AnyVec3b) { zyw_=(u) }
    def bar_=(u: AnyVec3b) { zwx_=(u) }
    def bag_=(u: AnyVec3b) { zwy_=(u) }
    def arg_=(u: AnyVec3b) { wxy_=(u) }
    def arb_=(u: AnyVec3b) { wxz_=(u) }
    def agr_=(u: AnyVec3b) { wyx_=(u) }
    def agb_=(u: AnyVec3b) { wyz_=(u) }
    def abr_=(u: AnyVec3b) { wzx_=(u) }
    def abg_=(u: AnyVec3b) { wzy_=(u) }

    def rgba_=(u: AnyVec4b) { xyzw_=(u) }
    def rgab_=(u: AnyVec4b) { xywz_=(u) }
    def rbga_=(u: AnyVec4b) { xzyw_=(u) }
    def rbag_=(u: AnyVec4b) { xzwy_=(u) }
    def ragb_=(u: AnyVec4b) { xwyz_=(u) }
    def rabg_=(u: AnyVec4b) { xwzy_=(u) }
    def grba_=(u: AnyVec4b) { yxzw_=(u) }
    def grab_=(u: AnyVec4b) { yxwz_=(u) }
    def gbra_=(u: AnyVec4b) { yzxw_=(u) }
    def gbar_=(u: AnyVec4b) { yzwx_=(u) }
    def garb_=(u: AnyVec4b) { ywxz_=(u) }
    def gabr_=(u: AnyVec4b) { ywzx_=(u) }
    def brga_=(u: AnyVec4b) { zxyw_=(u) }
    def brag_=(u: AnyVec4b) { zxwy_=(u) }
    def bgra_=(u: AnyVec4b) { zyxw_=(u) }
    def bgar_=(u: AnyVec4b) { zywx_=(u) }
    def barg_=(u: AnyVec4b) { zwxy_=(u) }
    def bagr_=(u: AnyVec4b) { zwyx_=(u) }
    def argb_=(u: AnyVec4b) { wxyz_=(u) }
    def arbg_=(u: AnyVec4b) { wxzy_=(u) }
    def agrb_=(u: AnyVec4b) { wyxz_=(u) }
    def agbr_=(u: AnyVec4b) { wyzx_=(u) }
    def abrg_=(u: AnyVec4b) { wzxy_=(u) }
    def abgr_=(u: AnyVec4b) { wzyx_=(u) }

    def st_=(u: AnyVec2b) { xy_=(u) }
    def sp_=(u: AnyVec2b) { xz_=(u) }
    def sq_=(u: AnyVec2b) { xw_=(u) }
    def ts_=(u: AnyVec2b) { yx_=(u) }
    def tp_=(u: AnyVec2b) { yz_=(u) }
    def tq_=(u: AnyVec2b) { yw_=(u) }
    def ps_=(u: AnyVec2b) { zx_=(u) }
    def pt_=(u: AnyVec2b) { zy_=(u) }
    def pq_=(u: AnyVec2b) { zw_=(u) }
    def qs_=(u: AnyVec2b) { wx_=(u) }
    def qt_=(u: AnyVec2b) { wy_=(u) }
    def qp_=(u: AnyVec2b) { wz_=(u) }

    def stp_=(u: AnyVec3b) { xyz_=(u) }
    def stq_=(u: AnyVec3b) { xyw_=(u) }
    def spt_=(u: AnyVec3b) { xzy_=(u) }
    def spq_=(u: AnyVec3b) { xzw_=(u) }
    def sqt_=(u: AnyVec3b) { xwy_=(u) }
    def sqp_=(u: AnyVec3b) { xwz_=(u) }
    def tsp_=(u: AnyVec3b) { yxz_=(u) }
    def tsq_=(u: AnyVec3b) { yxw_=(u) }
    def tps_=(u: AnyVec3b) { yzx_=(u) }
    def tpq_=(u: AnyVec3b) { yzw_=(u) }
    def tqs_=(u: AnyVec3b) { ywx_=(u) }
    def tqp_=(u: AnyVec3b) { ywz_=(u) }
    def pst_=(u: AnyVec3b) { zxy_=(u) }
    def psq_=(u: AnyVec3b) { zxw_=(u) }
    def pts_=(u: AnyVec3b) { zyx_=(u) }
    def ptq_=(u: AnyVec3b) { zyw_=(u) }
    def pqs_=(u: AnyVec3b) { zwx_=(u) }
    def pqt_=(u: AnyVec3b) { zwy_=(u) }
    def qst_=(u: AnyVec3b) { wxy_=(u) }
    def qsp_=(u: AnyVec3b) { wxz_=(u) }
    def qts_=(u: AnyVec3b) { wyx_=(u) }
    def qtp_=(u: AnyVec3b) { wyz_=(u) }
    def qps_=(u: AnyVec3b) { wzx_=(u) }
    def qpt_=(u: AnyVec3b) { wzy_=(u) }

    def stpq_=(u: AnyVec4b) { xyzw_=(u) }
    def stqp_=(u: AnyVec4b) { xywz_=(u) }
    def sptq_=(u: AnyVec4b) { xzyw_=(u) }
    def spqt_=(u: AnyVec4b) { xzwy_=(u) }
    def sqtp_=(u: AnyVec4b) { xwyz_=(u) }
    def sqpt_=(u: AnyVec4b) { xwzy_=(u) }
    def tspq_=(u: AnyVec4b) { yxzw_=(u) }
    def tsqp_=(u: AnyVec4b) { yxwz_=(u) }
    def tpsq_=(u: AnyVec4b) { yzxw_=(u) }
    def tpqs_=(u: AnyVec4b) { yzwx_=(u) }
    def tqsp_=(u: AnyVec4b) { ywxz_=(u) }
    def tqps_=(u: AnyVec4b) { ywzx_=(u) }
    def pstq_=(u: AnyVec4b) { zxyw_=(u) }
    def psqt_=(u: AnyVec4b) { zxwy_=(u) }
    def ptsq_=(u: AnyVec4b) { zyxw_=(u) }
    def ptqs_=(u: AnyVec4b) { zywx_=(u) }
    def pqst_=(u: AnyVec4b) { zwxy_=(u) }
    def pqts_=(u: AnyVec4b) { zwyx_=(u) }
    def qstp_=(u: AnyVec4b) { wxyz_=(u) }
    def qspt_=(u: AnyVec4b) { wxzy_=(u) }
    def qtsp_=(u: AnyVec4b) { wyxz_=(u) }
    def qtps_=(u: AnyVec4b) { wyzx_=(u) }
    def qpst_=(u: AnyVec4b) { wzxy_=(u) }
    def qpts_=(u: AnyVec4b) { wzyx_=(u) }
}

object Vec4b {
    val True = new ConstVec4b(true, true, true, true)
    val False = new ConstVec4b(false, false, false, false)

    def apply(s: Boolean) =
        new Vec4b(s, s, s, s)

    def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
        new Vec4b(x, y, z, w)

    def apply(u: Read4) =
        new Vec4b(u.bx, u.by, u.bz, u.bw)

    def apply(xy: Read2, z: Boolean, w: Boolean) =
        new Vec4b(xy.bx, xy.by, z, w)

    def apply(x: Boolean, yz: Read2, w: Boolean) =
        new Vec4b(x, yz.bx, yz.by, w)

    def apply(x: Boolean, y: Boolean, zw: Read2) =
        new Vec4b(x, y, zw.bx, zw.by)

    def apply(xy: Read2, zw: Read2) =
        new Vec4b(xy.bx, xy.by, zw.bx, zw.by)

    def apply(xyz: Read3, w: Boolean) =
        new Vec4b(xyz.bx, xyz.by, xyz.bz, w)

    def apply(x: Boolean, yzw: Read3) =
        new Vec4b(x, yzw.bx, yzw.by, yzw.bz)

    def apply(m: Read2x2) =
        new Vec4b(bool(m.f00), bool(m.f10), bool(m.f01), bool(m.f11))

    implicit def toMutable(u: ConstVec4b) = Vec4b(u)
}
