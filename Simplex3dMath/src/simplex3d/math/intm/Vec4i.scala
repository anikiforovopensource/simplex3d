/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4i extends Read4Int {

    private[math] type R2 = ConstVec2i
    private[math] type R3 = ConstVec3i
    private[math] type R4 = ConstVec4i
    
    protected def make2(x: Int, y: Int) =
        new ConstVec2i(x, y)
    protected def make3(x: Int, y: Int, z: Int) =
        new ConstVec3i(x, y, z)
    protected def make4(x: Int, y: Int, z: Int, w: Int) =
        new ConstVec4i(x, y, z, w)


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

    def unary_-() = new Vec4i(-x, -y, -z, -w)
    def unary_~() = new Vec4i(~x, ~y, ~z, ~w)

    def *(s: Int) = new Vec4i(x * s, y * s, z * s, w * s)
    def /(s: Int) = new Vec4i(x / s, y / s, z / s, w / s)
    private[math] def divideByComponent(s: Int) = {
        new Vec4i(s / x, s / y, s / z, s / w)
    }
    def %(s: Int) = new Vec4i(x % s, y % s, z % s, w % s)
    private[math] def modByComponent(s: Int) = new Vec4i(s%x, s%y, s%z, s%w)
    def >>(s: Int) = new Vec4i( x >> s, y >> s, z >> s, w >> s)
    def >>>(s: Int) = new Vec4i( x >>> s, y >>> s, z >>> s, w >>> s)
    def <<(s: Int) = new Vec4i( x << s, y << s, z << s, w << s)
    def &(s: Int) = new Vec4i( x & s, y & s, z & s, w & s)
    def |(s: Int) = new Vec4i( x | s, y | s, z | s, w | s)
    def ^(s: Int) = new Vec4i( x ^ s, y ^ s, z ^ s, w ^ s)

    def +(u: AnyVec4i) = new Vec4i(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4i) = new Vec4i(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4i) = new Vec4i(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4i) = new Vec4i(x / u.x, y / u.y, z / u.z, w / u.w)
    def %(u: AnyVec4i) = new Vec4i(x % u.x, y % u.y, z % u.z, w % u.w)
    def >>(u: AnyVec4i) = new Vec4i( x >> u.x, y >> u.y, z >> u.z, w >> u.w)
    def >>>(u: AnyVec4i) = new Vec4i( x >>> u.x, y >>> u.y, z >>> u.z, w >>> u.w)
    def <<(u: AnyVec4i) = new Vec4i( x << u.x, y << u.y, z << u.z, w << u.w)
    def &(u: AnyVec4i) = new Vec4i( x & u.x, y & u.y, z & u.z, w & u.w)
    def |(u: AnyVec4i) = new Vec4i( x | u.x, y | u.y, z | u.z, w | u.w)
    def ^(u: AnyVec4i) = new Vec4i( x ^ u.x, y ^ u.y, z ^ u.z, w ^ u.w)

    def ==(u: AnyVec4i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4i) :Boolean = !(this == u)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec4i => this == u
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

final class ConstVec4i private[math] (
    val x: Int, val y: Int, val z: Int, val w: Int)
extends AnyVec4i

object ConstVec4i {
    def apply(x: Int, y: Int, z: Int, w: Int) = {
        new ConstVec4i(x, y, z, w)
    }
    def apply(u: AnyVec4i) = new ConstVec4i(u.x, u.y, u.z, u.w)

    implicit def toConst(u: Vec4i) = new ConstVec4i(u.x, u.y, u.z, u.w)
}


final class Vec4i private[math] (
    var x: Int, var y: Int, var z: Int, var w: Int)
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
    def /=(s: Int) { x /= s; y /= s; z /= s; w /= s }
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

    // Swizzling
    override def xy: ConstVec2i = new ConstVec2i(x, y)
    override def xz: ConstVec2i = new ConstVec2i(x, z)
    override def xw: ConstVec2i = new ConstVec2i(x, w)
    override def yx: ConstVec2i = new ConstVec2i(y, x)
    override def yz: ConstVec2i = new ConstVec2i(y, z)
    override def yw: ConstVec2i = new ConstVec2i(y, w)
    override def zx: ConstVec2i = new ConstVec2i(z, x)
    override def zy: ConstVec2i = new ConstVec2i(z, y)
    override def zw: ConstVec2i = new ConstVec2i(z, w)
    override def wx: ConstVec2i = new ConstVec2i(w, x)
    override def wy: ConstVec2i = new ConstVec2i(w, y)
    override def wz: ConstVec2i = new ConstVec2i(w, z)

    override def xyz: ConstVec3i = new ConstVec3i(x, y, z)
    override def xyw: ConstVec3i = new ConstVec3i(x, y, w)
    override def xzy: ConstVec3i = new ConstVec3i(x, z, y)
    override def xzw: ConstVec3i = new ConstVec3i(x, z, w)
    override def xwy: ConstVec3i = new ConstVec3i(x, w, y)
    override def xwz: ConstVec3i = new ConstVec3i(x, w, z)
    override def yxz: ConstVec3i = new ConstVec3i(y, x, z)
    override def yxw: ConstVec3i = new ConstVec3i(y, x, w)
    override def yzx: ConstVec3i = new ConstVec3i(y, z, x)
    override def yzw: ConstVec3i = new ConstVec3i(y, z, w)
    override def ywx: ConstVec3i = new ConstVec3i(y, w, x)
    override def ywz: ConstVec3i = new ConstVec3i(y, w, z)
    override def zxy: ConstVec3i = new ConstVec3i(z, x, y)
    override def zxw: ConstVec3i = new ConstVec3i(z, x, w)
    override def zyx: ConstVec3i = new ConstVec3i(z, y, x)
    override def zyw: ConstVec3i = new ConstVec3i(z, y, w)
    override def zwx: ConstVec3i = new ConstVec3i(z, w, x)
    override def zwy: ConstVec3i = new ConstVec3i(z, w, y)
    override def wxy: ConstVec3i = new ConstVec3i(w, x, y)
    override def wxz: ConstVec3i = new ConstVec3i(w, x, z)
    override def wyx: ConstVec3i = new ConstVec3i(w, y, x)
    override def wyz: ConstVec3i = new ConstVec3i(w, y, z)
    override def wzx: ConstVec3i = new ConstVec3i(w, z, x)
    override def wzy: ConstVec3i = new ConstVec3i(w, z, y)

    override def xyzw: ConstVec4i = new ConstVec4i(x, y, z, w)
    override def xywz: ConstVec4i = new ConstVec4i(x, y, w, z)
    override def xzyw: ConstVec4i = new ConstVec4i(x, z, y, w)
    override def xzwy: ConstVec4i = new ConstVec4i(x, z, w, y)
    override def xwyz: ConstVec4i = new ConstVec4i(x, w, y, z)
    override def xwzy: ConstVec4i = new ConstVec4i(x, w, z, y)
    override def yxzw: ConstVec4i = new ConstVec4i(y, x, z, w)
    override def yxwz: ConstVec4i = new ConstVec4i(y, x, w, z)
    override def yzxw: ConstVec4i = new ConstVec4i(y, z, x, w)
    override def yzwx: ConstVec4i = new ConstVec4i(y, z, w, x)
    override def ywxz: ConstVec4i = new ConstVec4i(y, w, x, z)
    override def ywzx: ConstVec4i = new ConstVec4i(y, w, z, x)
    override def zxyw: ConstVec4i = new ConstVec4i(z, x, y, w)
    override def zxwy: ConstVec4i = new ConstVec4i(z, x, w, y)
    override def zyxw: ConstVec4i = new ConstVec4i(z, y, x, w)
    override def zywx: ConstVec4i = new ConstVec4i(z, y, w, x)
    override def zwxy: ConstVec4i = new ConstVec4i(z, w, x, y)
    override def zwyx: ConstVec4i = new ConstVec4i(z, w, y, x)
    override def wxyz: ConstVec4i = new ConstVec4i(w, x, y, z)
    override def wxzy: ConstVec4i = new ConstVec4i(w, x, z, y)
    override def wyxz: ConstVec4i = new ConstVec4i(w, y, x, z)
    override def wyzx: ConstVec4i = new ConstVec4i(w, y, z, x)
    override def wzxy: ConstVec4i = new ConstVec4i(w, z, x, y)
    override def wzyx: ConstVec4i = new ConstVec4i(w, z, y, x)

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


    def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2i) { x = u.x; z = u.y }
    def xw_=(u: AnyVec2i) { x = u.x; w = u.y }
    def yx_=(u: AnyVec2i) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2i) { y = u.x; z = u.y }
    def yw_=(u: AnyVec2i) { y = u.x; w = u.y }
    def zx_=(u: AnyVec2i) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2i) { z = u.x; y = u.y }
    def zw_=(u: AnyVec2i) { z = u.x; w = u.y }
    def wx_=(u: AnyVec2i) { w = u.x; x = u.y }
    def wy_=(u: AnyVec2i) { w = u.x; y = u.y }
    def wz_=(u: AnyVec2i) { w = u.x; z = u.y }

    def xyz_=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: AnyVec3i) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: AnyVec3i) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: AnyVec3i) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: AnyVec3i) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: AnyVec3i) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: AnyVec3i) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: AnyVec3i) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: AnyVec3i) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: AnyVec3i) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: AnyVec3i) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: AnyVec3i) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: AnyVec3i) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: AnyVec3i) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: AnyVec3i) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: AnyVec3i) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: AnyVec3i) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: AnyVec3i) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: AnyVec3i) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: AnyVec3i) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: AnyVec3i) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: AnyVec3i) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: AnyVec3i) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: AnyVec3i) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: AnyVec4i) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: AnyVec4i) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: AnyVec4i) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: AnyVec4i) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: AnyVec4i) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: AnyVec4i) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: AnyVec4i) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: AnyVec4i) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z;z=t}
    def yzxw_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: AnyVec4i) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: AnyVec4i) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y;y=t}
    def zwyx_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: AnyVec4i) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: AnyVec4i) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y;y=t}

    def rg_=(u: AnyVec2i) { xy_=(u) }
    def rb_=(u: AnyVec2i) { xz_=(u) }
    def ra_=(u: AnyVec2i) { xw_=(u) }
    def gr_=(u: AnyVec2i) { yx_=(u) }
    def gb_=(u: AnyVec2i) { yz_=(u) }
    def ga_=(u: AnyVec2i) { yw_=(u) }
    def br_=(u: AnyVec2i) { zx_=(u) }
    def bg_=(u: AnyVec2i) { zy_=(u) }
    def ba_=(u: AnyVec2i) { zw_=(u) }
    def ar_=(u: AnyVec2i) { wx_=(u) }
    def ag_=(u: AnyVec2i) { wy_=(u) }
    def ab_=(u: AnyVec2i) { wz_=(u) }

    def rgb_=(u: AnyVec3i) { xyz_=(u) }
    def rga_=(u: AnyVec3i) { xyw_=(u) }
    def rbg_=(u: AnyVec3i) { xzy_=(u) }
    def rba_=(u: AnyVec3i) { xzw_=(u) }
    def rag_=(u: AnyVec3i) { xwy_=(u) }
    def rab_=(u: AnyVec3i) { xwz_=(u) }
    def grb_=(u: AnyVec3i) { yxz_=(u) }
    def gra_=(u: AnyVec3i) { yxw_=(u) }
    def gbr_=(u: AnyVec3i) { yzx_=(u) }
    def gba_=(u: AnyVec3i) { yzw_=(u) }
    def gar_=(u: AnyVec3i) { ywx_=(u) }
    def gab_=(u: AnyVec3i) { ywz_=(u) }
    def brg_=(u: AnyVec3i) { zxy_=(u) }
    def bra_=(u: AnyVec3i) { zxw_=(u) }
    def bgr_=(u: AnyVec3i) { zyx_=(u) }
    def bga_=(u: AnyVec3i) { zyw_=(u) }
    def bar_=(u: AnyVec3i) { zwx_=(u) }
    def bag_=(u: AnyVec3i) { zwy_=(u) }
    def arg_=(u: AnyVec3i) { wxy_=(u) }
    def arb_=(u: AnyVec3i) { wxz_=(u) }
    def agr_=(u: AnyVec3i) { wyx_=(u) }
    def agb_=(u: AnyVec3i) { wyz_=(u) }
    def abr_=(u: AnyVec3i) { wzx_=(u) }
    def abg_=(u: AnyVec3i) { wzy_=(u) }

    def rgba_=(u: AnyVec4i) { xyzw_=(u) }
    def rgab_=(u: AnyVec4i) { xywz_=(u) }
    def rbga_=(u: AnyVec4i) { xzyw_=(u) }
    def rbag_=(u: AnyVec4i) { xzwy_=(u) }
    def ragb_=(u: AnyVec4i) { xwyz_=(u) }
    def rabg_=(u: AnyVec4i) { xwzy_=(u) }
    def grba_=(u: AnyVec4i) { yxzw_=(u) }
    def grab_=(u: AnyVec4i) { yxwz_=(u) }
    def gbra_=(u: AnyVec4i) { yzxw_=(u) }
    def gbar_=(u: AnyVec4i) { yzwx_=(u) }
    def garb_=(u: AnyVec4i) { ywxz_=(u) }
    def gabr_=(u: AnyVec4i) { ywzx_=(u) }
    def brga_=(u: AnyVec4i) { zxyw_=(u) }
    def brag_=(u: AnyVec4i) { zxwy_=(u) }
    def bgra_=(u: AnyVec4i) { zyxw_=(u) }
    def bgar_=(u: AnyVec4i) { zywx_=(u) }
    def barg_=(u: AnyVec4i) { zwxy_=(u) }
    def bagr_=(u: AnyVec4i) { zwyx_=(u) }
    def argb_=(u: AnyVec4i) { wxyz_=(u) }
    def arbg_=(u: AnyVec4i) { wxzy_=(u) }
    def agrb_=(u: AnyVec4i) { wyxz_=(u) }
    def agbr_=(u: AnyVec4i) { wyzx_=(u) }
    def abrg_=(u: AnyVec4i) { wzxy_=(u) }
    def abgr_=(u: AnyVec4i) { wzyx_=(u) }

    def st_=(u: AnyVec2i) { xy_=(u) }
    def sp_=(u: AnyVec2i) { xz_=(u) }
    def sq_=(u: AnyVec2i) { xw_=(u) }
    def ts_=(u: AnyVec2i) { yx_=(u) }
    def tp_=(u: AnyVec2i) { yz_=(u) }
    def tq_=(u: AnyVec2i) { yw_=(u) }
    def ps_=(u: AnyVec2i) { zx_=(u) }
    def pt_=(u: AnyVec2i) { zy_=(u) }
    def pq_=(u: AnyVec2i) { zw_=(u) }
    def qs_=(u: AnyVec2i) { wx_=(u) }
    def qt_=(u: AnyVec2i) { wy_=(u) }
    def qp_=(u: AnyVec2i) { wz_=(u) }

    def stp_=(u: AnyVec3i) { xyz_=(u) }
    def stq_=(u: AnyVec3i) { xyw_=(u) }
    def spt_=(u: AnyVec3i) { xzy_=(u) }
    def spq_=(u: AnyVec3i) { xzw_=(u) }
    def sqt_=(u: AnyVec3i) { xwy_=(u) }
    def sqp_=(u: AnyVec3i) { xwz_=(u) }
    def tsp_=(u: AnyVec3i) { yxz_=(u) }
    def tsq_=(u: AnyVec3i) { yxw_=(u) }
    def tps_=(u: AnyVec3i) { yzx_=(u) }
    def tpq_=(u: AnyVec3i) { yzw_=(u) }
    def tqs_=(u: AnyVec3i) { ywx_=(u) }
    def tqp_=(u: AnyVec3i) { ywz_=(u) }
    def pst_=(u: AnyVec3i) { zxy_=(u) }
    def psq_=(u: AnyVec3i) { zxw_=(u) }
    def pts_=(u: AnyVec3i) { zyx_=(u) }
    def ptq_=(u: AnyVec3i) { zyw_=(u) }
    def pqs_=(u: AnyVec3i) { zwx_=(u) }
    def pqt_=(u: AnyVec3i) { zwy_=(u) }
    def qst_=(u: AnyVec3i) { wxy_=(u) }
    def qsp_=(u: AnyVec3i) { wxz_=(u) }
    def qts_=(u: AnyVec3i) { wyx_=(u) }
    def qtp_=(u: AnyVec3i) { wyz_=(u) }
    def qps_=(u: AnyVec3i) { wzx_=(u) }
    def qpt_=(u: AnyVec3i) { wzy_=(u) }

    def stpq_=(u: AnyVec4i) { xyzw_=(u) }
    def stqp_=(u: AnyVec4i) { xywz_=(u) }
    def sptq_=(u: AnyVec4i) { xzyw_=(u) }
    def spqt_=(u: AnyVec4i) { xzwy_=(u) }
    def sqtp_=(u: AnyVec4i) { xwyz_=(u) }
    def sqpt_=(u: AnyVec4i) { xwzy_=(u) }
    def tspq_=(u: AnyVec4i) { yxzw_=(u) }
    def tsqp_=(u: AnyVec4i) { yxwz_=(u) }
    def tpsq_=(u: AnyVec4i) { yzxw_=(u) }
    def tpqs_=(u: AnyVec4i) { yzwx_=(u) }
    def tqsp_=(u: AnyVec4i) { ywxz_=(u) }
    def tqps_=(u: AnyVec4i) { ywzx_=(u) }
    def pstq_=(u: AnyVec4i) { zxyw_=(u) }
    def psqt_=(u: AnyVec4i) { zxwy_=(u) }
    def ptsq_=(u: AnyVec4i) { zyxw_=(u) }
    def ptqs_=(u: AnyVec4i) { zywx_=(u) }
    def pqst_=(u: AnyVec4i) { zwxy_=(u) }
    def pqts_=(u: AnyVec4i) { zwyx_=(u) }
    def qstp_=(u: AnyVec4i) { wxyz_=(u) }
    def qspt_=(u: AnyVec4i) { wxzy_=(u) }
    def qtsp_=(u: AnyVec4i) { wyxz_=(u) }
    def qtps_=(u: AnyVec4i) { wyzx_=(u) }
    def qpst_=(u: AnyVec4i) { wzxy_=(u) }
    def qpts_=(u: AnyVec4i) { wzyx_=(u) }
}

object Vec4i {
    val Zero = new ConstVec4i(0, 0, 0, 0)
    val UnitX = new ConstVec4i(1, 0, 0, 0)
    val UnitY = new ConstVec4i(0, 1, 0, 0)
    val UnitZ = new ConstVec4i(0, 0, 1, 0)
    val UnitW = new ConstVec4i(0, 0, 0, 1)
    val One = new ConstVec4i(1, 1, 1, 1)

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

    def apply(u: AnyVec4b) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(u: Read4Float) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(u: Read4Double) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    implicit def toMutable(u: ConstVec4i) = Vec4i(u)
}
