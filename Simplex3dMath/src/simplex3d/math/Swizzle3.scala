
/*
 * Simplex3D, Math module
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

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] trait Swizzle3Read[P, R2, R3, R4]
extends Swizzle2Read[P, R2, R3, R4]
{
    def z: P


    def xz: R2 = make2(x, z)
    def yz: R2 = make2(y, z)
    def zx: R2 = make2(z, x)
    def zy: R2 = make2(z, y)
    def zz: R2 = make2(z, z)

    def xxz: R3 = make3(x, x, z)
    def xyz: R3 = make3(x, y, z)
    def xzx: R3 = make3(x, z, x)
    def xzy: R3 = make3(x, z, y)
    def xzz: R3 = make3(x, z, z)
    def yxz: R3 = make3(y, x, z)
    def yyz: R3 = make3(y, y, z)
    def yzx: R3 = make3(y, z, x)
    def yzy: R3 = make3(y, z, y)
    def yzz: R3 = make3(y, z, z)
    def zxx: R3 = make3(z, x, x)
    def zxy: R3 = make3(z, x, y)
    def zxz: R3 = make3(z, x, z)
    def zyx: R3 = make3(z, y, x)
    def zyy: R3 = make3(z, y, y)
    def zyz: R3 = make3(z, y, z)
    def zzx: R3 = make3(z, z, x)
    def zzy: R3 = make3(z, z, y)
    def zzz: R3 = make3(z, z, z)

    def xxxz: R4 = make4(x, x, x, z)
    def xxyz: R4 = make4(x, x, y, z)
    def xxzx: R4 = make4(x, x, z, x)
    def xxzy: R4 = make4(x, x, z, y)
    def xxzz: R4 = make4(x, x, z, z)
    def xyxz: R4 = make4(x, y, x, z)
    def xyyz: R4 = make4(x, y, y, z)
    def xyzx: R4 = make4(x, y, z, x)
    def xyzy: R4 = make4(x, y, z, y)
    def xyzz: R4 = make4(x, y, z, z)
    def xzxx: R4 = make4(x, z, x, x)
    def xzxy: R4 = make4(x, z, x, y)
    def xzxz: R4 = make4(x, z, x, z)
    def xzyx: R4 = make4(x, z, y, x)
    def xzyy: R4 = make4(x, z, y, y)
    def xzyz: R4 = make4(x, z, y, z)
    def xzzx: R4 = make4(x, z, z, x)
    def xzzy: R4 = make4(x, z, z, y)
    def xzzz: R4 = make4(x, z, z, z)
    def yxxz: R4 = make4(y, x, x, z)
    def yxyz: R4 = make4(y, x, y, z)
    def yxzx: R4 = make4(y, x, z, x)
    def yxzy: R4 = make4(y, x, z, y)
    def yxzz: R4 = make4(y, x, z, z)
    def yyxz: R4 = make4(y, y, x, z)
    def yyyz: R4 = make4(y, y, y, z)
    def yyzx: R4 = make4(y, y, z, x)
    def yyzy: R4 = make4(y, y, z, y)
    def yyzz: R4 = make4(y, y, z, z)
    def yzxx: R4 = make4(y, z, x, x)
    def yzxy: R4 = make4(y, z, x, y)
    def yzxz: R4 = make4(y, z, x, z)
    def yzyx: R4 = make4(y, z, y, x)
    def yzyy: R4 = make4(y, z, y, y)
    def yzyz: R4 = make4(y, z, y, z)
    def yzzx: R4 = make4(y, z, z, x)
    def yzzy: R4 = make4(y, z, z, y)
    def yzzz: R4 = make4(y, z, z, z)
    def zxxx: R4 = make4(z, x, x, x)
    def zxxy: R4 = make4(z, x, x, y)
    def zxxz: R4 = make4(z, x, x, z)
    def zxyx: R4 = make4(z, x, y, x)
    def zxyy: R4 = make4(z, x, y, y)
    def zxyz: R4 = make4(z, x, y, z)
    def zxzx: R4 = make4(z, x, z, x)
    def zxzy: R4 = make4(z, x, z, y)
    def zxzz: R4 = make4(z, x, z, z)
    def zyxx: R4 = make4(z, y, x, x)
    def zyxy: R4 = make4(z, y, x, y)
    def zyxz: R4 = make4(z, y, x, z)
    def zyyx: R4 = make4(z, y, y, x)
    def zyyy: R4 = make4(z, y, y, y)
    def zyyz: R4 = make4(z, y, y, z)
    def zyzx: R4 = make4(z, y, z, x)
    def zyzy: R4 = make4(z, y, z, y)
    def zyzz: R4 = make4(z, y, z, z)
    def zzxx: R4 = make4(z, z, x, x)
    def zzxy: R4 = make4(z, z, x, y)
    def zzxz: R4 = make4(z, z, x, z)
    def zzyx: R4 = make4(z, z, y, x)
    def zzyy: R4 = make4(z, z, y, y)
    def zzyz: R4 = make4(z, z, y, z)
    def zzzx: R4 = make4(z, z, z, x)
    def zzzy: R4 = make4(z, z, z, y)
    def zzzz: R4 = make4(z, z, z, z)

    def rb = xz
    def gb = yz
    def br = zx
    def bg = zy
    def bb = zz

    def rrb = xxz
    def rgb = xyz
    def rbr = xzx
    def rbg = xzy
    def rbb = xzz
    def grb = yxz
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

    def rrrb = xxxz
    def rrgb = xxyz
    def rrbr = xxzx
    def rrbg = xxzy
    def rrbb = xxzz
    def rgrb = xyxz
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
    def grrb = yxxz
    def grgb = yxyz
    def grbr = yxzx
    def grbg = yxzy
    def grbb = yxzz
    def ggrb = yyxz
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

    def sp = xz
    def tp = yz
    def ps = zx
    def pt = zy
    def pp = zz

    def ssp = xxz
    def stp = xyz
    def sps = xzx
    def spt = xzy
    def spp = xzz
    def tsp = yxz
    def ttp = yyz
    def tps = yzx
    def tpt = yzy
    def tpp = yzz
    def pss = zxx
    def pst = zxy
    def psp = zxz
    def pts = zyx
    def ptt = zyy
    def ptp = zyz
    def pps = zzx
    def ppt = zzy
    def ppp = zzz

    def sssp = xxxz
    def sstp = xxyz
    def ssps = xxzx
    def sspt = xxzy
    def sspp = xxzz
    def stsp = xyxz
    def sttp = xyyz
    def stps = xyzx
    def stpt = xyzy
    def stpp = xyzz
    def spss = xzxx
    def spst = xzxy
    def spsp = xzxz
    def spts = xzyx
    def sptt = xzyy
    def sptp = xzyz
    def spps = xzzx
    def sppt = xzzy
    def sppp = xzzz
    def tssp = yxxz
    def tstp = yxyz
    def tsps = yxzx
    def tspt = yxzy
    def tspp = yxzz
    def ttsp = yyxz
    def tttp = yyyz
    def ttps = yyzx
    def ttpt = yyzy
    def ttpp = yyzz
    def tpss = yzxx
    def tpst = yzxy
    def tpsp = yzxz
    def tpts = yzyx
    def tptt = yzyy
    def tptp = yzyz
    def tpps = yzzx
    def tppt = yzzy
    def tppp = yzzz
    def psss = zxxx
    def psst = zxxy
    def pssp = zxxz
    def psts = zxyx
    def pstt = zxyy
    def pstp = zxyz
    def psps = zxzx
    def pspt = zxzy
    def pspp = zxzz
    def ptss = zyxx
    def ptst = zyxy
    def ptsp = zyxz
    def ptts = zyyx
    def pttt = zyyy
    def pttp = zyyz
    def ptps = zyzx
    def ptpt = zyzy
    def ptpp = zyzz
    def ppss = zzxx
    def ppst = zzxy
    def ppsp = zzxz
    def ppts = zzyx
    def pptt = zzyy
    def pptp = zzyz
    def ppps = zzzx
    def pppt = zzzy
    def pppp = zzzz
}

/*
 * A field must be defined as both read and write in the same trait
 * to be treated as a var and be eligible for assignment expansion.
 * For example both "def xy" and "def xy_=(u: Read[2])" must be defined
 * in the same trait so that "a.yx += b" compiles.
 */

/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] trait Swizzle3Write[P, R2, R3, R4]
extends Swizzle3Read[P, R2, R3, R4]
{
    def x_=(x: P) :Unit
    def y_=(y: P) :Unit
    def z_=(z: P) :Unit


    override def xy: R2 = make2(x, y)
    override def xz: R2 = make2(x, z)
    override def yx: R2 = make2(y, x)
    override def yz: R2 = make2(y, z)
    override def zx: R2 = make2(z, x)
    override def zy: R2 = make2(z, y)

    override def xyz: R3 = make3(x, y, z)
    override def xzy: R3 = make3(x, z, y)
    override def yxz: R3 = make3(y, x, z)
    override def yzx: R3 = make3(y, z, x)
    override def zxy: R3 = make3(z, x, y)
    override def zyx: R3 = make3(z, y, x)

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


    def xy_=(u: Read2[P]) { x = u.x; y = u.y }
    def xz_=(u: Read2[P]) { x = u.x; z = u.y }
    def yx_=(u: Read2[P]) { y = u.x; x = u.y }
    def yz_=(u: Read2[P]) { y = u.x; z = u.y }
    def zx_=(u: Read2[P]) { z = u.x; x = u.y }
    def zy_=(u: Read2[P]) { z = u.x; y = u.y }

    def xyz_=(u: Read3[P]) { x = u.x; y = u.y; z = u.z }
    def xzy_=(u: Read3[P]) { x = u.x; var t = u.z; z = u.y; y = t }
    def yxz_=(u: Read3[P]) { var t = u.y; y = u.x; x = t; z = u.z }
    def yzx_=(u: Read3[P]) { var t = u.y; y = u.x; x = u.z; z = t }
    def zxy_=(u: Read3[P]) { var t = u.z; z = u.x; x = u.y; y = t }
    def zyx_=(u: Read3[P]) { var t = u.z; z = u.x; x = t; y = u.y }

    def rg_=(u: Read2[P]) { xy_=(u) }
    def rb_=(u: Read2[P]) { xz_=(u) }
    def gr_=(u: Read2[P]) { yx_=(u) }
    def gb_=(u: Read2[P]) { yz_=(u) }
    def br_=(u: Read2[P]) { zx_=(u) }
    def bg_=(u: Read2[P]) { zy_=(u) }

    def rgb_=(u: Read3[P]) { xyz_=(u) }
    def rbg_=(u: Read3[P]) { xzy_=(u) }
    def grb_=(u: Read3[P]) { yxz_=(u) }
    def gbr_=(u: Read3[P]) { yzx_=(u) }
    def brg_=(u: Read3[P]) { zxy_=(u) }
    def bgr_=(u: Read3[P]) { zyx_=(u) }

    def st_=(u: Read2[P]) { xy_=(u) }
    def sp_=(u: Read2[P]) { xz_=(u) }
    def ts_=(u: Read2[P]) { yx_=(u) }
    def tp_=(u: Read2[P]) { yz_=(u) }
    def ps_=(u: Read2[P]) { zx_=(u) }
    def pt_=(u: Read2[P]) { zy_=(u) }

    def stp_=(u: Read3[P]) { xyz_=(u) }
    def spt_=(u: Read3[P]) { xzy_=(u) }
    def tsp_=(u: Read3[P]) { yxz_=(u) }
    def tps_=(u: Read3[P]) { yzx_=(u) }
    def pst_=(u: Read3[P]) { zxy_=(u) }
    def pts_=(u: Read3[P]) { zyx_=(u) }
}
