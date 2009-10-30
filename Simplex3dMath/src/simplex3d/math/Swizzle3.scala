
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


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] trait Swizzle3Read[P, R2, R3, R4]
extends VecFactory[P, R2, R3, R4]
{
    def x: P
    def y: P
    def z: P


    def xx: R2 = make2(x, x)
    def xy: R2 = make2(x, y)
    def xz: R2 = make2(x, z)
    def yx: R2 = make2(y, x)
    def yy: R2 = make2(y, y)
    def yz: R2 = make2(y, z)
    def zx: R2 = make2(z, x)
    def zy: R2 = make2(z, y)
    def zz: R2 = make2(z, z)

    def xxx: R3 = make3(x, x, x)
    def xxy: R3 = make3(x, x, y)
    def xxz: R3 = make3(x, x, z)
    def xyx: R3 = make3(x, y, x)
    def xyy: R3 = make3(x, y, y)
    def xyz: R3 = make3(x, y, z)
    def xzx: R3 = make3(x, z, x)
    def xzy: R3 = make3(x, z, y)
    def xzz: R3 = make3(x, z, z)
    def yxx: R3 = make3(y, x, x)
    def yxy: R3 = make3(y, x, y)
    def yxz: R3 = make3(y, x, z)
    def yyx: R3 = make3(y, y, x)
    def yyy: R3 = make3(y, y, y)
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

    def xxxx: R4 = make4(x, x, x, x)
    def xxxy: R4 = make4(x, x, x, y)
    def xxxz: R4 = make4(x, x, x, z)
    def xxyx: R4 = make4(x, x, y, x)
    def xxyy: R4 = make4(x, x, y, y)
    def xxyz: R4 = make4(x, x, y, z)
    def xxzx: R4 = make4(x, x, z, x)
    def xxzy: R4 = make4(x, x, z, y)
    def xxzz: R4 = make4(x, x, z, z)
    def xyxx: R4 = make4(x, y, x, x)
    def xyxy: R4 = make4(x, y, x, y)
    def xyxz: R4 = make4(x, y, x, z)
    def xyyx: R4 = make4(x, y, y, x)
    def xyyy: R4 = make4(x, y, y, y)
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
    def yxxx: R4 = make4(y, x, x, x)
    def yxxy: R4 = make4(y, x, x, y)
    def yxxz: R4 = make4(y, x, x, z)
    def yxyx: R4 = make4(y, x, y, x)
    def yxyy: R4 = make4(y, x, y, y)
    def yxyz: R4 = make4(y, x, y, z)
    def yxzx: R4 = make4(y, x, z, x)
    def yxzy: R4 = make4(y, x, z, y)
    def yxzz: R4 = make4(y, x, z, z)
    def yyxx: R4 = make4(y, y, x, x)
    def yyxy: R4 = make4(y, y, x, y)
    def yyxz: R4 = make4(y, y, x, z)
    def yyyx: R4 = make4(y, y, y, x)
    def yyyy: R4 = make4(y, y, y, y)
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

    def ss = xx
    def st = xy
    def sp = xz
    def ts = yx
    def tt = yy
    def tp = yz
    def ps = zx
    def pt = zy
    def pp = zz

    def sss = xxx
    def sst = xxy
    def ssp = xxz
    def sts = xyx
    def stt = xyy
    def stp = xyz
    def sps = xzx
    def spt = xzy
    def spp = xzz
    def tss = yxx
    def tst = yxy
    def tsp = yxz
    def tts = yyx
    def ttt = yyy
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

    def ssss = xxxx
    def ssst = xxxy
    def sssp = xxxz
    def ssts = xxyx
    def sstt = xxyy
    def sstp = xxyz
    def ssps = xxzx
    def sspt = xxzy
    def sspp = xxzz
    def stss = xyxx
    def stst = xyxy
    def stsp = xyxz
    def stts = xyyx
    def sttt = xyyy
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
    def tsss = yxxx
    def tsst = yxxy
    def tssp = yxxz
    def tsts = yxyx
    def tstt = yxyy
    def tstp = yxyz
    def tsps = yxzx
    def tspt = yxzy
    def tspp = yxzz
    def ttss = yyxx
    def ttst = yyxy
    def ttsp = yyxz
    def ttts = yyyx
    def tttt = yyyy
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
    override def x: P
    override def y: P
    override def z: P

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
