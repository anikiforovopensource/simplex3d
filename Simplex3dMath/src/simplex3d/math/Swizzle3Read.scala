/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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


/** <code>Sizzle3Read</code> contains abstract read-only swizzling
 * for 3-dimensional vectors.
 *
 * @author Aleksey Nikiforov (ledx)
 */
private[math] abstract class Swizzle3Read[P] extends Swizzle2Read[P] {
  
  private[math] def dz: Double


  final def xz: R2 = make2(dx, dz)
  final def yz: R2 = make2(dy, dz)
  final def zx: R2 = make2(dz, dx)
  final def zy: R2 = make2(dz, dy)
  final def zz: R2 = make2(dz, dz)

  final def xxz: R3 = make3(dx, dx, dz)
  final def xyz: R3 = make3(dx, dy, dz)
  final def xzx: R3 = make3(dx, dz, dx)
  final def xzy: R3 = make3(dx, dz, dy)
  final def xzz: R3 = make3(dx, dz, dz)
  final def yxz: R3 = make3(dy, dx, dz)
  final def yyz: R3 = make3(dy, dy, dz)
  final def yzx: R3 = make3(dy, dz, dx)
  final def yzy: R3 = make3(dy, dz, dy)
  final def yzz: R3 = make3(dy, dz, dz)
  final def zxx: R3 = make3(dz, dx, dx)
  final def zxy: R3 = make3(dz, dx, dy)
  final def zxz: R3 = make3(dz, dx, dz)
  final def zyx: R3 = make3(dz, dy, dx)
  final def zyy: R3 = make3(dz, dy, dy)
  final def zyz: R3 = make3(dz, dy, dz)
  final def zzx: R3 = make3(dz, dz, dx)
  final def zzy: R3 = make3(dz, dz, dy)
  final def zzz: R3 = make3(dz, dz, dz)

  final def xxxz: R4 = make4(dx, dx, dx, dz)
  final def xxyz: R4 = make4(dx, dx, dy, dz)
  final def xxzx: R4 = make4(dx, dx, dz, dx)
  final def xxzy: R4 = make4(dx, dx, dz, dy)
  final def xxzz: R4 = make4(dx, dx, dz, dz)
  final def xyxz: R4 = make4(dx, dy, dx, dz)
  final def xyyz: R4 = make4(dx, dy, dy, dz)
  final def xyzx: R4 = make4(dx, dy, dz, dx)
  final def xyzy: R4 = make4(dx, dy, dz, dy)
  final def xyzz: R4 = make4(dx, dy, dz, dz)
  final def xzxx: R4 = make4(dx, dz, dx, dx)
  final def xzxy: R4 = make4(dx, dz, dx, dy)
  final def xzxz: R4 = make4(dx, dz, dx, dz)
  final def xzyx: R4 = make4(dx, dz, dy, dx)
  final def xzyy: R4 = make4(dx, dz, dy, dy)
  final def xzyz: R4 = make4(dx, dz, dy, dz)
  final def xzzx: R4 = make4(dx, dz, dz, dx)
  final def xzzy: R4 = make4(dx, dz, dz, dy)
  final def xzzz: R4 = make4(dx, dz, dz, dz)
  final def yxxz: R4 = make4(dy, dx, dx, dz)
  final def yxyz: R4 = make4(dy, dx, dy, dz)
  final def yxzx: R4 = make4(dy, dx, dz, dx)
  final def yxzy: R4 = make4(dy, dx, dz, dy)
  final def yxzz: R4 = make4(dy, dx, dz, dz)
  final def yyxz: R4 = make4(dy, dy, dx, dz)
  final def yyyz: R4 = make4(dy, dy, dy, dz)
  final def yyzx: R4 = make4(dy, dy, dz, dx)
  final def yyzy: R4 = make4(dy, dy, dz, dy)
  final def yyzz: R4 = make4(dy, dy, dz, dz)
  final def yzxx: R4 = make4(dy, dz, dx, dx)
  final def yzxy: R4 = make4(dy, dz, dx, dy)
  final def yzxz: R4 = make4(dy, dz, dx, dz)
  final def yzyx: R4 = make4(dy, dz, dy, dx)
  final def yzyy: R4 = make4(dy, dz, dy, dy)
  final def yzyz: R4 = make4(dy, dz, dy, dz)
  final def yzzx: R4 = make4(dy, dz, dz, dx)
  final def yzzy: R4 = make4(dy, dz, dz, dy)
  final def yzzz: R4 = make4(dy, dz, dz, dz)
  final def zxxx: R4 = make4(dz, dx, dx, dx)
  final def zxxy: R4 = make4(dz, dx, dx, dy)
  final def zxxz: R4 = make4(dz, dx, dx, dz)
  final def zxyx: R4 = make4(dz, dx, dy, dx)
  final def zxyy: R4 = make4(dz, dx, dy, dy)
  final def zxyz: R4 = make4(dz, dx, dy, dz)
  final def zxzx: R4 = make4(dz, dx, dz, dx)
  final def zxzy: R4 = make4(dz, dx, dz, dy)
  final def zxzz: R4 = make4(dz, dx, dz, dz)
  final def zyxx: R4 = make4(dz, dy, dx, dx)
  final def zyxy: R4 = make4(dz, dy, dx, dy)
  final def zyxz: R4 = make4(dz, dy, dx, dz)
  final def zyyx: R4 = make4(dz, dy, dy, dx)
  final def zyyy: R4 = make4(dz, dy, dy, dy)
  final def zyyz: R4 = make4(dz, dy, dy, dz)
  final def zyzx: R4 = make4(dz, dy, dz, dx)
  final def zyzy: R4 = make4(dz, dy, dz, dy)
  final def zyzz: R4 = make4(dz, dy, dz, dz)
  final def zzxx: R4 = make4(dz, dz, dx, dx)
  final def zzxy: R4 = make4(dz, dz, dx, dy)
  final def zzxz: R4 = make4(dz, dz, dx, dz)
  final def zzyx: R4 = make4(dz, dz, dy, dx)
  final def zzyy: R4 = make4(dz, dz, dy, dy)
  final def zzyz: R4 = make4(dz, dz, dy, dz)
  final def zzzx: R4 = make4(dz, dz, dz, dx)
  final def zzzy: R4 = make4(dz, dz, dz, dy)
  final def zzzz: R4 = make4(dz, dz, dz, dz)

  final def rb = xz
  final def gb = yz
  final def br = zx
  final def bg = zy
  final def bb = zz

  final def rrb = xxz
  final def rgb = xyz
  final def rbr = xzx
  final def rbg = xzy
  final def rbb = xzz
  final def grb = yxz
  final def ggb = yyz
  final def gbr = yzx
  final def gbg = yzy
  final def gbb = yzz
  final def brr = zxx
  final def brg = zxy
  final def brb = zxz
  final def bgr = zyx
  final def bgg = zyy
  final def bgb = zyz
  final def bbr = zzx
  final def bbg = zzy
  final def bbb = zzz

  final def rrrb = xxxz
  final def rrgb = xxyz
  final def rrbr = xxzx
  final def rrbg = xxzy
  final def rrbb = xxzz
  final def rgrb = xyxz
  final def rggb = xyyz
  final def rgbr = xyzx
  final def rgbg = xyzy
  final def rgbb = xyzz
  final def rbrr = xzxx
  final def rbrg = xzxy
  final def rbrb = xzxz
  final def rbgr = xzyx
  final def rbgg = xzyy
  final def rbgb = xzyz
  final def rbbr = xzzx
  final def rbbg = xzzy
  final def rbbb = xzzz
  final def grrb = yxxz
  final def grgb = yxyz
  final def grbr = yxzx
  final def grbg = yxzy
  final def grbb = yxzz
  final def ggrb = yyxz
  final def gggb = yyyz
  final def ggbr = yyzx
  final def ggbg = yyzy
  final def ggbb = yyzz
  final def gbrr = yzxx
  final def gbrg = yzxy
  final def gbrb = yzxz
  final def gbgr = yzyx
  final def gbgg = yzyy
  final def gbgb = yzyz
  final def gbbr = yzzx
  final def gbbg = yzzy
  final def gbbb = yzzz
  final def brrr = zxxx
  final def brrg = zxxy
  final def brrb = zxxz
  final def brgr = zxyx
  final def brgg = zxyy
  final def brgb = zxyz
  final def brbr = zxzx
  final def brbg = zxzy
  final def brbb = zxzz
  final def bgrr = zyxx
  final def bgrg = zyxy
  final def bgrb = zyxz
  final def bggr = zyyx
  final def bggg = zyyy
  final def bggb = zyyz
  final def bgbr = zyzx
  final def bgbg = zyzy
  final def bgbb = zyzz
  final def bbrr = zzxx
  final def bbrg = zzxy
  final def bbrb = zzxz
  final def bbgr = zzyx
  final def bbgg = zzyy
  final def bbgb = zzyz
  final def bbbr = zzzx
  final def bbbg = zzzy
  final def bbbb = zzzz

  final def sp = xz
  final def tp = yz
  final def ps = zx
  final def pt = zy
  final def pp = zz

  final def ssp = xxz
  final def stp = xyz
  final def sps = xzx
  final def spt = xzy
  final def spp = xzz
  final def tsp = yxz
  final def ttp = yyz
  final def tps = yzx
  final def tpt = yzy
  final def tpp = yzz
  final def pss = zxx
  final def pst = zxy
  final def psp = zxz
  final def pts = zyx
  final def ptt = zyy
  final def ptp = zyz
  final def pps = zzx
  final def ppt = zzy
  final def ppp = zzz

  final def sssp = xxxz
  final def sstp = xxyz
  final def ssps = xxzx
  final def sspt = xxzy
  final def sspp = xxzz
  final def stsp = xyxz
  final def sttp = xyyz
  final def stps = xyzx
  final def stpt = xyzy
  final def stpp = xyzz
  final def spss = xzxx
  final def spst = xzxy
  final def spsp = xzxz
  final def spts = xzyx
  final def sptt = xzyy
  final def sptp = xzyz
  final def spps = xzzx
  final def sppt = xzzy
  final def sppp = xzzz
  final def tssp = yxxz
  final def tstp = yxyz
  final def tsps = yxzx
  final def tspt = yxzy
  final def tspp = yxzz
  final def ttsp = yyxz
  final def tttp = yyyz
  final def ttps = yyzx
  final def ttpt = yyzy
  final def ttpp = yyzz
  final def tpss = yzxx
  final def tpst = yzxy
  final def tpsp = yzxz
  final def tpts = yzyx
  final def tptt = yzyy
  final def tptp = yzyz
  final def tpps = yzzx
  final def tppt = yzzy
  final def tppp = yzzz
  final def psss = zxxx
  final def psst = zxxy
  final def pssp = zxxz
  final def psts = zxyx
  final def pstt = zxyy
  final def pstp = zxyz
  final def psps = zxzx
  final def pspt = zxzy
  final def pspp = zxzz
  final def ptss = zyxx
  final def ptst = zyxy
  final def ptsp = zyxz
  final def ptts = zyyx
  final def pttt = zyyy
  final def pttp = zyyz
  final def ptps = zyzx
  final def ptpt = zyzy
  final def ptpp = zyzz
  final def ppss = zzxx
  final def ppst = zzxy
  final def ppsp = zzxz
  final def ppts = zzyx
  final def pptt = zzyy
  final def pptp = zzyz
  final def ppps = zzzx
  final def pppt = zzzy
  final def pppp = zzzz


  protected def xz_=(u: R2) { throw new UnsupportedOperationException }
  protected def yz_=(u: R2) { throw new UnsupportedOperationException }
  protected def zx_=(u: R2) { throw new UnsupportedOperationException }
  protected def zy_=(u: R2) { throw new UnsupportedOperationException }

  protected def xyz_=(u: R3) { throw new UnsupportedOperationException }
  protected def xzy_=(u: R3) { throw new UnsupportedOperationException }
  protected def yxz_=(u: R3) { throw new UnsupportedOperationException }
  protected def yzx_=(u: R3) { throw new UnsupportedOperationException }
  protected def zxy_=(u: R3) { throw new UnsupportedOperationException }
  protected def zyx_=(u: R3) { throw new UnsupportedOperationException }

  protected def rb_=(u: R2) { throw new UnsupportedOperationException }
  protected def gb_=(u: R2) { throw new UnsupportedOperationException }
  protected def br_=(u: R2) { throw new UnsupportedOperationException }
  protected def bg_=(u: R2) { throw new UnsupportedOperationException }

  protected def rgb_=(u: R3) { throw new UnsupportedOperationException }
  protected def rbg_=(u: R3) { throw new UnsupportedOperationException }
  protected def grb_=(u: R3) { throw new UnsupportedOperationException }
  protected def gbr_=(u: R3) { throw new UnsupportedOperationException }
  protected def brg_=(u: R3) { throw new UnsupportedOperationException }
  protected def bgr_=(u: R3) { throw new UnsupportedOperationException }

  protected def sp_=(u: R2) { throw new UnsupportedOperationException }
  protected def tp_=(u: R2) { throw new UnsupportedOperationException }
  protected def ps_=(u: R2) { throw new UnsupportedOperationException }
  protected def pt_=(u: R2) { throw new UnsupportedOperationException }

  protected def stp_=(u: R3) { throw new UnsupportedOperationException }
  protected def spt_=(u: R3) { throw new UnsupportedOperationException }
  protected def tsp_=(u: R3) { throw new UnsupportedOperationException }
  protected def tps_=(u: R3) { throw new UnsupportedOperationException }
  protected def pst_=(u: R3) { throw new UnsupportedOperationException }
  protected def pts_=(u: R3) { throw new UnsupportedOperationException }
}
