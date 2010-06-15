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
private[math] abstract class Swizzle3Read extends Swizzle2Read {
  
  private[math] def dz: Double


  def xz: R2 = make2(dx, dz)
  def yz: R2 = make2(dy, dz)
  def zx: R2 = make2(dz, dx)
  def zy: R2 = make2(dz, dy)
  def zz: R2 = make2(dz, dz)

  def xxz: R3 = make3(dx, dx, dz)
  def xyz: R3 = make3(dx, dy, dz)
  def xzx: R3 = make3(dx, dz, dx)
  def xzy: R3 = make3(dx, dz, dy)
  def xzz: R3 = make3(dx, dz, dz)
  def yxz: R3 = make3(dy, dx, dz)
  def yyz: R3 = make3(dy, dy, dz)
  def yzx: R3 = make3(dy, dz, dx)
  def yzy: R3 = make3(dy, dz, dy)
  def yzz: R3 = make3(dy, dz, dz)
  def zxx: R3 = make3(dz, dx, dx)
  def zxy: R3 = make3(dz, dx, dy)
  def zxz: R3 = make3(dz, dx, dz)
  def zyx: R3 = make3(dz, dy, dx)
  def zyy: R3 = make3(dz, dy, dy)
  def zyz: R3 = make3(dz, dy, dz)
  def zzx: R3 = make3(dz, dz, dx)
  def zzy: R3 = make3(dz, dz, dy)
  def zzz: R3 = make3(dz, dz, dz)

  def xxxz: R4 = make4(dx, dx, dx, dz)
  def xxyz: R4 = make4(dx, dx, dy, dz)
  def xxzx: R4 = make4(dx, dx, dz, dx)
  def xxzy: R4 = make4(dx, dx, dz, dy)
  def xxzz: R4 = make4(dx, dx, dz, dz)
  def xyxz: R4 = make4(dx, dy, dx, dz)
  def xyyz: R4 = make4(dx, dy, dy, dz)
  def xyzx: R4 = make4(dx, dy, dz, dx)
  def xyzy: R4 = make4(dx, dy, dz, dy)
  def xyzz: R4 = make4(dx, dy, dz, dz)
  def xzxx: R4 = make4(dx, dz, dx, dx)
  def xzxy: R4 = make4(dx, dz, dx, dy)
  def xzxz: R4 = make4(dx, dz, dx, dz)
  def xzyx: R4 = make4(dx, dz, dy, dx)
  def xzyy: R4 = make4(dx, dz, dy, dy)
  def xzyz: R4 = make4(dx, dz, dy, dz)
  def xzzx: R4 = make4(dx, dz, dz, dx)
  def xzzy: R4 = make4(dx, dz, dz, dy)
  def xzzz: R4 = make4(dx, dz, dz, dz)
  def yxxz: R4 = make4(dy, dx, dx, dz)
  def yxyz: R4 = make4(dy, dx, dy, dz)
  def yxzx: R4 = make4(dy, dx, dz, dx)
  def yxzy: R4 = make4(dy, dx, dz, dy)
  def yxzz: R4 = make4(dy, dx, dz, dz)
  def yyxz: R4 = make4(dy, dy, dx, dz)
  def yyyz: R4 = make4(dy, dy, dy, dz)
  def yyzx: R4 = make4(dy, dy, dz, dx)
  def yyzy: R4 = make4(dy, dy, dz, dy)
  def yyzz: R4 = make4(dy, dy, dz, dz)
  def yzxx: R4 = make4(dy, dz, dx, dx)
  def yzxy: R4 = make4(dy, dz, dx, dy)
  def yzxz: R4 = make4(dy, dz, dx, dz)
  def yzyx: R4 = make4(dy, dz, dy, dx)
  def yzyy: R4 = make4(dy, dz, dy, dy)
  def yzyz: R4 = make4(dy, dz, dy, dz)
  def yzzx: R4 = make4(dy, dz, dz, dx)
  def yzzy: R4 = make4(dy, dz, dz, dy)
  def yzzz: R4 = make4(dy, dz, dz, dz)
  def zxxx: R4 = make4(dz, dx, dx, dx)
  def zxxy: R4 = make4(dz, dx, dx, dy)
  def zxxz: R4 = make4(dz, dx, dx, dz)
  def zxyx: R4 = make4(dz, dx, dy, dx)
  def zxyy: R4 = make4(dz, dx, dy, dy)
  def zxyz: R4 = make4(dz, dx, dy, dz)
  def zxzx: R4 = make4(dz, dx, dz, dx)
  def zxzy: R4 = make4(dz, dx, dz, dy)
  def zxzz: R4 = make4(dz, dx, dz, dz)
  def zyxx: R4 = make4(dz, dy, dx, dx)
  def zyxy: R4 = make4(dz, dy, dx, dy)
  def zyxz: R4 = make4(dz, dy, dx, dz)
  def zyyx: R4 = make4(dz, dy, dy, dx)
  def zyyy: R4 = make4(dz, dy, dy, dy)
  def zyyz: R4 = make4(dz, dy, dy, dz)
  def zyzx: R4 = make4(dz, dy, dz, dx)
  def zyzy: R4 = make4(dz, dy, dz, dy)
  def zyzz: R4 = make4(dz, dy, dz, dz)
  def zzxx: R4 = make4(dz, dz, dx, dx)
  def zzxy: R4 = make4(dz, dz, dx, dy)
  def zzxz: R4 = make4(dz, dz, dx, dz)
  def zzyx: R4 = make4(dz, dz, dy, dx)
  def zzyy: R4 = make4(dz, dz, dy, dy)
  def zzyz: R4 = make4(dz, dz, dy, dz)
  def zzzx: R4 = make4(dz, dz, dz, dx)
  def zzzy: R4 = make4(dz, dz, dz, dy)
  def zzzz: R4 = make4(dz, dz, dz, dz)

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
