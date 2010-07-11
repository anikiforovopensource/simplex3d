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


/** <code>Sizzle2Read</code> contains abstract read-only swizzling
 * for 2-dimensional vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Swizzle2Read[P, T] extends ReadVec[P, T] {

  private[math] type R2
  private[math] type R3
  private[math] type R4
  
  protected def make2(x: Double, y: Double) :R2
  protected def make3(x: Double, y: Double, z: Double) :R3
  protected def make4(x: Double, y: Double, z: Double, w: Double) :R4

  private[math] def dx: Double
  private[math] def dy: Double


  final def xx: R2 = make2(dx, dx)
  final def xy: R2 = make2(dx, dy)
  final def yx: R2 = make2(dy, dx)
  final def yy: R2 = make2(dy, dy)

  final def xxx: R3 = make3(dx, dx, dx)
  final def xxy: R3 = make3(dx, dx, dy)
  final def xyx: R3 = make3(dx, dy, dx)
  final def xyy: R3 = make3(dx, dy, dy)
  final def yxx: R3 = make3(dy, dx, dx)
  final def yxy: R3 = make3(dy, dx, dy)
  final def yyx: R3 = make3(dy, dy, dx)
  final def yyy: R3 = make3(dy, dy, dy)

  final def xxxx: R4 = make4(dx, dx, dx, dx)
  final def xxxy: R4 = make4(dx, dx, dx, dy)
  final def xxyx: R4 = make4(dx, dx, dy, dx)
  final def xxyy: R4 = make4(dx, dx, dy, dy)
  final def xyxx: R4 = make4(dx, dy, dx, dx)
  final def xyxy: R4 = make4(dx, dy, dx, dy)
  final def xyyx: R4 = make4(dx, dy, dy, dx)
  final def xyyy: R4 = make4(dx, dy, dy, dy)
  final def yxxx: R4 = make4(dy, dx, dx, dx)
  final def yxxy: R4 = make4(dy, dx, dx, dy)
  final def yxyx: R4 = make4(dy, dx, dy, dx)
  final def yxyy: R4 = make4(dy, dx, dy, dy)
  final def yyxx: R4 = make4(dy, dy, dx, dx)
  final def yyxy: R4 = make4(dy, dy, dx, dy)
  final def yyyx: R4 = make4(dy, dy, dy, dx)
  final def yyyy: R4 = make4(dy, dy, dy, dy)

  final def rr = xx
  final def rg = xy
  final def gr = yx
  final def gg = yy

  final def rrr = xxx
  final def rrg = xxy
  final def rgr = xyx
  final def rgg = xyy
  final def grr = yxx
  final def grg = yxy
  final def ggr = yyx
  final def ggg = yyy

  final def rrrr = xxxx
  final def rrrg = xxxy
  final def rrgr = xxyx
  final def rrgg = xxyy
  final def rgrr = xyxx
  final def rgrg = xyxy
  final def rggr = xyyx
  final def rggg = xyyy
  final def grrr = yxxx
  final def grrg = yxxy
  final def grgr = yxyx
  final def grgg = yxyy
  final def ggrr = yyxx
  final def ggrg = yyxy
  final def gggr = yyyx
  final def gggg = yyyy

  final def ss = xx
  final def st = xy
  final def ts = yx
  final def tt = yy

  final def sss = xxx
  final def sst = xxy
  final def sts = xyx
  final def stt = xyy
  final def tss = yxx
  final def tst = yxy
  final def tts = yyx
  final def ttt = yyy

  final def ssss = xxxx
  final def ssst = xxxy
  final def ssts = xxyx
  final def sstt = xxyy
  final def stss = xyxx
  final def stst = xyxy
  final def stts = xyyx
  final def sttt = xyyy
  final def tsss = yxxx
  final def tsst = yxxy
  final def tsts = yxyx
  final def tstt = yxyy
  final def ttss = yyxx
  final def ttst = yyxy
  final def ttts = yyyx
  final def tttt = yyyy


  protected def xy_=(u: R2) { throw new UnsupportedOperationException }
  protected def yx_=(u: R2) { throw new UnsupportedOperationException }

  protected def rg_=(u: R2) { throw new UnsupportedOperationException }
  protected def gr_=(u: R2) { throw new UnsupportedOperationException }

  protected def st_=(u: R2) { throw new UnsupportedOperationException }
  protected def ts_=(u: R2) { throw new UnsupportedOperationException }
}
