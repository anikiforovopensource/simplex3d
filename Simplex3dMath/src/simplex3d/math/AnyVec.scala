/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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


/** <code>AnyVec</code> is a base class for all vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class AnyVec[P] private[math] () {

  override def clone() = this


  private[math] type R2 <: AnyVec2[P]
  private[math] type R3 <: AnyVec3[P]
  private[math] type R4 <: AnyVec4[P]
  
  private[math] type C2 <: R2
  private[math] type C3 <: R3
  private[math] type C4 <: R4
  
  protected def make2(x: Double, y: Double) :C2
  protected def make3(x: Double, y: Double, z: Double) :C3
  protected def make4(x: Double, y: Double, z: Double, w: Double) :C4


  private[math] def bx: Boolean
  private[math] def by: Boolean

  private[math] def ix: Int
  private[math] def iy: Int

  private[math] def fx: Float
  private[math] def fy: Float

  private[math] def dx: Double
  private[math] def dy: Double


  final def xx: C2 = make2(dx, dx)
  final def xy: C2 = make2(dx, dy)
  final def yx: C2 = make2(dy, dx)
  final def yy: C2 = make2(dy, dy)

  final def xxx: C3 = make3(dx, dx, dx)
  final def xxy: C3 = make3(dx, dx, dy)
  final def xyx: C3 = make3(dx, dy, dx)
  final def xyy: C3 = make3(dx, dy, dy)
  final def yxx: C3 = make3(dy, dx, dx)
  final def yxy: C3 = make3(dy, dx, dy)
  final def yyx: C3 = make3(dy, dy, dx)
  final def yyy: C3 = make3(dy, dy, dy)

  final def xxxx: C4 = make4(dx, dx, dx, dx)
  final def xxxy: C4 = make4(dx, dx, dx, dy)
  final def xxyx: C4 = make4(dx, dx, dy, dx)
  final def xxyy: C4 = make4(dx, dx, dy, dy)
  final def xyxx: C4 = make4(dx, dy, dx, dx)
  final def xyxy: C4 = make4(dx, dy, dx, dy)
  final def xyyx: C4 = make4(dx, dy, dy, dx)
  final def xyyy: C4 = make4(dx, dy, dy, dy)
  final def yxxx: C4 = make4(dy, dx, dx, dx)
  final def yxxy: C4 = make4(dy, dx, dx, dy)
  final def yxyx: C4 = make4(dy, dx, dy, dx)
  final def yxyy: C4 = make4(dy, dx, dy, dy)
  final def yyxx: C4 = make4(dy, dy, dx, dx)
  final def yyxy: C4 = make4(dy, dy, dx, dy)
  final def yyyx: C4 = make4(dy, dy, dy, dx)
  final def yyyy: C4 = make4(dy, dy, dy, dy)

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
