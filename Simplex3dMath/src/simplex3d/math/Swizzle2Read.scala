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
private[math] abstract class Swizzle2Read {

  private[math] type R2
  private[math] type R3
  private[math] type R4
  
  protected def make2(x: Double, y: Double) :R2
  protected def make3(x: Double, y: Double, z: Double) :R3
  protected def make4(x: Double, y: Double, z: Double, w: Double) :R4

  private[math] def dx: Double
  private[math] def dy: Double


  def xx: R2 = make2(dx, dx)
  def xy: R2 = make2(dx, dy)
  def yx: R2 = make2(dy, dx)
  def yy: R2 = make2(dy, dy)

  def xxx: R3 = make3(dx, dx, dx)
  def xxy: R3 = make3(dx, dx, dy)
  def xyx: R3 = make3(dx, dy, dx)
  def xyy: R3 = make3(dx, dy, dy)
  def yxx: R3 = make3(dy, dx, dx)
  def yxy: R3 = make3(dy, dx, dy)
  def yyx: R3 = make3(dy, dy, dx)
  def yyy: R3 = make3(dy, dy, dy)

  def xxxx: R4 = make4(dx, dx, dx, dx)
  def xxxy: R4 = make4(dx, dx, dx, dy)
  def xxyx: R4 = make4(dx, dx, dy, dx)
  def xxyy: R4 = make4(dx, dx, dy, dy)
  def xyxx: R4 = make4(dx, dy, dx, dx)
  def xyxy: R4 = make4(dx, dy, dx, dy)
  def xyyx: R4 = make4(dx, dy, dy, dx)
  def xyyy: R4 = make4(dx, dy, dy, dy)
  def yxxx: R4 = make4(dy, dx, dx, dx)
  def yxxy: R4 = make4(dy, dx, dx, dy)
  def yxyx: R4 = make4(dy, dx, dy, dx)
  def yxyy: R4 = make4(dy, dx, dy, dy)
  def yyxx: R4 = make4(dy, dy, dx, dx)
  def yyxy: R4 = make4(dy, dy, dx, dy)
  def yyyx: R4 = make4(dy, dy, dy, dx)
  def yyyy: R4 = make4(dy, dy, dy, dy)

  def rr = xx
  def rg = xy
  def gr = yx
  def gg = yy

  def rrr = xxx
  def rrg = xxy
  def rgr = xyx
  def rgg = xyy
  def grr = yxx
  def grg = yxy
  def ggr = yyx
  def ggg = yyy

  def rrrr = xxxx
  def rrrg = xxxy
  def rrgr = xxyx
  def rrgg = xxyy
  def rgrr = xyxx
  def rgrg = xyxy
  def rggr = xyyx
  def rggg = xyyy
  def grrr = yxxx
  def grrg = yxxy
  def grgr = yxyx
  def grgg = yxyy
  def ggrr = yyxx
  def ggrg = yyxy
  def gggr = yyyx
  def gggg = yyyy

  def ss = xx
  def st = xy
  def ts = yx
  def tt = yy

  def sss = xxx
  def sst = xxy
  def sts = xyx
  def stt = xyy
  def tss = yxx
  def tst = yxy
  def tts = yyx
  def ttt = yyy

  def ssss = xxxx
  def ssst = xxxy
  def ssts = xxyx
  def sstt = xxyy
  def stss = xyxx
  def stst = xyxy
  def stts = xyyx
  def sttt = xyyy
  def tsss = yxxx
  def tsst = yxxy
  def tsts = yxyx
  def tstt = yxyy
  def ttss = yyxx
  def ttst = yyxy
  def ttts = yyyx
  def tttt = yyyy
}
