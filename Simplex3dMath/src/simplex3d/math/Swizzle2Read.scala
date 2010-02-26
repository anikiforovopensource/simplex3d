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


/** Sizzle2Read contains abstract read-only swizzling for 2-dimensional vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
private[math] abstract class Swizzle2Read[T] extends AnyVec[T] {

    private[math] type R2
    private[math] type R3
    private[math] type R4
    
    protected def make2(x: T, y: T) :R2
    protected def make3(x: T, y: T, z: T) :R3
    protected def make4(x: T, y: T, z: T, w: T) :R4

    def x: T
    def y: T


    def xx: R2 = make2(x, x)
    def xy: R2 = make2(x, y)
    def yx: R2 = make2(y, x)
    def yy: R2 = make2(y, y)

    def xxx: R3 = make3(x, x, x)
    def xxy: R3 = make3(x, x, y)
    def xyx: R3 = make3(x, y, x)
    def xyy: R3 = make3(x, y, y)
    def yxx: R3 = make3(y, x, x)
    def yxy: R3 = make3(y, x, y)
    def yyx: R3 = make3(y, y, x)
    def yyy: R3 = make3(y, y, y)

    def xxxx: R4 = make4(x, x, x, x)
    def xxxy: R4 = make4(x, x, x, y)
    def xxyx: R4 = make4(x, x, y, x)
    def xxyy: R4 = make4(x, x, y, y)
    def xyxx: R4 = make4(x, y, x, x)
    def xyxy: R4 = make4(x, y, x, y)
    def xyyx: R4 = make4(x, y, y, x)
    def xyyy: R4 = make4(x, y, y, y)
    def yxxx: R4 = make4(y, x, x, x)
    def yxxy: R4 = make4(y, x, x, y)
    def yxyx: R4 = make4(y, x, y, x)
    def yxyy: R4 = make4(y, x, y, y)
    def yyxx: R4 = make4(y, y, x, x)
    def yyxy: R4 = make4(y, y, x, y)
    def yyyx: R4 = make4(y, y, y, x)
    def yyyy: R4 = make4(y, y, y, y)

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
