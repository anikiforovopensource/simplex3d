
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
private[math] trait Swizzle2Read[P, R2, R3, R4]
extends VecFactory[P, R2, R3, R4]
{
    def x: P
    def y: P


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

/*
 * A field must be defined as both read and write in the same trait
 * to be treated as a var and be eligible for assignment expansion.
 * For example both "def xy" and "def xy_=(u: Read[2])" must be defined
 * in the same trait so that "a.yx += b" compiles.
 */

/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] trait Swizzle2Write[P, R2, R3, R4]
extends Swizzle2Read[P, R2, R3, R4]
{
    override def x: P
    override def y: P

    def x_=(x: P) :Unit
    def y_=(y: P) :Unit


    override def xy: R2 = make2(x, y)
    override def yx: R2 = make2(y, x)

    override def rg = xy
    override def gr = yx

    override def st = xy
    override def ts = yx


    def xy_=(u: Read2[P]) { x = u.x; y = u.y }
    def yx_=(u: Read2[P]) { var t = u.y; y = u.x; x = t }

    def rg_=(u: Read2[P]) { xy_=(u) }
    def gr_=(u: Read2[P]) { yx_=(u) }

    def st_=(u: Read2[P]) { xy_=(u) }
    def ts_=(u: Read2[P]) { yx_=(u) }
}
