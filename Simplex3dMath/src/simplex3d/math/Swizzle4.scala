
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
protected trait Swizzle4Read[P, R2, R3, R4]
extends VecFactory[P, R2, R3, R4]
{
    def x: P
    def y: P
    def z: P
    def w: P

    def r = x
    def g = y
    def b = z
    def a = w

    def s = x
    def t = y
    def p = z
    def q = w
    def xx: R2 = make2(x, x)
    def xy: R2 = make2(x, y)
    def xz: R2 = make2(x, z)
    def xw: R2 = make2(x, w)
    def yx: R2 = make2(y, x)
    def yy: R2 = make2(y, y)
    def yz: R2 = make2(y, z)
    def yw: R2 = make2(y, w)
    def zx: R2 = make2(z, x)
    def zy: R2 = make2(z, y)
    def zz: R2 = make2(z, z)
    def zw: R2 = make2(z, w)
    def wx: R2 = make2(w, x)
    def wy: R2 = make2(w, y)
    def wz: R2 = make2(w, z)
    def ww: R2 = make2(w, w)

    def xxx: R3 = make3(x, x, x)
    def xxy: R3 = make3(x, x, y)
    def xxz: R3 = make3(x, x, z)
    def xxw: R3 = make3(x, x, w)
    def xyx: R3 = make3(x, y, x)
    def xyy: R3 = make3(x, y, y)
    def xyz: R3 = make3(x, y, z)
    def xyw: R3 = make3(x, y, w)
    def xzx: R3 = make3(x, z, x)
    def xzy: R3 = make3(x, z, y)
    def xzz: R3 = make3(x, z, z)
    def xzw: R3 = make3(x, z, w)
    def xwx: R3 = make3(x, w, x)
    def xwy: R3 = make3(x, w, y)
    def xwz: R3 = make3(x, w, z)
    def xww: R3 = make3(x, w, w)
    def yxx: R3 = make3(y, x, x)
    def yxy: R3 = make3(y, x, y)
    def yxz: R3 = make3(y, x, z)
    def yxw: R3 = make3(y, x, w)
    def yyx: R3 = make3(y, y, x)
    def yyy: R3 = make3(y, y, y)
    def yyz: R3 = make3(y, y, z)
    def yyw: R3 = make3(y, y, w)
    def yzx: R3 = make3(y, z, x)
    def yzy: R3 = make3(y, z, y)
    def yzz: R3 = make3(y, z, z)
    def yzw: R3 = make3(y, z, w)
    def ywx: R3 = make3(y, w, x)
    def ywy: R3 = make3(y, w, y)
    def ywz: R3 = make3(y, w, z)
    def yww: R3 = make3(y, w, w)
    def zxx: R3 = make3(z, x, x)
    def zxy: R3 = make3(z, x, y)
    def zxz: R3 = make3(z, x, z)
    def zxw: R3 = make3(z, x, w)
    def zyx: R3 = make3(z, y, x)
    def zyy: R3 = make3(z, y, y)
    def zyz: R3 = make3(z, y, z)
    def zyw: R3 = make3(z, y, w)
    def zzx: R3 = make3(z, z, x)
    def zzy: R3 = make3(z, z, y)
    def zzz: R3 = make3(z, z, z)
    def zzw: R3 = make3(z, z, w)
    def zwx: R3 = make3(z, w, x)
    def zwy: R3 = make3(z, w, y)
    def zwz: R3 = make3(z, w, z)
    def zww: R3 = make3(z, w, w)
    def wxx: R3 = make3(w, x, x)
    def wxy: R3 = make3(w, x, y)
    def wxz: R3 = make3(w, x, z)
    def wxw: R3 = make3(w, x, w)
    def wyx: R3 = make3(w, y, x)
    def wyy: R3 = make3(w, y, y)
    def wyz: R3 = make3(w, y, z)
    def wyw: R3 = make3(w, y, w)
    def wzx: R3 = make3(w, z, x)
    def wzy: R3 = make3(w, z, y)
    def wzz: R3 = make3(w, z, z)
    def wzw: R3 = make3(w, z, w)
    def wwx: R3 = make3(w, w, x)
    def wwy: R3 = make3(w, w, y)
    def wwz: R3 = make3(w, w, z)
    def www: R3 = make3(w, w, w)

    def xxxx: R4 = make4(x, x, x, x)
    def xxxy: R4 = make4(x, x, x, y)
    def xxxz: R4 = make4(x, x, x, z)
    def xxxw: R4 = make4(x, x, x, w)
    def xxyx: R4 = make4(x, x, y, x)
    def xxyy: R4 = make4(x, x, y, y)
    def xxyz: R4 = make4(x, x, y, z)
    def xxyw: R4 = make4(x, x, y, w)
    def xxzx: R4 = make4(x, x, z, x)
    def xxzy: R4 = make4(x, x, z, y)
    def xxzz: R4 = make4(x, x, z, z)
    def xxzw: R4 = make4(x, x, z, w)
    def xxwx: R4 = make4(x, x, w, x)
    def xxwy: R4 = make4(x, x, w, y)
    def xxwz: R4 = make4(x, x, w, z)
    def xxww: R4 = make4(x, x, w, w)
    def xyxx: R4 = make4(x, y, x, x)
    def xyxy: R4 = make4(x, y, x, y)
    def xyxz: R4 = make4(x, y, x, z)
    def xyxw: R4 = make4(x, y, x, w)
    def xyyx: R4 = make4(x, y, y, x)
    def xyyy: R4 = make4(x, y, y, y)
    def xyyz: R4 = make4(x, y, y, z)
    def xyyw: R4 = make4(x, y, y, w)
    def xyzx: R4 = make4(x, y, z, x)
    def xyzy: R4 = make4(x, y, z, y)
    def xyzz: R4 = make4(x, y, z, z)
    def xyzw: R4 = make4(x, y, z, w)
    def xywx: R4 = make4(x, y, w, x)
    def xywy: R4 = make4(x, y, w, y)
    def xywz: R4 = make4(x, y, w, z)
    def xyww: R4 = make4(x, y, w, w)
    def xzxx: R4 = make4(x, z, x, x)
    def xzxy: R4 = make4(x, z, x, y)
    def xzxz: R4 = make4(x, z, x, z)
    def xzxw: R4 = make4(x, z, x, w)
    def xzyx: R4 = make4(x, z, y, x)
    def xzyy: R4 = make4(x, z, y, y)
    def xzyz: R4 = make4(x, z, y, z)
    def xzyw: R4 = make4(x, z, y, w)
    def xzzx: R4 = make4(x, z, z, x)
    def xzzy: R4 = make4(x, z, z, y)
    def xzzz: R4 = make4(x, z, z, z)
    def xzzw: R4 = make4(x, z, z, w)
    def xzwx: R4 = make4(x, z, w, x)
    def xzwy: R4 = make4(x, z, w, y)
    def xzwz: R4 = make4(x, z, w, z)
    def xzww: R4 = make4(x, z, w, w)
    def xwxx: R4 = make4(x, w, x, x)
    def xwxy: R4 = make4(x, w, x, y)
    def xwxz: R4 = make4(x, w, x, z)
    def xwxw: R4 = make4(x, w, x, w)
    def xwyx: R4 = make4(x, w, y, x)
    def xwyy: R4 = make4(x, w, y, y)
    def xwyz: R4 = make4(x, w, y, z)
    def xwyw: R4 = make4(x, w, y, w)
    def xwzx: R4 = make4(x, w, z, x)
    def xwzy: R4 = make4(x, w, z, y)
    def xwzz: R4 = make4(x, w, z, z)
    def xwzw: R4 = make4(x, w, z, w)
    def xwwx: R4 = make4(x, w, w, x)
    def xwwy: R4 = make4(x, w, w, y)
    def xwwz: R4 = make4(x, w, w, z)
    def xwww: R4 = make4(x, w, w, w)
    def yxxx: R4 = make4(y, x, x, x)
    def yxxy: R4 = make4(y, x, x, y)
    def yxxz: R4 = make4(y, x, x, z)
    def yxxw: R4 = make4(y, x, x, w)
    def yxyx: R4 = make4(y, x, y, x)
    def yxyy: R4 = make4(y, x, y, y)
    def yxyz: R4 = make4(y, x, y, z)
    def yxyw: R4 = make4(y, x, y, w)
    def yxzx: R4 = make4(y, x, z, x)
    def yxzy: R4 = make4(y, x, z, y)
    def yxzz: R4 = make4(y, x, z, z)
    def yxzw: R4 = make4(y, x, z, w)
    def yxwx: R4 = make4(y, x, w, x)
    def yxwy: R4 = make4(y, x, w, y)
    def yxwz: R4 = make4(y, x, w, z)
    def yxww: R4 = make4(y, x, w, w)
    def yyxx: R4 = make4(y, y, x, x)
    def yyxy: R4 = make4(y, y, x, y)
    def yyxz: R4 = make4(y, y, x, z)
    def yyxw: R4 = make4(y, y, x, w)
    def yyyx: R4 = make4(y, y, y, x)
    def yyyy: R4 = make4(y, y, y, y)
    def yyyz: R4 = make4(y, y, y, z)
    def yyyw: R4 = make4(y, y, y, w)
    def yyzx: R4 = make4(y, y, z, x)
    def yyzy: R4 = make4(y, y, z, y)
    def yyzz: R4 = make4(y, y, z, z)
    def yyzw: R4 = make4(y, y, z, w)
    def yywx: R4 = make4(y, y, w, x)
    def yywy: R4 = make4(y, y, w, y)
    def yywz: R4 = make4(y, y, w, z)
    def yyww: R4 = make4(y, y, w, w)
    def yzxx: R4 = make4(y, z, x, x)
    def yzxy: R4 = make4(y, z, x, y)
    def yzxz: R4 = make4(y, z, x, z)
    def yzxw: R4 = make4(y, z, x, w)
    def yzyx: R4 = make4(y, z, y, x)
    def yzyy: R4 = make4(y, z, y, y)
    def yzyz: R4 = make4(y, z, y, z)
    def yzyw: R4 = make4(y, z, y, w)
    def yzzx: R4 = make4(y, z, z, x)
    def yzzy: R4 = make4(y, z, z, y)
    def yzzz: R4 = make4(y, z, z, z)
    def yzzw: R4 = make4(y, z, z, w)
    def yzwx: R4 = make4(y, z, w, x)
    def yzwy: R4 = make4(y, z, w, y)
    def yzwz: R4 = make4(y, z, w, z)
    def yzww: R4 = make4(y, z, w, w)
    def ywxx: R4 = make4(y, w, x, x)
    def ywxy: R4 = make4(y, w, x, y)
    def ywxz: R4 = make4(y, w, x, z)
    def ywxw: R4 = make4(y, w, x, w)
    def ywyx: R4 = make4(y, w, y, x)
    def ywyy: R4 = make4(y, w, y, y)
    def ywyz: R4 = make4(y, w, y, z)
    def ywyw: R4 = make4(y, w, y, w)
    def ywzx: R4 = make4(y, w, z, x)
    def ywzy: R4 = make4(y, w, z, y)
    def ywzz: R4 = make4(y, w, z, z)
    def ywzw: R4 = make4(y, w, z, w)
    def ywwx: R4 = make4(y, w, w, x)
    def ywwy: R4 = make4(y, w, w, y)
    def ywwz: R4 = make4(y, w, w, z)
    def ywww: R4 = make4(y, w, w, w)
    def zxxx: R4 = make4(z, x, x, x)
    def zxxy: R4 = make4(z, x, x, y)
    def zxxz: R4 = make4(z, x, x, z)
    def zxxw: R4 = make4(z, x, x, w)
    def zxyx: R4 = make4(z, x, y, x)
    def zxyy: R4 = make4(z, x, y, y)
    def zxyz: R4 = make4(z, x, y, z)
    def zxyw: R4 = make4(z, x, y, w)
    def zxzx: R4 = make4(z, x, z, x)
    def zxzy: R4 = make4(z, x, z, y)
    def zxzz: R4 = make4(z, x, z, z)
    def zxzw: R4 = make4(z, x, z, w)
    def zxwx: R4 = make4(z, x, w, x)
    def zxwy: R4 = make4(z, x, w, y)
    def zxwz: R4 = make4(z, x, w, z)
    def zxww: R4 = make4(z, x, w, w)
    def zyxx: R4 = make4(z, y, x, x)
    def zyxy: R4 = make4(z, y, x, y)
    def zyxz: R4 = make4(z, y, x, z)
    def zyxw: R4 = make4(z, y, x, w)
    def zyyx: R4 = make4(z, y, y, x)
    def zyyy: R4 = make4(z, y, y, y)
    def zyyz: R4 = make4(z, y, y, z)
    def zyyw: R4 = make4(z, y, y, w)
    def zyzx: R4 = make4(z, y, z, x)
    def zyzy: R4 = make4(z, y, z, y)
    def zyzz: R4 = make4(z, y, z, z)
    def zyzw: R4 = make4(z, y, z, w)
    def zywx: R4 = make4(z, y, w, x)
    def zywy: R4 = make4(z, y, w, y)
    def zywz: R4 = make4(z, y, w, z)
    def zyww: R4 = make4(z, y, w, w)
    def zzxx: R4 = make4(z, z, x, x)
    def zzxy: R4 = make4(z, z, x, y)
    def zzxz: R4 = make4(z, z, x, z)
    def zzxw: R4 = make4(z, z, x, w)
    def zzyx: R4 = make4(z, z, y, x)
    def zzyy: R4 = make4(z, z, y, y)
    def zzyz: R4 = make4(z, z, y, z)
    def zzyw: R4 = make4(z, z, y, w)
    def zzzx: R4 = make4(z, z, z, x)
    def zzzy: R4 = make4(z, z, z, y)
    def zzzz: R4 = make4(z, z, z, z)
    def zzzw: R4 = make4(z, z, z, w)
    def zzwx: R4 = make4(z, z, w, x)
    def zzwy: R4 = make4(z, z, w, y)
    def zzwz: R4 = make4(z, z, w, z)
    def zzww: R4 = make4(z, z, w, w)
    def zwxx: R4 = make4(z, w, x, x)
    def zwxy: R4 = make4(z, w, x, y)
    def zwxz: R4 = make4(z, w, x, z)
    def zwxw: R4 = make4(z, w, x, w)
    def zwyx: R4 = make4(z, w, y, x)
    def zwyy: R4 = make4(z, w, y, y)
    def zwyz: R4 = make4(z, w, y, z)
    def zwyw: R4 = make4(z, w, y, w)
    def zwzx: R4 = make4(z, w, z, x)
    def zwzy: R4 = make4(z, w, z, y)
    def zwzz: R4 = make4(z, w, z, z)
    def zwzw: R4 = make4(z, w, z, w)
    def zwwx: R4 = make4(z, w, w, x)
    def zwwy: R4 = make4(z, w, w, y)
    def zwwz: R4 = make4(z, w, w, z)
    def zwww: R4 = make4(z, w, w, w)
    def wxxx: R4 = make4(w, x, x, x)
    def wxxy: R4 = make4(w, x, x, y)
    def wxxz: R4 = make4(w, x, x, z)
    def wxxw: R4 = make4(w, x, x, w)
    def wxyx: R4 = make4(w, x, y, x)
    def wxyy: R4 = make4(w, x, y, y)
    def wxyz: R4 = make4(w, x, y, z)
    def wxyw: R4 = make4(w, x, y, w)
    def wxzx: R4 = make4(w, x, z, x)
    def wxzy: R4 = make4(w, x, z, y)
    def wxzz: R4 = make4(w, x, z, z)
    def wxzw: R4 = make4(w, x, z, w)
    def wxwx: R4 = make4(w, x, w, x)
    def wxwy: R4 = make4(w, x, w, y)
    def wxwz: R4 = make4(w, x, w, z)
    def wxww: R4 = make4(w, x, w, w)
    def wyxx: R4 = make4(w, y, x, x)
    def wyxy: R4 = make4(w, y, x, y)
    def wyxz: R4 = make4(w, y, x, z)
    def wyxw: R4 = make4(w, y, x, w)
    def wyyx: R4 = make4(w, y, y, x)
    def wyyy: R4 = make4(w, y, y, y)
    def wyyz: R4 = make4(w, y, y, z)
    def wyyw: R4 = make4(w, y, y, w)
    def wyzx: R4 = make4(w, y, z, x)
    def wyzy: R4 = make4(w, y, z, y)
    def wyzz: R4 = make4(w, y, z, z)
    def wyzw: R4 = make4(w, y, z, w)
    def wywx: R4 = make4(w, y, w, x)
    def wywy: R4 = make4(w, y, w, y)
    def wywz: R4 = make4(w, y, w, z)
    def wyww: R4 = make4(w, y, w, w)
    def wzxx: R4 = make4(w, z, x, x)
    def wzxy: R4 = make4(w, z, x, y)
    def wzxz: R4 = make4(w, z, x, z)
    def wzxw: R4 = make4(w, z, x, w)
    def wzyx: R4 = make4(w, z, y, x)
    def wzyy: R4 = make4(w, z, y, y)
    def wzyz: R4 = make4(w, z, y, z)
    def wzyw: R4 = make4(w, z, y, w)
    def wzzx: R4 = make4(w, z, z, x)
    def wzzy: R4 = make4(w, z, z, y)
    def wzzz: R4 = make4(w, z, z, z)
    def wzzw: R4 = make4(w, z, z, w)
    def wzwx: R4 = make4(w, z, w, x)
    def wzwy: R4 = make4(w, z, w, y)
    def wzwz: R4 = make4(w, z, w, z)
    def wzww: R4 = make4(w, z, w, w)
    def wwxx: R4 = make4(w, w, x, x)
    def wwxy: R4 = make4(w, w, x, y)
    def wwxz: R4 = make4(w, w, x, z)
    def wwxw: R4 = make4(w, w, x, w)
    def wwyx: R4 = make4(w, w, y, x)
    def wwyy: R4 = make4(w, w, y, y)
    def wwyz: R4 = make4(w, w, y, z)
    def wwyw: R4 = make4(w, w, y, w)
    def wwzx: R4 = make4(w, w, z, x)
    def wwzy: R4 = make4(w, w, z, y)
    def wwzz: R4 = make4(w, w, z, z)
    def wwzw: R4 = make4(w, w, z, w)
    def wwwx: R4 = make4(w, w, w, x)
    def wwwy: R4 = make4(w, w, w, y)
    def wwwz: R4 = make4(w, w, w, z)
    def wwww: R4 = make4(w, w, w, w)

    def rr = xx
    def rg = xy
    def rb = xz
    def ra = xw
    def gr = yx
    def gg = yy
    def gb = yz
    def ga = yw
    def br = zx
    def bg = zy
    def bb = zz
    def ba = zw
    def ar = wx
    def ag = wy
    def ab = wz
    def aa = ww

    def rrr = xxx
    def rrg = xxy
    def rrb = xxz
    def rra = xxw
    def rgr = xyx
    def rgg = xyy
    def rgb = xyz
    def rga = xyw
    def rbr = xzx
    def rbg = xzy
    def rbb = xzz
    def rba = xzw
    def rar = xwx
    def rag = xwy
    def rab = xwz
    def raa = xww
    def grr = yxx
    def grg = yxy
    def grb = yxz
    def gra = yxw
    def ggr = yyx
    def ggg = yyy
    def ggb = yyz
    def gga = yyw
    def gbr = yzx
    def gbg = yzy
    def gbb = yzz
    def gba = yzw
    def gar = ywx
    def gag = ywy
    def gab = ywz
    def gaa = yww
    def brr = zxx
    def brg = zxy
    def brb = zxz
    def bra = zxw
    def bgr = zyx
    def bgg = zyy
    def bgb = zyz
    def bga = zyw
    def bbr = zzx
    def bbg = zzy
    def bbb = zzz
    def bba = zzw
    def bar = zwx
    def bag = zwy
    def bab = zwz
    def baa = zww
    def arr = wxx
    def arg = wxy
    def arb = wxz
    def ara = wxw
    def agr = wyx
    def agg = wyy
    def agb = wyz
    def aga = wyw
    def abr = wzx
    def abg = wzy
    def abb = wzz
    def aba = wzw
    def aar = wwx
    def aag = wwy
    def aab = wwz
    def aaa = www

    def rrrr = xxxx
    def rrrg = xxxy
    def rrrb = xxxz
    def rrra = xxxw
    def rrgr = xxyx
    def rrgg = xxyy
    def rrgb = xxyz
    def rrga = xxyw
    def rrbr = xxzx
    def rrbg = xxzy
    def rrbb = xxzz
    def rrba = xxzw
    def rrar = xxwx
    def rrag = xxwy
    def rrab = xxwz
    def rraa = xxww
    def rgrr = xyxx
    def rgrg = xyxy
    def rgrb = xyxz
    def rgra = xyxw
    def rggr = xyyx
    def rggg = xyyy
    def rggb = xyyz
    def rgga = xyyw
    def rgbr = xyzx
    def rgbg = xyzy
    def rgbb = xyzz
    def rgba = xyzw
    def rgar = xywx
    def rgag = xywy
    def rgab = xywz
    def rgaa = xyww
    def rbrr = xzxx
    def rbrg = xzxy
    def rbrb = xzxz
    def rbra = xzxw
    def rbgr = xzyx
    def rbgg = xzyy
    def rbgb = xzyz
    def rbga = xzyw
    def rbbr = xzzx
    def rbbg = xzzy
    def rbbb = xzzz
    def rbba = xzzw
    def rbar = xzwx
    def rbag = xzwy
    def rbab = xzwz
    def rbaa = xzww
    def rarr = xwxx
    def rarg = xwxy
    def rarb = xwxz
    def rara = xwxw
    def ragr = xwyx
    def ragg = xwyy
    def ragb = xwyz
    def raga = xwyw
    def rabr = xwzx
    def rabg = xwzy
    def rabb = xwzz
    def raba = xwzw
    def raar = xwwx
    def raag = xwwy
    def raab = xwwz
    def raaa = xwww
    def grrr = yxxx
    def grrg = yxxy
    def grrb = yxxz
    def grra = yxxw
    def grgr = yxyx
    def grgg = yxyy
    def grgb = yxyz
    def grga = yxyw
    def grbr = yxzx
    def grbg = yxzy
    def grbb = yxzz
    def grba = yxzw
    def grar = yxwx
    def grag = yxwy
    def grab = yxwz
    def graa = yxww
    def ggrr = yyxx
    def ggrg = yyxy
    def ggrb = yyxz
    def ggra = yyxw
    def gggr = yyyx
    def gggg = yyyy
    def gggb = yyyz
    def ggga = yyyw
    def ggbr = yyzx
    def ggbg = yyzy
    def ggbb = yyzz
    def ggba = yyzw
    def ggar = yywx
    def ggag = yywy
    def ggab = yywz
    def ggaa = yyww
    def gbrr = yzxx
    def gbrg = yzxy
    def gbrb = yzxz
    def gbra = yzxw
    def gbgr = yzyx
    def gbgg = yzyy
    def gbgb = yzyz
    def gbga = yzyw
    def gbbr = yzzx
    def gbbg = yzzy
    def gbbb = yzzz
    def gbba = yzzw
    def gbar = yzwx
    def gbag = yzwy
    def gbab = yzwz
    def gbaa = yzww
    def garr = ywxx
    def garg = ywxy
    def garb = ywxz
    def gara = ywxw
    def gagr = ywyx
    def gagg = ywyy
    def gagb = ywyz
    def gaga = ywyw
    def gabr = ywzx
    def gabg = ywzy
    def gabb = ywzz
    def gaba = ywzw
    def gaar = ywwx
    def gaag = ywwy
    def gaab = ywwz
    def gaaa = ywww
    def brrr = zxxx
    def brrg = zxxy
    def brrb = zxxz
    def brra = zxxw
    def brgr = zxyx
    def brgg = zxyy
    def brgb = zxyz
    def brga = zxyw
    def brbr = zxzx
    def brbg = zxzy
    def brbb = zxzz
    def brba = zxzw
    def brar = zxwx
    def brag = zxwy
    def brab = zxwz
    def braa = zxww
    def bgrr = zyxx
    def bgrg = zyxy
    def bgrb = zyxz
    def bgra = zyxw
    def bggr = zyyx
    def bggg = zyyy
    def bggb = zyyz
    def bgga = zyyw
    def bgbr = zyzx
    def bgbg = zyzy
    def bgbb = zyzz
    def bgba = zyzw
    def bgar = zywx
    def bgag = zywy
    def bgab = zywz
    def bgaa = zyww
    def bbrr = zzxx
    def bbrg = zzxy
    def bbrb = zzxz
    def bbra = zzxw
    def bbgr = zzyx
    def bbgg = zzyy
    def bbgb = zzyz
    def bbga = zzyw
    def bbbr = zzzx
    def bbbg = zzzy
    def bbbb = zzzz
    def bbba = zzzw
    def bbar = zzwx
    def bbag = zzwy
    def bbab = zzwz
    def bbaa = zzww
    def barr = zwxx
    def barg = zwxy
    def barb = zwxz
    def bara = zwxw
    def bagr = zwyx
    def bagg = zwyy
    def bagb = zwyz
    def baga = zwyw
    def babr = zwzx
    def babg = zwzy
    def babb = zwzz
    def baba = zwzw
    def baar = zwwx
    def baag = zwwy
    def baab = zwwz
    def baaa = zwww
    def arrr = wxxx
    def arrg = wxxy
    def arrb = wxxz
    def arra = wxxw
    def argr = wxyx
    def argg = wxyy
    def argb = wxyz
    def arga = wxyw
    def arbr = wxzx
    def arbg = wxzy
    def arbb = wxzz
    def arba = wxzw
    def arar = wxwx
    def arag = wxwy
    def arab = wxwz
    def araa = wxww
    def agrr = wyxx
    def agrg = wyxy
    def agrb = wyxz
    def agra = wyxw
    def aggr = wyyx
    def aggg = wyyy
    def aggb = wyyz
    def agga = wyyw
    def agbr = wyzx
    def agbg = wyzy
    def agbb = wyzz
    def agba = wyzw
    def agar = wywx
    def agag = wywy
    def agab = wywz
    def agaa = wyww
    def abrr = wzxx
    def abrg = wzxy
    def abrb = wzxz
    def abra = wzxw
    def abgr = wzyx
    def abgg = wzyy
    def abgb = wzyz
    def abga = wzyw
    def abbr = wzzx
    def abbg = wzzy
    def abbb = wzzz
    def abba = wzzw
    def abar = wzwx
    def abag = wzwy
    def abab = wzwz
    def abaa = wzww
    def aarr = wwxx
    def aarg = wwxy
    def aarb = wwxz
    def aara = wwxw
    def aagr = wwyx
    def aagg = wwyy
    def aagb = wwyz
    def aaga = wwyw
    def aabr = wwzx
    def aabg = wwzy
    def aabb = wwzz
    def aaba = wwzw
    def aaar = wwwx
    def aaag = wwwy
    def aaab = wwwz
    def aaaa = wwww

    def ss = xx
    def st = xy
    def sp = xz
    def sq = xw
    def ts = yx
    def tt = yy
    def tp = yz
    def tq = yw
    def ps = zx
    def pt = zy
    def pp = zz
    def pq = zw
    def qs = wx
    def qt = wy
    def qp = wz
    def qq = ww

    def sss = xxx
    def sst = xxy
    def ssp = xxz
    def ssq = xxw
    def sts = xyx
    def stt = xyy
    def stp = xyz
    def stq = xyw
    def sps = xzx
    def spt = xzy
    def spp = xzz
    def spq = xzw
    def sqs = xwx
    def sqt = xwy
    def sqp = xwz
    def sqq = xww
    def tss = yxx
    def tst = yxy
    def tsp = yxz
    def tsq = yxw
    def tts = yyx
    def ttt = yyy
    def ttp = yyz
    def ttq = yyw
    def tps = yzx
    def tpt = yzy
    def tpp = yzz
    def tpq = yzw
    def tqs = ywx
    def tqt = ywy
    def tqp = ywz
    def tqq = yww
    def pss = zxx
    def pst = zxy
    def psp = zxz
    def psq = zxw
    def pts = zyx
    def ptt = zyy
    def ptp = zyz
    def ptq = zyw
    def pps = zzx
    def ppt = zzy
    def ppp = zzz
    def ppq = zzw
    def pqs = zwx
    def pqt = zwy
    def pqp = zwz
    def pqq = zww
    def qss = wxx
    def qst = wxy
    def qsp = wxz
    def qsq = wxw
    def qts = wyx
    def qtt = wyy
    def qtp = wyz
    def qtq = wyw
    def qps = wzx
    def qpt = wzy
    def qpp = wzz
    def qpq = wzw
    def qqs = wwx
    def qqt = wwy
    def qqp = wwz
    def qqq = www

    def ssss = xxxx
    def ssst = xxxy
    def sssp = xxxz
    def sssq = xxxw
    def ssts = xxyx
    def sstt = xxyy
    def sstp = xxyz
    def sstq = xxyw
    def ssps = xxzx
    def sspt = xxzy
    def sspp = xxzz
    def sspq = xxzw
    def ssqs = xxwx
    def ssqt = xxwy
    def ssqp = xxwz
    def ssqq = xxww
    def stss = xyxx
    def stst = xyxy
    def stsp = xyxz
    def stsq = xyxw
    def stts = xyyx
    def sttt = xyyy
    def sttp = xyyz
    def sttq = xyyw
    def stps = xyzx
    def stpt = xyzy
    def stpp = xyzz
    def stpq = xyzw
    def stqs = xywx
    def stqt = xywy
    def stqp = xywz
    def stqq = xyww
    def spss = xzxx
    def spst = xzxy
    def spsp = xzxz
    def spsq = xzxw
    def spts = xzyx
    def sptt = xzyy
    def sptp = xzyz
    def sptq = xzyw
    def spps = xzzx
    def sppt = xzzy
    def sppp = xzzz
    def sppq = xzzw
    def spqs = xzwx
    def spqt = xzwy
    def spqp = xzwz
    def spqq = xzww
    def sqss = xwxx
    def sqst = xwxy
    def sqsp = xwxz
    def sqsq = xwxw
    def sqts = xwyx
    def sqtt = xwyy
    def sqtp = xwyz
    def sqtq = xwyw
    def sqps = xwzx
    def sqpt = xwzy
    def sqpp = xwzz
    def sqpq = xwzw
    def sqqs = xwwx
    def sqqt = xwwy
    def sqqp = xwwz
    def sqqq = xwww
    def tsss = yxxx
    def tsst = yxxy
    def tssp = yxxz
    def tssq = yxxw
    def tsts = yxyx
    def tstt = yxyy
    def tstp = yxyz
    def tstq = yxyw
    def tsps = yxzx
    def tspt = yxzy
    def tspp = yxzz
    def tspq = yxzw
    def tsqs = yxwx
    def tsqt = yxwy
    def tsqp = yxwz
    def tsqq = yxww
    def ttss = yyxx
    def ttst = yyxy
    def ttsp = yyxz
    def ttsq = yyxw
    def ttts = yyyx
    def tttt = yyyy
    def tttp = yyyz
    def tttq = yyyw
    def ttps = yyzx
    def ttpt = yyzy
    def ttpp = yyzz
    def ttpq = yyzw
    def ttqs = yywx
    def ttqt = yywy
    def ttqp = yywz
    def ttqq = yyww
    def tpss = yzxx
    def tpst = yzxy
    def tpsp = yzxz
    def tpsq = yzxw
    def tpts = yzyx
    def tptt = yzyy
    def tptp = yzyz
    def tptq = yzyw
    def tpps = yzzx
    def tppt = yzzy
    def tppp = yzzz
    def tppq = yzzw
    def tpqs = yzwx
    def tpqt = yzwy
    def tpqp = yzwz
    def tpqq = yzww
    def tqss = ywxx
    def tqst = ywxy
    def tqsp = ywxz
    def tqsq = ywxw
    def tqts = ywyx
    def tqtt = ywyy
    def tqtp = ywyz
    def tqtq = ywyw
    def tqps = ywzx
    def tqpt = ywzy
    def tqpp = ywzz
    def tqpq = ywzw
    def tqqs = ywwx
    def tqqt = ywwy
    def tqqp = ywwz
    def tqqq = ywww
    def psss = zxxx
    def psst = zxxy
    def pssp = zxxz
    def pssq = zxxw
    def psts = zxyx
    def pstt = zxyy
    def pstp = zxyz
    def pstq = zxyw
    def psps = zxzx
    def pspt = zxzy
    def pspp = zxzz
    def pspq = zxzw
    def psqs = zxwx
    def psqt = zxwy
    def psqp = zxwz
    def psqq = zxww
    def ptss = zyxx
    def ptst = zyxy
    def ptsp = zyxz
    def ptsq = zyxw
    def ptts = zyyx
    def pttt = zyyy
    def pttp = zyyz
    def pttq = zyyw
    def ptps = zyzx
    def ptpt = zyzy
    def ptpp = zyzz
    def ptpq = zyzw
    def ptqs = zywx
    def ptqt = zywy
    def ptqp = zywz
    def ptqq = zyww
    def ppss = zzxx
    def ppst = zzxy
    def ppsp = zzxz
    def ppsq = zzxw
    def ppts = zzyx
    def pptt = zzyy
    def pptp = zzyz
    def pptq = zzyw
    def ppps = zzzx
    def pppt = zzzy
    def pppp = zzzz
    def pppq = zzzw
    def ppqs = zzwx
    def ppqt = zzwy
    def ppqp = zzwz
    def ppqq = zzww
    def pqss = zwxx
    def pqst = zwxy
    def pqsp = zwxz
    def pqsq = zwxw
    def pqts = zwyx
    def pqtt = zwyy
    def pqtp = zwyz
    def pqtq = zwyw
    def pqps = zwzx
    def pqpt = zwzy
    def pqpp = zwzz
    def pqpq = zwzw
    def pqqs = zwwx
    def pqqt = zwwy
    def pqqp = zwwz
    def pqqq = zwww
    def qsss = wxxx
    def qsst = wxxy
    def qssp = wxxz
    def qssq = wxxw
    def qsts = wxyx
    def qstt = wxyy
    def qstp = wxyz
    def qstq = wxyw
    def qsps = wxzx
    def qspt = wxzy
    def qspp = wxzz
    def qspq = wxzw
    def qsqs = wxwx
    def qsqt = wxwy
    def qsqp = wxwz
    def qsqq = wxww
    def qtss = wyxx
    def qtst = wyxy
    def qtsp = wyxz
    def qtsq = wyxw
    def qtts = wyyx
    def qttt = wyyy
    def qttp = wyyz
    def qttq = wyyw
    def qtps = wyzx
    def qtpt = wyzy
    def qtpp = wyzz
    def qtpq = wyzw
    def qtqs = wywx
    def qtqt = wywy
    def qtqp = wywz
    def qtqq = wyww
    def qpss = wzxx
    def qpst = wzxy
    def qpsp = wzxz
    def qpsq = wzxw
    def qpts = wzyx
    def qptt = wzyy
    def qptp = wzyz
    def qptq = wzyw
    def qpps = wzzx
    def qppt = wzzy
    def qppp = wzzz
    def qppq = wzzw
    def qpqs = wzwx
    def qpqt = wzwy
    def qpqp = wzwz
    def qpqq = wzww
    def qqss = wwxx
    def qqst = wwxy
    def qqsp = wwxz
    def qqsq = wwxw
    def qqts = wwyx
    def qqtt = wwyy
    def qqtp = wwyz
    def qqtq = wwyw
    def qqps = wwzx
    def qqpt = wwzy
    def qqpp = wwzz
    def qqpq = wwzw
    def qqqs = wwwx
    def qqqt = wwwy
    def qqqp = wwwz
    def qqqq = wwww
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
protected trait Swizzle4Write[P, R2, R3, R4]
extends Swizzle4Read[P, R2, R3, R4]
{
    override def x: P
    override def y: P
    override def z: P
    override def w: P

    override def r = x
    override def g = y
    override def b = z
    override def a = w

    override def s = x
    override def t = y
    override def p = z
    override def q = w

    def x_=(x: P) :Unit
    def y_=(y: P) :Unit
    def z_=(z: P) :Unit
    def w_=(z: P) :Unit

    def r_=(r: P) { x = r }
    def g_=(g: P) { y = g }
    def b_=(b: P) { z = b }
    def a_=(a: P) { w = a }

    def s_=(s: P) { x = s }
    def t_=(t: P) { y = t }
    def p_=(p: P) { z = p }
    def q_=(q: P) { w = q }


    override def xy: R2 = make2(x, y)
    override def xz: R2 = make2(x, z)
    override def xw: R2 = make2(x, w)
    override def yx: R2 = make2(y, x)
    override def yz: R2 = make2(y, z)
    override def yw: R2 = make2(y, w)
    override def zx: R2 = make2(z, x)
    override def zy: R2 = make2(z, y)
    override def zw: R2 = make2(z, w)
    override def wx: R2 = make2(w, x)
    override def wy: R2 = make2(w, y)
    override def wz: R2 = make2(w, z)

    override def xyz: R3 = make3(x, y, z)
    override def xyw: R3 = make3(x, y, w)
    override def xzy: R3 = make3(x, z, y)
    override def xzw: R3 = make3(x, z, w)
    override def xwy: R3 = make3(x, w, y)
    override def xwz: R3 = make3(x, w, z)
    override def yxz: R3 = make3(y, x, z)
    override def yxw: R3 = make3(y, x, w)
    override def yzx: R3 = make3(y, z, x)
    override def yzw: R3 = make3(y, z, w)
    override def ywx: R3 = make3(y, w, x)
    override def ywz: R3 = make3(y, w, z)
    override def zxy: R3 = make3(z, x, y)
    override def zxw: R3 = make3(z, x, w)
    override def zyx: R3 = make3(z, y, x)
    override def zyw: R3 = make3(z, y, w)
    override def zwx: R3 = make3(z, w, x)
    override def zwy: R3 = make3(z, w, y)
    override def wxy: R3 = make3(w, x, y)
    override def wxz: R3 = make3(w, x, z)
    override def wyx: R3 = make3(w, y, x)
    override def wyz: R3 = make3(w, y, z)
    override def wzx: R3 = make3(w, z, x)
    override def wzy: R3 = make3(w, z, y)

    override def xyzw: R4 = make4(x, y, z, w)
    override def xywz: R4 = make4(x, y, w, z)
    override def xzyw: R4 = make4(x, z, y, w)
    override def xzwy: R4 = make4(x, z, w, y)
    override def xwyz: R4 = make4(x, w, y, z)
    override def xwzy: R4 = make4(x, w, z, y)
    override def yxzw: R4 = make4(y, x, z, w)
    override def yxwz: R4 = make4(y, x, w, z)
    override def yzxw: R4 = make4(y, z, x, w)
    override def yzwx: R4 = make4(y, z, w, x)
    override def ywxz: R4 = make4(y, w, x, z)
    override def ywzx: R4 = make4(y, w, z, x)
    override def zxyw: R4 = make4(z, x, y, w)
    override def zxwy: R4 = make4(z, x, w, y)
    override def zyxw: R4 = make4(z, y, x, w)
    override def zywx: R4 = make4(z, y, w, x)
    override def zwxy: R4 = make4(z, w, x, y)
    override def zwyx: R4 = make4(z, w, y, x)
    override def wxyz: R4 = make4(w, x, y, z)
    override def wxzy: R4 = make4(w, x, z, y)
    override def wyxz: R4 = make4(w, y, x, z)
    override def wyzx: R4 = make4(w, y, z, x)
    override def wzxy: R4 = make4(w, z, x, y)
    override def wzyx: R4 = make4(w, z, y, x)

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


    def xy_=(u: Read2[P]) { x = u.x; y = u.y }
    def xz_=(u: Read2[P]) { x = u.x; z = u.y }
    def xw_=(u: Read2[P]) { x = u.x; w = u.y }
    def yx_=(u: Read2[P]) { y = u.x; x = u.y }
    def yz_=(u: Read2[P]) { y = u.x; z = u.y }
    def yw_=(u: Read2[P]) { y = u.x; w = u.y }
    def zx_=(u: Read2[P]) { z = u.x; x = u.y }
    def zy_=(u: Read2[P]) { z = u.x; y = u.y }
    def zw_=(u: Read2[P]) { z = u.x; w = u.y }
    def wx_=(u: Read2[P]) { w = u.x; x = u.y }
    def wy_=(u: Read2[P]) { w = u.x; y = u.y }
    def wz_=(u: Read2[P]) { w = u.x; z = u.y }

    def xyz_=(u: Read3[P]) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: Read3[P]) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: Read3[P]) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: Read3[P]) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: Read3[P]) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: Read3[P]) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: Read3[P]) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: Read3[P]) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: Read3[P]) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: Read3[P]) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: Read3[P]) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: Read3[P]) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: Read3[P]) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: Read3[P]) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: Read3[P]) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: Read3[P]) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: Read3[P]) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: Read3[P]) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: Read3[P]) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: Read3[P]) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: Read3[P]) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: Read3[P]) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: Read3[P]) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: Read3[P]) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: Read4[P]) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: Read4[P]) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: Read4[P]) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: Read4[P]) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: Read4[P]) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: Read4[P]) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: Read4[P]) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: Read4[P]) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z = t }
    def yzxw_=(u: Read4[P]) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: Read4[P]) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: Read4[P]) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: Read4[P]) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: Read4[P]) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: Read4[P]) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: Read4[P]) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: Read4[P]) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: Read4[P]) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y = t }
    def zwyx_=(u: Read4[P]) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: Read4[P]) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: Read4[P]) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: Read4[P]) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: Read4[P]) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: Read4[P]) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: Read4[P]) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y = t }

    def rg_=(u: Read2[P]) { xy_=(u) }
    def rb_=(u: Read2[P]) { xz_=(u) }
    def ra_=(u: Read2[P]) { xw_=(u) }
    def gr_=(u: Read2[P]) { yx_=(u) }
    def gb_=(u: Read2[P]) { yz_=(u) }
    def ga_=(u: Read2[P]) { yw_=(u) }
    def br_=(u: Read2[P]) { zx_=(u) }
    def bg_=(u: Read2[P]) { zy_=(u) }
    def ba_=(u: Read2[P]) { zw_=(u) }
    def ar_=(u: Read2[P]) { wx_=(u) }
    def ag_=(u: Read2[P]) { wy_=(u) }
    def ab_=(u: Read2[P]) { wz_=(u) }

    def rgb_=(u: Read3[P]) { xyz_=(u) }
    def rga_=(u: Read3[P]) { xyw_=(u) }
    def rbg_=(u: Read3[P]) { xzy_=(u) }
    def rba_=(u: Read3[P]) { xzw_=(u) }
    def rag_=(u: Read3[P]) { xwy_=(u) }
    def rab_=(u: Read3[P]) { xwz_=(u) }
    def grb_=(u: Read3[P]) { yxz_=(u) }
    def gra_=(u: Read3[P]) { yxw_=(u) }
    def gbr_=(u: Read3[P]) { yzx_=(u) }
    def gba_=(u: Read3[P]) { yzw_=(u) }
    def gar_=(u: Read3[P]) { ywx_=(u) }
    def gab_=(u: Read3[P]) { ywz_=(u) }
    def brg_=(u: Read3[P]) { zxy_=(u) }
    def bra_=(u: Read3[P]) { zxw_=(u) }
    def bgr_=(u: Read3[P]) { zyx_=(u) }
    def bga_=(u: Read3[P]) { zyw_=(u) }
    def bar_=(u: Read3[P]) { zwx_=(u) }
    def bag_=(u: Read3[P]) { zwy_=(u) }
    def arg_=(u: Read3[P]) { wxy_=(u) }
    def arb_=(u: Read3[P]) { wxz_=(u) }
    def agr_=(u: Read3[P]) { wyx_=(u) }
    def agb_=(u: Read3[P]) { wyz_=(u) }
    def abr_=(u: Read3[P]) { wzx_=(u) }
    def abg_=(u: Read3[P]) { wzy_=(u) }

    def rgba_=(u: Read4[P]) { xyzw_=(u) }
    def rgab_=(u: Read4[P]) { xywz_=(u) }
    def rbga_=(u: Read4[P]) { xzyw_=(u) }
    def rbag_=(u: Read4[P]) { xzwy_=(u) }
    def ragb_=(u: Read4[P]) { xwyz_=(u) }
    def rabg_=(u: Read4[P]) { xwzy_=(u) }
    def grba_=(u: Read4[P]) { yxzw_=(u) }
    def grab_=(u: Read4[P]) { yxwz_=(u) }
    def gbra_=(u: Read4[P]) { yzxw_=(u) }
    def gbar_=(u: Read4[P]) { yzwx_=(u) }
    def garb_=(u: Read4[P]) { ywxz_=(u) }
    def gabr_=(u: Read4[P]) { ywzx_=(u) }
    def brga_=(u: Read4[P]) { zxyw_=(u) }
    def brag_=(u: Read4[P]) { zxwy_=(u) }
    def bgra_=(u: Read4[P]) { zyxw_=(u) }
    def bgar_=(u: Read4[P]) { zywx_=(u) }
    def barg_=(u: Read4[P]) { zwxy_=(u) }
    def bagr_=(u: Read4[P]) { zwyx_=(u) }
    def argb_=(u: Read4[P]) { wxyz_=(u) }
    def arbg_=(u: Read4[P]) { wxzy_=(u) }
    def agrb_=(u: Read4[P]) { wyxz_=(u) }
    def agbr_=(u: Read4[P]) { wyzx_=(u) }
    def abrg_=(u: Read4[P]) { wzxy_=(u) }
    def abgr_=(u: Read4[P]) { wzyx_=(u) }

    def st_=(u: Read2[P]) { xy_=(u) }
    def sp_=(u: Read2[P]) { xz_=(u) }
    def sq_=(u: Read2[P]) { xw_=(u) }
    def ts_=(u: Read2[P]) { yx_=(u) }
    def tp_=(u: Read2[P]) { yz_=(u) }
    def tq_=(u: Read2[P]) { yw_=(u) }
    def ps_=(u: Read2[P]) { zx_=(u) }
    def pt_=(u: Read2[P]) { zy_=(u) }
    def pq_=(u: Read2[P]) { zw_=(u) }
    def qs_=(u: Read2[P]) { wx_=(u) }
    def qt_=(u: Read2[P]) { wy_=(u) }
    def qp_=(u: Read2[P]) { wz_=(u) }

    def stp_=(u: Read3[P]) { xyz_=(u) }
    def stq_=(u: Read3[P]) { xyw_=(u) }
    def spt_=(u: Read3[P]) { xzy_=(u) }
    def spq_=(u: Read3[P]) { xzw_=(u) }
    def sqt_=(u: Read3[P]) { xwy_=(u) }
    def sqp_=(u: Read3[P]) { xwz_=(u) }
    def tsp_=(u: Read3[P]) { yxz_=(u) }
    def tsq_=(u: Read3[P]) { yxw_=(u) }
    def tps_=(u: Read3[P]) { yzx_=(u) }
    def tpq_=(u: Read3[P]) { yzw_=(u) }
    def tqs_=(u: Read3[P]) { ywx_=(u) }
    def tqp_=(u: Read3[P]) { ywz_=(u) }
    def pst_=(u: Read3[P]) { zxy_=(u) }
    def psq_=(u: Read3[P]) { zxw_=(u) }
    def pts_=(u: Read3[P]) { zyx_=(u) }
    def ptq_=(u: Read3[P]) { zyw_=(u) }
    def pqs_=(u: Read3[P]) { zwx_=(u) }
    def pqt_=(u: Read3[P]) { zwy_=(u) }
    def qst_=(u: Read3[P]) { wxy_=(u) }
    def qsp_=(u: Read3[P]) { wxz_=(u) }
    def qts_=(u: Read3[P]) { wyx_=(u) }
    def qtp_=(u: Read3[P]) { wyz_=(u) }
    def qps_=(u: Read3[P]) { wzx_=(u) }
    def qpt_=(u: Read3[P]) { wzy_=(u) }

    def stpq_=(u: Read4[P]) { xyzw_=(u) }
    def stqp_=(u: Read4[P]) { xywz_=(u) }
    def sptq_=(u: Read4[P]) { xzyw_=(u) }
    def spqt_=(u: Read4[P]) { xzwy_=(u) }
    def sqtp_=(u: Read4[P]) { xwyz_=(u) }
    def sqpt_=(u: Read4[P]) { xwzy_=(u) }
    def tspq_=(u: Read4[P]) { yxzw_=(u) }
    def tsqp_=(u: Read4[P]) { yxwz_=(u) }
    def tpsq_=(u: Read4[P]) { yzxw_=(u) }
    def tpqs_=(u: Read4[P]) { yzwx_=(u) }
    def tqsp_=(u: Read4[P]) { ywxz_=(u) }
    def tqps_=(u: Read4[P]) { ywzx_=(u) }
    def pstq_=(u: Read4[P]) { zxyw_=(u) }
    def psqt_=(u: Read4[P]) { zxwy_=(u) }
    def ptsq_=(u: Read4[P]) { zyxw_=(u) }
    def ptqs_=(u: Read4[P]) { zywx_=(u) }
    def pqst_=(u: Read4[P]) { zwxy_=(u) }
    def pqts_=(u: Read4[P]) { zwyx_=(u) }
    def qstp_=(u: Read4[P]) { wxyz_=(u) }
    def qspt_=(u: Read4[P]) { wxzy_=(u) }
    def qtsp_=(u: Read4[P]) { wyxz_=(u) }
    def qtps_=(u: Read4[P]) { wyzx_=(u) }
    def qpst_=(u: Read4[P]) { wzxy_=(u) }
    def qpts_=(u: Read4[P]) { wzyx_=(u) }
}
