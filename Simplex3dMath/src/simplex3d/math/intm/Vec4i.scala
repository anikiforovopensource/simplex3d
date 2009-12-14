/*
 * Simplex3D, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4i extends Read4Int {

    def r = x
    def g = y
    def b = z
    def a = w


    def apply(i: Int) :Int = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = new Vec4i(-x, -y, -z, -w)
    def unary_~() = new Vec4i(~x, ~y, ~z, ~w)

    def *(s: Int) = new Vec4i(x * s, y * s, z * s, w * s)
    def /(s: Int) = new Vec4i(x / s, y / s, z / s, w / s)
    private[math] def divideByComponent(s: Int) = {
        new Vec4i(s / x, s / y, s / z, s / w)
    }
    def %(s: Int) = new Vec4i(x % s, y % s, z % s, w % s)
    private[math] def modByComponent(s: Int) = new Vec4i(s%x, s%y, s%z, s%w)
    def >>(s: Int) = new Vec4i( x >> s, y >> s, z >> s, w >> s)
    def >>>(s: Int) = new Vec4i( x >>> s, y >>> s, z >>> s, w >>> s)
    def <<(s: Int) = new Vec4i( x << s, y << s, z << s, w << s)
    def &(s: Int) = new Vec4i( x & s, y & s, z & s, w & s)
    def |(s: Int) = new Vec4i( x | s, y | s, z | s, w | s)
    def ^(s: Int) = new Vec4i( x ^ s, y ^ s, z ^ s, w ^ s)

    def +(u: AnyVec4i) = new Vec4i(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4i) = new Vec4i(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4i) = new Vec4i(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4i) = new Vec4i(x / u.x, y / u.y, z / u.z, w / u.w)
    def %(u: AnyVec4i) = new Vec4i(x % u.x, y % u.y, z % u.z, w % u.w)
    def >>(u: AnyVec4i) = new Vec4i( x >> u.x, y >> u.y, z >> u.z, w >> u.w)
    def >>>(u: AnyVec4i) = new Vec4i( x >>> u.x, y >>> u.y, z >>> u.z, w >>> u.w)
    def <<(u: AnyVec4i) = new Vec4i( x << u.x, y << u.y, z << u.z, w << u.w)
    def &(u: AnyVec4i) = new Vec4i( x & u.x, y & u.y, z & u.z, w & u.w)
    def |(u: AnyVec4i) = new Vec4i( x | u.x, y | u.y, z | u.z, w | u.w)
    def ^(u: AnyVec4i) = new Vec4i( x ^ u.x, y ^ u.y, z ^ u.z, w ^ u.w)

    def ==(u: AnyVec4i) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4i) :Boolean = !(this == u)
    
    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }

    // Swizzling
    def xx: ConstVec2i = new ConstVec2i(x, x)
    def xy: ConstVec2i = new ConstVec2i(x, y)
    def xz: ConstVec2i = new ConstVec2i(x, z)
    def xw: ConstVec2i = new ConstVec2i(x, w)
    def yx: ConstVec2i = new ConstVec2i(y, x)
    def yy: ConstVec2i = new ConstVec2i(y, y)
    def yz: ConstVec2i = new ConstVec2i(y, z)
    def yw: ConstVec2i = new ConstVec2i(y, w)
    def zx: ConstVec2i = new ConstVec2i(z, x)
    def zy: ConstVec2i = new ConstVec2i(z, y)
    def zz: ConstVec2i = new ConstVec2i(z, z)
    def zw: ConstVec2i = new ConstVec2i(z, w)
    def wx: ConstVec2i = new ConstVec2i(w, x)
    def wy: ConstVec2i = new ConstVec2i(w, y)
    def wz: ConstVec2i = new ConstVec2i(w, z)
    def ww: ConstVec2i = new ConstVec2i(w, w)

    def xxx: ConstVec3i = new ConstVec3i(x, x, x)
    def xxy: ConstVec3i = new ConstVec3i(x, x, y)
    def xxz: ConstVec3i = new ConstVec3i(x, x, z)
    def xxw: ConstVec3i = new ConstVec3i(x, x, w)
    def xyx: ConstVec3i = new ConstVec3i(x, y, x)
    def xyy: ConstVec3i = new ConstVec3i(x, y, y)
    def xyz: ConstVec3i = new ConstVec3i(x, y, z)
    def xyw: ConstVec3i = new ConstVec3i(x, y, w)
    def xzx: ConstVec3i = new ConstVec3i(x, z, x)
    def xzy: ConstVec3i = new ConstVec3i(x, z, y)
    def xzz: ConstVec3i = new ConstVec3i(x, z, z)
    def xzw: ConstVec3i = new ConstVec3i(x, z, w)
    def xwx: ConstVec3i = new ConstVec3i(x, w, x)
    def xwy: ConstVec3i = new ConstVec3i(x, w, y)
    def xwz: ConstVec3i = new ConstVec3i(x, w, z)
    def xww: ConstVec3i = new ConstVec3i(x, w, w)
    def yxx: ConstVec3i = new ConstVec3i(y, x, x)
    def yxy: ConstVec3i = new ConstVec3i(y, x, y)
    def yxz: ConstVec3i = new ConstVec3i(y, x, z)
    def yxw: ConstVec3i = new ConstVec3i(y, x, w)
    def yyx: ConstVec3i = new ConstVec3i(y, y, x)
    def yyy: ConstVec3i = new ConstVec3i(y, y, y)
    def yyz: ConstVec3i = new ConstVec3i(y, y, z)
    def yyw: ConstVec3i = new ConstVec3i(y, y, w)
    def yzx: ConstVec3i = new ConstVec3i(y, z, x)
    def yzy: ConstVec3i = new ConstVec3i(y, z, y)
    def yzz: ConstVec3i = new ConstVec3i(y, z, z)
    def yzw: ConstVec3i = new ConstVec3i(y, z, w)
    def ywx: ConstVec3i = new ConstVec3i(y, w, x)
    def ywy: ConstVec3i = new ConstVec3i(y, w, y)
    def ywz: ConstVec3i = new ConstVec3i(y, w, z)
    def yww: ConstVec3i = new ConstVec3i(y, w, w)
    def zxx: ConstVec3i = new ConstVec3i(z, x, x)
    def zxy: ConstVec3i = new ConstVec3i(z, x, y)
    def zxz: ConstVec3i = new ConstVec3i(z, x, z)
    def zxw: ConstVec3i = new ConstVec3i(z, x, w)
    def zyx: ConstVec3i = new ConstVec3i(z, y, x)
    def zyy: ConstVec3i = new ConstVec3i(z, y, y)
    def zyz: ConstVec3i = new ConstVec3i(z, y, z)
    def zyw: ConstVec3i = new ConstVec3i(z, y, w)
    def zzx: ConstVec3i = new ConstVec3i(z, z, x)
    def zzy: ConstVec3i = new ConstVec3i(z, z, y)
    def zzz: ConstVec3i = new ConstVec3i(z, z, z)
    def zzw: ConstVec3i = new ConstVec3i(z, z, w)
    def zwx: ConstVec3i = new ConstVec3i(z, w, x)
    def zwy: ConstVec3i = new ConstVec3i(z, w, y)
    def zwz: ConstVec3i = new ConstVec3i(z, w, z)
    def zww: ConstVec3i = new ConstVec3i(z, w, w)
    def wxx: ConstVec3i = new ConstVec3i(w, x, x)
    def wxy: ConstVec3i = new ConstVec3i(w, x, y)
    def wxz: ConstVec3i = new ConstVec3i(w, x, z)
    def wxw: ConstVec3i = new ConstVec3i(w, x, w)
    def wyx: ConstVec3i = new ConstVec3i(w, y, x)
    def wyy: ConstVec3i = new ConstVec3i(w, y, y)
    def wyz: ConstVec3i = new ConstVec3i(w, y, z)
    def wyw: ConstVec3i = new ConstVec3i(w, y, w)
    def wzx: ConstVec3i = new ConstVec3i(w, z, x)
    def wzy: ConstVec3i = new ConstVec3i(w, z, y)
    def wzz: ConstVec3i = new ConstVec3i(w, z, z)
    def wzw: ConstVec3i = new ConstVec3i(w, z, w)
    def wwx: ConstVec3i = new ConstVec3i(w, w, x)
    def wwy: ConstVec3i = new ConstVec3i(w, w, y)
    def wwz: ConstVec3i = new ConstVec3i(w, w, z)
    def www: ConstVec3i = new ConstVec3i(w, w, w)

    def xxxx: ConstVec4i = new ConstVec4i(x, x, x, x)
    def xxxy: ConstVec4i = new ConstVec4i(x, x, x, y)
    def xxxz: ConstVec4i = new ConstVec4i(x, x, x, z)
    def xxxw: ConstVec4i = new ConstVec4i(x, x, x, w)
    def xxyx: ConstVec4i = new ConstVec4i(x, x, y, x)
    def xxyy: ConstVec4i = new ConstVec4i(x, x, y, y)
    def xxyz: ConstVec4i = new ConstVec4i(x, x, y, z)
    def xxyw: ConstVec4i = new ConstVec4i(x, x, y, w)
    def xxzx: ConstVec4i = new ConstVec4i(x, x, z, x)
    def xxzy: ConstVec4i = new ConstVec4i(x, x, z, y)
    def xxzz: ConstVec4i = new ConstVec4i(x, x, z, z)
    def xxzw: ConstVec4i = new ConstVec4i(x, x, z, w)
    def xxwx: ConstVec4i = new ConstVec4i(x, x, w, x)
    def xxwy: ConstVec4i = new ConstVec4i(x, x, w, y)
    def xxwz: ConstVec4i = new ConstVec4i(x, x, w, z)
    def xxww: ConstVec4i = new ConstVec4i(x, x, w, w)
    def xyxx: ConstVec4i = new ConstVec4i(x, y, x, x)
    def xyxy: ConstVec4i = new ConstVec4i(x, y, x, y)
    def xyxz: ConstVec4i = new ConstVec4i(x, y, x, z)
    def xyxw: ConstVec4i = new ConstVec4i(x, y, x, w)
    def xyyx: ConstVec4i = new ConstVec4i(x, y, y, x)
    def xyyy: ConstVec4i = new ConstVec4i(x, y, y, y)
    def xyyz: ConstVec4i = new ConstVec4i(x, y, y, z)
    def xyyw: ConstVec4i = new ConstVec4i(x, y, y, w)
    def xyzx: ConstVec4i = new ConstVec4i(x, y, z, x)
    def xyzy: ConstVec4i = new ConstVec4i(x, y, z, y)
    def xyzz: ConstVec4i = new ConstVec4i(x, y, z, z)
    def xyzw: ConstVec4i = new ConstVec4i(x, y, z, w)
    def xywx: ConstVec4i = new ConstVec4i(x, y, w, x)
    def xywy: ConstVec4i = new ConstVec4i(x, y, w, y)
    def xywz: ConstVec4i = new ConstVec4i(x, y, w, z)
    def xyww: ConstVec4i = new ConstVec4i(x, y, w, w)
    def xzxx: ConstVec4i = new ConstVec4i(x, z, x, x)
    def xzxy: ConstVec4i = new ConstVec4i(x, z, x, y)
    def xzxz: ConstVec4i = new ConstVec4i(x, z, x, z)
    def xzxw: ConstVec4i = new ConstVec4i(x, z, x, w)
    def xzyx: ConstVec4i = new ConstVec4i(x, z, y, x)
    def xzyy: ConstVec4i = new ConstVec4i(x, z, y, y)
    def xzyz: ConstVec4i = new ConstVec4i(x, z, y, z)
    def xzyw: ConstVec4i = new ConstVec4i(x, z, y, w)
    def xzzx: ConstVec4i = new ConstVec4i(x, z, z, x)
    def xzzy: ConstVec4i = new ConstVec4i(x, z, z, y)
    def xzzz: ConstVec4i = new ConstVec4i(x, z, z, z)
    def xzzw: ConstVec4i = new ConstVec4i(x, z, z, w)
    def xzwx: ConstVec4i = new ConstVec4i(x, z, w, x)
    def xzwy: ConstVec4i = new ConstVec4i(x, z, w, y)
    def xzwz: ConstVec4i = new ConstVec4i(x, z, w, z)
    def xzww: ConstVec4i = new ConstVec4i(x, z, w, w)
    def xwxx: ConstVec4i = new ConstVec4i(x, w, x, x)
    def xwxy: ConstVec4i = new ConstVec4i(x, w, x, y)
    def xwxz: ConstVec4i = new ConstVec4i(x, w, x, z)
    def xwxw: ConstVec4i = new ConstVec4i(x, w, x, w)
    def xwyx: ConstVec4i = new ConstVec4i(x, w, y, x)
    def xwyy: ConstVec4i = new ConstVec4i(x, w, y, y)
    def xwyz: ConstVec4i = new ConstVec4i(x, w, y, z)
    def xwyw: ConstVec4i = new ConstVec4i(x, w, y, w)
    def xwzx: ConstVec4i = new ConstVec4i(x, w, z, x)
    def xwzy: ConstVec4i = new ConstVec4i(x, w, z, y)
    def xwzz: ConstVec4i = new ConstVec4i(x, w, z, z)
    def xwzw: ConstVec4i = new ConstVec4i(x, w, z, w)
    def xwwx: ConstVec4i = new ConstVec4i(x, w, w, x)
    def xwwy: ConstVec4i = new ConstVec4i(x, w, w, y)
    def xwwz: ConstVec4i = new ConstVec4i(x, w, w, z)
    def xwww: ConstVec4i = new ConstVec4i(x, w, w, w)
    def yxxx: ConstVec4i = new ConstVec4i(y, x, x, x)
    def yxxy: ConstVec4i = new ConstVec4i(y, x, x, y)
    def yxxz: ConstVec4i = new ConstVec4i(y, x, x, z)
    def yxxw: ConstVec4i = new ConstVec4i(y, x, x, w)
    def yxyx: ConstVec4i = new ConstVec4i(y, x, y, x)
    def yxyy: ConstVec4i = new ConstVec4i(y, x, y, y)
    def yxyz: ConstVec4i = new ConstVec4i(y, x, y, z)
    def yxyw: ConstVec4i = new ConstVec4i(y, x, y, w)
    def yxzx: ConstVec4i = new ConstVec4i(y, x, z, x)
    def yxzy: ConstVec4i = new ConstVec4i(y, x, z, y)
    def yxzz: ConstVec4i = new ConstVec4i(y, x, z, z)
    def yxzw: ConstVec4i = new ConstVec4i(y, x, z, w)
    def yxwx: ConstVec4i = new ConstVec4i(y, x, w, x)
    def yxwy: ConstVec4i = new ConstVec4i(y, x, w, y)
    def yxwz: ConstVec4i = new ConstVec4i(y, x, w, z)
    def yxww: ConstVec4i = new ConstVec4i(y, x, w, w)
    def yyxx: ConstVec4i = new ConstVec4i(y, y, x, x)
    def yyxy: ConstVec4i = new ConstVec4i(y, y, x, y)
    def yyxz: ConstVec4i = new ConstVec4i(y, y, x, z)
    def yyxw: ConstVec4i = new ConstVec4i(y, y, x, w)
    def yyyx: ConstVec4i = new ConstVec4i(y, y, y, x)
    def yyyy: ConstVec4i = new ConstVec4i(y, y, y, y)
    def yyyz: ConstVec4i = new ConstVec4i(y, y, y, z)
    def yyyw: ConstVec4i = new ConstVec4i(y, y, y, w)
    def yyzx: ConstVec4i = new ConstVec4i(y, y, z, x)
    def yyzy: ConstVec4i = new ConstVec4i(y, y, z, y)
    def yyzz: ConstVec4i = new ConstVec4i(y, y, z, z)
    def yyzw: ConstVec4i = new ConstVec4i(y, y, z, w)
    def yywx: ConstVec4i = new ConstVec4i(y, y, w, x)
    def yywy: ConstVec4i = new ConstVec4i(y, y, w, y)
    def yywz: ConstVec4i = new ConstVec4i(y, y, w, z)
    def yyww: ConstVec4i = new ConstVec4i(y, y, w, w)
    def yzxx: ConstVec4i = new ConstVec4i(y, z, x, x)
    def yzxy: ConstVec4i = new ConstVec4i(y, z, x, y)
    def yzxz: ConstVec4i = new ConstVec4i(y, z, x, z)
    def yzxw: ConstVec4i = new ConstVec4i(y, z, x, w)
    def yzyx: ConstVec4i = new ConstVec4i(y, z, y, x)
    def yzyy: ConstVec4i = new ConstVec4i(y, z, y, y)
    def yzyz: ConstVec4i = new ConstVec4i(y, z, y, z)
    def yzyw: ConstVec4i = new ConstVec4i(y, z, y, w)
    def yzzx: ConstVec4i = new ConstVec4i(y, z, z, x)
    def yzzy: ConstVec4i = new ConstVec4i(y, z, z, y)
    def yzzz: ConstVec4i = new ConstVec4i(y, z, z, z)
    def yzzw: ConstVec4i = new ConstVec4i(y, z, z, w)
    def yzwx: ConstVec4i = new ConstVec4i(y, z, w, x)
    def yzwy: ConstVec4i = new ConstVec4i(y, z, w, y)
    def yzwz: ConstVec4i = new ConstVec4i(y, z, w, z)
    def yzww: ConstVec4i = new ConstVec4i(y, z, w, w)
    def ywxx: ConstVec4i = new ConstVec4i(y, w, x, x)
    def ywxy: ConstVec4i = new ConstVec4i(y, w, x, y)
    def ywxz: ConstVec4i = new ConstVec4i(y, w, x, z)
    def ywxw: ConstVec4i = new ConstVec4i(y, w, x, w)
    def ywyx: ConstVec4i = new ConstVec4i(y, w, y, x)
    def ywyy: ConstVec4i = new ConstVec4i(y, w, y, y)
    def ywyz: ConstVec4i = new ConstVec4i(y, w, y, z)
    def ywyw: ConstVec4i = new ConstVec4i(y, w, y, w)
    def ywzx: ConstVec4i = new ConstVec4i(y, w, z, x)
    def ywzy: ConstVec4i = new ConstVec4i(y, w, z, y)
    def ywzz: ConstVec4i = new ConstVec4i(y, w, z, z)
    def ywzw: ConstVec4i = new ConstVec4i(y, w, z, w)
    def ywwx: ConstVec4i = new ConstVec4i(y, w, w, x)
    def ywwy: ConstVec4i = new ConstVec4i(y, w, w, y)
    def ywwz: ConstVec4i = new ConstVec4i(y, w, w, z)
    def ywww: ConstVec4i = new ConstVec4i(y, w, w, w)
    def zxxx: ConstVec4i = new ConstVec4i(z, x, x, x)
    def zxxy: ConstVec4i = new ConstVec4i(z, x, x, y)
    def zxxz: ConstVec4i = new ConstVec4i(z, x, x, z)
    def zxxw: ConstVec4i = new ConstVec4i(z, x, x, w)
    def zxyx: ConstVec4i = new ConstVec4i(z, x, y, x)
    def zxyy: ConstVec4i = new ConstVec4i(z, x, y, y)
    def zxyz: ConstVec4i = new ConstVec4i(z, x, y, z)
    def zxyw: ConstVec4i = new ConstVec4i(z, x, y, w)
    def zxzx: ConstVec4i = new ConstVec4i(z, x, z, x)
    def zxzy: ConstVec4i = new ConstVec4i(z, x, z, y)
    def zxzz: ConstVec4i = new ConstVec4i(z, x, z, z)
    def zxzw: ConstVec4i = new ConstVec4i(z, x, z, w)
    def zxwx: ConstVec4i = new ConstVec4i(z, x, w, x)
    def zxwy: ConstVec4i = new ConstVec4i(z, x, w, y)
    def zxwz: ConstVec4i = new ConstVec4i(z, x, w, z)
    def zxww: ConstVec4i = new ConstVec4i(z, x, w, w)
    def zyxx: ConstVec4i = new ConstVec4i(z, y, x, x)
    def zyxy: ConstVec4i = new ConstVec4i(z, y, x, y)
    def zyxz: ConstVec4i = new ConstVec4i(z, y, x, z)
    def zyxw: ConstVec4i = new ConstVec4i(z, y, x, w)
    def zyyx: ConstVec4i = new ConstVec4i(z, y, y, x)
    def zyyy: ConstVec4i = new ConstVec4i(z, y, y, y)
    def zyyz: ConstVec4i = new ConstVec4i(z, y, y, z)
    def zyyw: ConstVec4i = new ConstVec4i(z, y, y, w)
    def zyzx: ConstVec4i = new ConstVec4i(z, y, z, x)
    def zyzy: ConstVec4i = new ConstVec4i(z, y, z, y)
    def zyzz: ConstVec4i = new ConstVec4i(z, y, z, z)
    def zyzw: ConstVec4i = new ConstVec4i(z, y, z, w)
    def zywx: ConstVec4i = new ConstVec4i(z, y, w, x)
    def zywy: ConstVec4i = new ConstVec4i(z, y, w, y)
    def zywz: ConstVec4i = new ConstVec4i(z, y, w, z)
    def zyww: ConstVec4i = new ConstVec4i(z, y, w, w)
    def zzxx: ConstVec4i = new ConstVec4i(z, z, x, x)
    def zzxy: ConstVec4i = new ConstVec4i(z, z, x, y)
    def zzxz: ConstVec4i = new ConstVec4i(z, z, x, z)
    def zzxw: ConstVec4i = new ConstVec4i(z, z, x, w)
    def zzyx: ConstVec4i = new ConstVec4i(z, z, y, x)
    def zzyy: ConstVec4i = new ConstVec4i(z, z, y, y)
    def zzyz: ConstVec4i = new ConstVec4i(z, z, y, z)
    def zzyw: ConstVec4i = new ConstVec4i(z, z, y, w)
    def zzzx: ConstVec4i = new ConstVec4i(z, z, z, x)
    def zzzy: ConstVec4i = new ConstVec4i(z, z, z, y)
    def zzzz: ConstVec4i = new ConstVec4i(z, z, z, z)
    def zzzw: ConstVec4i = new ConstVec4i(z, z, z, w)
    def zzwx: ConstVec4i = new ConstVec4i(z, z, w, x)
    def zzwy: ConstVec4i = new ConstVec4i(z, z, w, y)
    def zzwz: ConstVec4i = new ConstVec4i(z, z, w, z)
    def zzww: ConstVec4i = new ConstVec4i(z, z, w, w)
    def zwxx: ConstVec4i = new ConstVec4i(z, w, x, x)
    def zwxy: ConstVec4i = new ConstVec4i(z, w, x, y)
    def zwxz: ConstVec4i = new ConstVec4i(z, w, x, z)
    def zwxw: ConstVec4i = new ConstVec4i(z, w, x, w)
    def zwyx: ConstVec4i = new ConstVec4i(z, w, y, x)
    def zwyy: ConstVec4i = new ConstVec4i(z, w, y, y)
    def zwyz: ConstVec4i = new ConstVec4i(z, w, y, z)
    def zwyw: ConstVec4i = new ConstVec4i(z, w, y, w)
    def zwzx: ConstVec4i = new ConstVec4i(z, w, z, x)
    def zwzy: ConstVec4i = new ConstVec4i(z, w, z, y)
    def zwzz: ConstVec4i = new ConstVec4i(z, w, z, z)
    def zwzw: ConstVec4i = new ConstVec4i(z, w, z, w)
    def zwwx: ConstVec4i = new ConstVec4i(z, w, w, x)
    def zwwy: ConstVec4i = new ConstVec4i(z, w, w, y)
    def zwwz: ConstVec4i = new ConstVec4i(z, w, w, z)
    def zwww: ConstVec4i = new ConstVec4i(z, w, w, w)
    def wxxx: ConstVec4i = new ConstVec4i(w, x, x, x)
    def wxxy: ConstVec4i = new ConstVec4i(w, x, x, y)
    def wxxz: ConstVec4i = new ConstVec4i(w, x, x, z)
    def wxxw: ConstVec4i = new ConstVec4i(w, x, x, w)
    def wxyx: ConstVec4i = new ConstVec4i(w, x, y, x)
    def wxyy: ConstVec4i = new ConstVec4i(w, x, y, y)
    def wxyz: ConstVec4i = new ConstVec4i(w, x, y, z)
    def wxyw: ConstVec4i = new ConstVec4i(w, x, y, w)
    def wxzx: ConstVec4i = new ConstVec4i(w, x, z, x)
    def wxzy: ConstVec4i = new ConstVec4i(w, x, z, y)
    def wxzz: ConstVec4i = new ConstVec4i(w, x, z, z)
    def wxzw: ConstVec4i = new ConstVec4i(w, x, z, w)
    def wxwx: ConstVec4i = new ConstVec4i(w, x, w, x)
    def wxwy: ConstVec4i = new ConstVec4i(w, x, w, y)
    def wxwz: ConstVec4i = new ConstVec4i(w, x, w, z)
    def wxww: ConstVec4i = new ConstVec4i(w, x, w, w)
    def wyxx: ConstVec4i = new ConstVec4i(w, y, x, x)
    def wyxy: ConstVec4i = new ConstVec4i(w, y, x, y)
    def wyxz: ConstVec4i = new ConstVec4i(w, y, x, z)
    def wyxw: ConstVec4i = new ConstVec4i(w, y, x, w)
    def wyyx: ConstVec4i = new ConstVec4i(w, y, y, x)
    def wyyy: ConstVec4i = new ConstVec4i(w, y, y, y)
    def wyyz: ConstVec4i = new ConstVec4i(w, y, y, z)
    def wyyw: ConstVec4i = new ConstVec4i(w, y, y, w)
    def wyzx: ConstVec4i = new ConstVec4i(w, y, z, x)
    def wyzy: ConstVec4i = new ConstVec4i(w, y, z, y)
    def wyzz: ConstVec4i = new ConstVec4i(w, y, z, z)
    def wyzw: ConstVec4i = new ConstVec4i(w, y, z, w)
    def wywx: ConstVec4i = new ConstVec4i(w, y, w, x)
    def wywy: ConstVec4i = new ConstVec4i(w, y, w, y)
    def wywz: ConstVec4i = new ConstVec4i(w, y, w, z)
    def wyww: ConstVec4i = new ConstVec4i(w, y, w, w)
    def wzxx: ConstVec4i = new ConstVec4i(w, z, x, x)
    def wzxy: ConstVec4i = new ConstVec4i(w, z, x, y)
    def wzxz: ConstVec4i = new ConstVec4i(w, z, x, z)
    def wzxw: ConstVec4i = new ConstVec4i(w, z, x, w)
    def wzyx: ConstVec4i = new ConstVec4i(w, z, y, x)
    def wzyy: ConstVec4i = new ConstVec4i(w, z, y, y)
    def wzyz: ConstVec4i = new ConstVec4i(w, z, y, z)
    def wzyw: ConstVec4i = new ConstVec4i(w, z, y, w)
    def wzzx: ConstVec4i = new ConstVec4i(w, z, z, x)
    def wzzy: ConstVec4i = new ConstVec4i(w, z, z, y)
    def wzzz: ConstVec4i = new ConstVec4i(w, z, z, z)
    def wzzw: ConstVec4i = new ConstVec4i(w, z, z, w)
    def wzwx: ConstVec4i = new ConstVec4i(w, z, w, x)
    def wzwy: ConstVec4i = new ConstVec4i(w, z, w, y)
    def wzwz: ConstVec4i = new ConstVec4i(w, z, w, z)
    def wzww: ConstVec4i = new ConstVec4i(w, z, w, w)
    def wwxx: ConstVec4i = new ConstVec4i(w, w, x, x)
    def wwxy: ConstVec4i = new ConstVec4i(w, w, x, y)
    def wwxz: ConstVec4i = new ConstVec4i(w, w, x, z)
    def wwxw: ConstVec4i = new ConstVec4i(w, w, x, w)
    def wwyx: ConstVec4i = new ConstVec4i(w, w, y, x)
    def wwyy: ConstVec4i = new ConstVec4i(w, w, y, y)
    def wwyz: ConstVec4i = new ConstVec4i(w, w, y, z)
    def wwyw: ConstVec4i = new ConstVec4i(w, w, y, w)
    def wwzx: ConstVec4i = new ConstVec4i(w, w, z, x)
    def wwzy: ConstVec4i = new ConstVec4i(w, w, z, y)
    def wwzz: ConstVec4i = new ConstVec4i(w, w, z, z)
    def wwzw: ConstVec4i = new ConstVec4i(w, w, z, w)
    def wwwx: ConstVec4i = new ConstVec4i(w, w, w, x)
    def wwwy: ConstVec4i = new ConstVec4i(w, w, w, y)
    def wwwz: ConstVec4i = new ConstVec4i(w, w, w, z)
    def wwww: ConstVec4i = new ConstVec4i(w, w, w, w)

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
}

final class ConstVec4i private[math] (
    val x: Int, val y: Int, val z: Int, val w: Int)
extends AnyVec4i

object ConstVec4i {
    def apply(x: Int, y: Int, z: Int, w: Int) = {
        new ConstVec4i(x, y, z, w)
    }
    def apply(u: AnyVec4i) = new ConstVec4i(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4i) = new ConstVec4i(u.x, u.y, u.z, u.w)
}


final class Vec4i private[math] (
    var x: Int, var y: Int, var z: Int, var w: Int)
extends AnyVec4i
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    def r_=(r: Int) { x = r }
    def g_=(g: Int) { y = g }
    def b_=(b: Int) { z = b }
    def a_=(a: Int) { w = a }


    def *=(s: Int) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Int) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }
    def %=(s: Int) { x %= s; y %= s; z %= s; w %= s }
    def >>=(s: Int) = { x >>= s; y >>= s; z >>= s; w >>= s }
    def >>>=(s: Int) = { x >>>= s; y >>>= s; z >>>= s; w >>>= s }
    def <<=(s: Int) = { x <<= s; y <<= s; z <<= s; w <<= s }
    def &=(s: Int) = { x &= s; y &= s; z &= s; w &= s }
    def |=(s: Int) = { x |= s; y |= s; z |= s; w |= s }
    def ^=(s: Int) = { x ^= s; y ^= s; z ^= s; w ^= s }

    def +=(u: AnyVec4i) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4i) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4i) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4i) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }
    def %=(u: AnyVec4i) { x %= u.x; y %= u.y; z %= u.z; w %= u.w }
    def >>=(u: AnyVec4i) = { x >>= u.x; y >>= u.y; z >>= u.z; w >>= u.w }
    def >>>=(u: AnyVec4i) = { x >>>= u.x; y >>>= u.y; z >>>= u.z; w >>>= u.w }
    def <<=(u: AnyVec4i) = { x <<= u.x; y <<= u.y; z <<= u.z; w <<= u.w }
    def &=(u: AnyVec4i) = { x &= u.x; y &= u.y; z &= u.z; w &= u.w }
    def |=(u: AnyVec4i) = { x |= u.x; y |= u.y; z |= u.z; w |= u.w }
    def ^=(u: AnyVec4i) = { x ^= u.x; y ^= u.y; z ^= u.z; w ^= u.w }

    def :=(u: AnyVec4i) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Int, y: Int, z: Int, w: Int) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Int) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case 3 => w = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2i = new ConstVec2i(x, y)
    override def xz: ConstVec2i = new ConstVec2i(x, z)
    override def xw: ConstVec2i = new ConstVec2i(x, w)
    override def yx: ConstVec2i = new ConstVec2i(y, x)
    override def yz: ConstVec2i = new ConstVec2i(y, z)
    override def yw: ConstVec2i = new ConstVec2i(y, w)
    override def zx: ConstVec2i = new ConstVec2i(z, x)
    override def zy: ConstVec2i = new ConstVec2i(z, y)
    override def zw: ConstVec2i = new ConstVec2i(z, w)
    override def wx: ConstVec2i = new ConstVec2i(w, x)
    override def wy: ConstVec2i = new ConstVec2i(w, y)
    override def wz: ConstVec2i = new ConstVec2i(w, z)

    override def xyz: ConstVec3i = new ConstVec3i(x, y, z)
    override def xyw: ConstVec3i = new ConstVec3i(x, y, w)
    override def xzy: ConstVec3i = new ConstVec3i(x, z, y)
    override def xzw: ConstVec3i = new ConstVec3i(x, z, w)
    override def xwy: ConstVec3i = new ConstVec3i(x, w, y)
    override def xwz: ConstVec3i = new ConstVec3i(x, w, z)
    override def yxz: ConstVec3i = new ConstVec3i(y, x, z)
    override def yxw: ConstVec3i = new ConstVec3i(y, x, w)
    override def yzx: ConstVec3i = new ConstVec3i(y, z, x)
    override def yzw: ConstVec3i = new ConstVec3i(y, z, w)
    override def ywx: ConstVec3i = new ConstVec3i(y, w, x)
    override def ywz: ConstVec3i = new ConstVec3i(y, w, z)
    override def zxy: ConstVec3i = new ConstVec3i(z, x, y)
    override def zxw: ConstVec3i = new ConstVec3i(z, x, w)
    override def zyx: ConstVec3i = new ConstVec3i(z, y, x)
    override def zyw: ConstVec3i = new ConstVec3i(z, y, w)
    override def zwx: ConstVec3i = new ConstVec3i(z, w, x)
    override def zwy: ConstVec3i = new ConstVec3i(z, w, y)
    override def wxy: ConstVec3i = new ConstVec3i(w, x, y)
    override def wxz: ConstVec3i = new ConstVec3i(w, x, z)
    override def wyx: ConstVec3i = new ConstVec3i(w, y, x)
    override def wyz: ConstVec3i = new ConstVec3i(w, y, z)
    override def wzx: ConstVec3i = new ConstVec3i(w, z, x)
    override def wzy: ConstVec3i = new ConstVec3i(w, z, y)

    override def xyzw: ConstVec4i = new ConstVec4i(x, y, z, w)
    override def xywz: ConstVec4i = new ConstVec4i(x, y, w, z)
    override def xzyw: ConstVec4i = new ConstVec4i(x, z, y, w)
    override def xzwy: ConstVec4i = new ConstVec4i(x, z, w, y)
    override def xwyz: ConstVec4i = new ConstVec4i(x, w, y, z)
    override def xwzy: ConstVec4i = new ConstVec4i(x, w, z, y)
    override def yxzw: ConstVec4i = new ConstVec4i(y, x, z, w)
    override def yxwz: ConstVec4i = new ConstVec4i(y, x, w, z)
    override def yzxw: ConstVec4i = new ConstVec4i(y, z, x, w)
    override def yzwx: ConstVec4i = new ConstVec4i(y, z, w, x)
    override def ywxz: ConstVec4i = new ConstVec4i(y, w, x, z)
    override def ywzx: ConstVec4i = new ConstVec4i(y, w, z, x)
    override def zxyw: ConstVec4i = new ConstVec4i(z, x, y, w)
    override def zxwy: ConstVec4i = new ConstVec4i(z, x, w, y)
    override def zyxw: ConstVec4i = new ConstVec4i(z, y, x, w)
    override def zywx: ConstVec4i = new ConstVec4i(z, y, w, x)
    override def zwxy: ConstVec4i = new ConstVec4i(z, w, x, y)
    override def zwyx: ConstVec4i = new ConstVec4i(z, w, y, x)
    override def wxyz: ConstVec4i = new ConstVec4i(w, x, y, z)
    override def wxzy: ConstVec4i = new ConstVec4i(w, x, z, y)
    override def wyxz: ConstVec4i = new ConstVec4i(w, y, x, z)
    override def wyzx: ConstVec4i = new ConstVec4i(w, y, z, x)
    override def wzxy: ConstVec4i = new ConstVec4i(w, z, x, y)
    override def wzyx: ConstVec4i = new ConstVec4i(w, z, y, x)

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


    def xy_=(u: AnyVec2i) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2i) { x = u.x; z = u.y }
    def xw_=(u: AnyVec2i) { x = u.x; w = u.y }
    def yx_=(u: AnyVec2i) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2i) { y = u.x; z = u.y }
    def yw_=(u: AnyVec2i) { y = u.x; w = u.y }
    def zx_=(u: AnyVec2i) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2i) { z = u.x; y = u.y }
    def zw_=(u: AnyVec2i) { z = u.x; w = u.y }
    def wx_=(u: AnyVec2i) { w = u.x; x = u.y }
    def wy_=(u: AnyVec2i) { w = u.x; y = u.y }
    def wz_=(u: AnyVec2i) { w = u.x; z = u.y }

    def xyz_=(u: AnyVec3i) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: AnyVec3i) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: AnyVec3i) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: AnyVec3i) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: AnyVec3i) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: AnyVec3i) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: AnyVec3i) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: AnyVec3i) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: AnyVec3i) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: AnyVec3i) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: AnyVec3i) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: AnyVec3i) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: AnyVec3i) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: AnyVec3i) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: AnyVec3i) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: AnyVec3i) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: AnyVec3i) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: AnyVec3i) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: AnyVec3i) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: AnyVec3i) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: AnyVec3i) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: AnyVec3i) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: AnyVec3i) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: AnyVec3i) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: AnyVec4i) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: AnyVec4i) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: AnyVec4i) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: AnyVec4i) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: AnyVec4i) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: AnyVec4i) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: AnyVec4i) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: AnyVec4i) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z; z = t }
    def yzxw_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: AnyVec4i) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: AnyVec4i) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: AnyVec4i) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y; y = t }
    def zwyx_=(u: AnyVec4i) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: AnyVec4i) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: AnyVec4i) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: AnyVec4i) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y; y = t }

    def rg_=(u: AnyVec2i) { xy_=(u) }
    def rb_=(u: AnyVec2i) { xz_=(u) }
    def ra_=(u: AnyVec2i) { xw_=(u) }
    def gr_=(u: AnyVec2i) { yx_=(u) }
    def gb_=(u: AnyVec2i) { yz_=(u) }
    def ga_=(u: AnyVec2i) { yw_=(u) }
    def br_=(u: AnyVec2i) { zx_=(u) }
    def bg_=(u: AnyVec2i) { zy_=(u) }
    def ba_=(u: AnyVec2i) { zw_=(u) }
    def ar_=(u: AnyVec2i) { wx_=(u) }
    def ag_=(u: AnyVec2i) { wy_=(u) }
    def ab_=(u: AnyVec2i) { wz_=(u) }

    def rgb_=(u: AnyVec3i) { xyz_=(u) }
    def rga_=(u: AnyVec3i) { xyw_=(u) }
    def rbg_=(u: AnyVec3i) { xzy_=(u) }
    def rba_=(u: AnyVec3i) { xzw_=(u) }
    def rag_=(u: AnyVec3i) { xwy_=(u) }
    def rab_=(u: AnyVec3i) { xwz_=(u) }
    def grb_=(u: AnyVec3i) { yxz_=(u) }
    def gra_=(u: AnyVec3i) { yxw_=(u) }
    def gbr_=(u: AnyVec3i) { yzx_=(u) }
    def gba_=(u: AnyVec3i) { yzw_=(u) }
    def gar_=(u: AnyVec3i) { ywx_=(u) }
    def gab_=(u: AnyVec3i) { ywz_=(u) }
    def brg_=(u: AnyVec3i) { zxy_=(u) }
    def bra_=(u: AnyVec3i) { zxw_=(u) }
    def bgr_=(u: AnyVec3i) { zyx_=(u) }
    def bga_=(u: AnyVec3i) { zyw_=(u) }
    def bar_=(u: AnyVec3i) { zwx_=(u) }
    def bag_=(u: AnyVec3i) { zwy_=(u) }
    def arg_=(u: AnyVec3i) { wxy_=(u) }
    def arb_=(u: AnyVec3i) { wxz_=(u) }
    def agr_=(u: AnyVec3i) { wyx_=(u) }
    def agb_=(u: AnyVec3i) { wyz_=(u) }
    def abr_=(u: AnyVec3i) { wzx_=(u) }
    def abg_=(u: AnyVec3i) { wzy_=(u) }

    def rgba_=(u: AnyVec4i) { xyzw_=(u) }
    def rgab_=(u: AnyVec4i) { xywz_=(u) }
    def rbga_=(u: AnyVec4i) { xzyw_=(u) }
    def rbag_=(u: AnyVec4i) { xzwy_=(u) }
    def ragb_=(u: AnyVec4i) { xwyz_=(u) }
    def rabg_=(u: AnyVec4i) { xwzy_=(u) }
    def grba_=(u: AnyVec4i) { yxzw_=(u) }
    def grab_=(u: AnyVec4i) { yxwz_=(u) }
    def gbra_=(u: AnyVec4i) { yzxw_=(u) }
    def gbar_=(u: AnyVec4i) { yzwx_=(u) }
    def garb_=(u: AnyVec4i) { ywxz_=(u) }
    def gabr_=(u: AnyVec4i) { ywzx_=(u) }
    def brga_=(u: AnyVec4i) { zxyw_=(u) }
    def brag_=(u: AnyVec4i) { zxwy_=(u) }
    def bgra_=(u: AnyVec4i) { zyxw_=(u) }
    def bgar_=(u: AnyVec4i) { zywx_=(u) }
    def barg_=(u: AnyVec4i) { zwxy_=(u) }
    def bagr_=(u: AnyVec4i) { zwyx_=(u) }
    def argb_=(u: AnyVec4i) { wxyz_=(u) }
    def arbg_=(u: AnyVec4i) { wxzy_=(u) }
    def agrb_=(u: AnyVec4i) { wyxz_=(u) }
    def agbr_=(u: AnyVec4i) { wyzx_=(u) }
    def abrg_=(u: AnyVec4i) { wzxy_=(u) }
    def abgr_=(u: AnyVec4i) { wzyx_=(u) }
}

object Vec4i {
    val Origin = new ConstVec4i(0, 0, 0, 0)
    val UnitX = new ConstVec4i(1, 0, 0, 0)
    val UnitY = new ConstVec4i(0, 1, 0, 0)
    val UnitZ = new ConstVec4i(0, 0, 1, 0)
    val UnitW = new ConstVec4i(0, 0, 0, 1)

    def apply(s: Int) =
        new Vec4i(s, s, s, s)

    def apply(x: Int, y: Int, z: Int, w: Int) =
        new Vec4i(x, y, z, w)

    def apply(u: AnyVec4i) =
        new Vec4i(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2i, z: Int, w: Int) =
        new Vec4i(xy.x, xy.y, z, w)

    def apply(x: Int, yz: AnyVec2i, w: Int) =
        new Vec4i(x, yz.x, yz.y, w)

    def apply(x: Int, y: Int, zw: AnyVec2i) =
        new Vec4i(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2i, zw: AnyVec2i) =
        new Vec4i(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3i, w: Int) =
        new Vec4i(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Int, yzw: AnyVec3i) =
        new Vec4i(x, yzw.x, yzw.y, yzw.z)

    def apply(u: Read4Float) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    def apply(u: Read4Double) =
        new Vec4i(int(u.x), int(u.y), int(u.z), int(u.w))

    implicit def constToMutable(u: ConstVec4i) = Vec4i(u)
}
