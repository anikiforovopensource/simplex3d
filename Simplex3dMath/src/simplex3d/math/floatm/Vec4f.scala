/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009 Simplex3d Team
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4f extends Read4Float {

    def r = x
    def g = y
    def b = z
    def a = w


    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = new Vec4f(-x, -y, -z, -w)
    def *(s: Float) = new Vec4f(x * s, y * s, z * s, w * s)
    def /(s: Float) = { val inv = 1/s;
       new Vec4f(x * inv, y * inv, z * inv, w * inv)
    }
    private[math] def divideByComponent(s: Float) = {
        new Vec4f(s / x, s / y, s / z, s / w)
    }

    def +(u: AnyVec4f) = new Vec4f(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4f) = new Vec4f(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4f) = new Vec4f(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4f) = new Vec4f(x / u.x, y / u.y, z / u.z, w / u.w)

    def *(m: AnyMat4x2f) :Vec2f = m.transposeMul(this)
    def *(m: AnyMat4x3f) :Vec3f = m.transposeMul(this)
    def *(m: AnyMat4f) :Vec4f = m.transposeMul(this)

    def ==(u: AnyVec4f) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4f) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z) ||
            isNaN(w) || isInfinite(w)
        )
    }
    
    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }
    
    // Swizzling
    def xx: ConstVec2f = new ConstVec2f(x, x)
    def xy: ConstVec2f = new ConstVec2f(x, y)
    def xz: ConstVec2f = new ConstVec2f(x, z)
    def xw: ConstVec2f = new ConstVec2f(x, w)
    def yx: ConstVec2f = new ConstVec2f(y, x)
    def yy: ConstVec2f = new ConstVec2f(y, y)
    def yz: ConstVec2f = new ConstVec2f(y, z)
    def yw: ConstVec2f = new ConstVec2f(y, w)
    def zx: ConstVec2f = new ConstVec2f(z, x)
    def zy: ConstVec2f = new ConstVec2f(z, y)
    def zz: ConstVec2f = new ConstVec2f(z, z)
    def zw: ConstVec2f = new ConstVec2f(z, w)
    def wx: ConstVec2f = new ConstVec2f(w, x)
    def wy: ConstVec2f = new ConstVec2f(w, y)
    def wz: ConstVec2f = new ConstVec2f(w, z)
    def ww: ConstVec2f = new ConstVec2f(w, w)

    def xxx: ConstVec3f = new ConstVec3f(x, x, x)
    def xxy: ConstVec3f = new ConstVec3f(x, x, y)
    def xxz: ConstVec3f = new ConstVec3f(x, x, z)
    def xxw: ConstVec3f = new ConstVec3f(x, x, w)
    def xyx: ConstVec3f = new ConstVec3f(x, y, x)
    def xyy: ConstVec3f = new ConstVec3f(x, y, y)
    def xyz: ConstVec3f = new ConstVec3f(x, y, z)
    def xyw: ConstVec3f = new ConstVec3f(x, y, w)
    def xzx: ConstVec3f = new ConstVec3f(x, z, x)
    def xzy: ConstVec3f = new ConstVec3f(x, z, y)
    def xzz: ConstVec3f = new ConstVec3f(x, z, z)
    def xzw: ConstVec3f = new ConstVec3f(x, z, w)
    def xwx: ConstVec3f = new ConstVec3f(x, w, x)
    def xwy: ConstVec3f = new ConstVec3f(x, w, y)
    def xwz: ConstVec3f = new ConstVec3f(x, w, z)
    def xww: ConstVec3f = new ConstVec3f(x, w, w)
    def yxx: ConstVec3f = new ConstVec3f(y, x, x)
    def yxy: ConstVec3f = new ConstVec3f(y, x, y)
    def yxz: ConstVec3f = new ConstVec3f(y, x, z)
    def yxw: ConstVec3f = new ConstVec3f(y, x, w)
    def yyx: ConstVec3f = new ConstVec3f(y, y, x)
    def yyy: ConstVec3f = new ConstVec3f(y, y, y)
    def yyz: ConstVec3f = new ConstVec3f(y, y, z)
    def yyw: ConstVec3f = new ConstVec3f(y, y, w)
    def yzx: ConstVec3f = new ConstVec3f(y, z, x)
    def yzy: ConstVec3f = new ConstVec3f(y, z, y)
    def yzz: ConstVec3f = new ConstVec3f(y, z, z)
    def yzw: ConstVec3f = new ConstVec3f(y, z, w)
    def ywx: ConstVec3f = new ConstVec3f(y, w, x)
    def ywy: ConstVec3f = new ConstVec3f(y, w, y)
    def ywz: ConstVec3f = new ConstVec3f(y, w, z)
    def yww: ConstVec3f = new ConstVec3f(y, w, w)
    def zxx: ConstVec3f = new ConstVec3f(z, x, x)
    def zxy: ConstVec3f = new ConstVec3f(z, x, y)
    def zxz: ConstVec3f = new ConstVec3f(z, x, z)
    def zxw: ConstVec3f = new ConstVec3f(z, x, w)
    def zyx: ConstVec3f = new ConstVec3f(z, y, x)
    def zyy: ConstVec3f = new ConstVec3f(z, y, y)
    def zyz: ConstVec3f = new ConstVec3f(z, y, z)
    def zyw: ConstVec3f = new ConstVec3f(z, y, w)
    def zzx: ConstVec3f = new ConstVec3f(z, z, x)
    def zzy: ConstVec3f = new ConstVec3f(z, z, y)
    def zzz: ConstVec3f = new ConstVec3f(z, z, z)
    def zzw: ConstVec3f = new ConstVec3f(z, z, w)
    def zwx: ConstVec3f = new ConstVec3f(z, w, x)
    def zwy: ConstVec3f = new ConstVec3f(z, w, y)
    def zwz: ConstVec3f = new ConstVec3f(z, w, z)
    def zww: ConstVec3f = new ConstVec3f(z, w, w)
    def wxx: ConstVec3f = new ConstVec3f(w, x, x)
    def wxy: ConstVec3f = new ConstVec3f(w, x, y)
    def wxz: ConstVec3f = new ConstVec3f(w, x, z)
    def wxw: ConstVec3f = new ConstVec3f(w, x, w)
    def wyx: ConstVec3f = new ConstVec3f(w, y, x)
    def wyy: ConstVec3f = new ConstVec3f(w, y, y)
    def wyz: ConstVec3f = new ConstVec3f(w, y, z)
    def wyw: ConstVec3f = new ConstVec3f(w, y, w)
    def wzx: ConstVec3f = new ConstVec3f(w, z, x)
    def wzy: ConstVec3f = new ConstVec3f(w, z, y)
    def wzz: ConstVec3f = new ConstVec3f(w, z, z)
    def wzw: ConstVec3f = new ConstVec3f(w, z, w)
    def wwx: ConstVec3f = new ConstVec3f(w, w, x)
    def wwy: ConstVec3f = new ConstVec3f(w, w, y)
    def wwz: ConstVec3f = new ConstVec3f(w, w, z)
    def www: ConstVec3f = new ConstVec3f(w, w, w)

    def xxxx: ConstVec4f = new ConstVec4f(x, x, x, x)
    def xxxy: ConstVec4f = new ConstVec4f(x, x, x, y)
    def xxxz: ConstVec4f = new ConstVec4f(x, x, x, z)
    def xxxw: ConstVec4f = new ConstVec4f(x, x, x, w)
    def xxyx: ConstVec4f = new ConstVec4f(x, x, y, x)
    def xxyy: ConstVec4f = new ConstVec4f(x, x, y, y)
    def xxyz: ConstVec4f = new ConstVec4f(x, x, y, z)
    def xxyw: ConstVec4f = new ConstVec4f(x, x, y, w)
    def xxzx: ConstVec4f = new ConstVec4f(x, x, z, x)
    def xxzy: ConstVec4f = new ConstVec4f(x, x, z, y)
    def xxzz: ConstVec4f = new ConstVec4f(x, x, z, z)
    def xxzw: ConstVec4f = new ConstVec4f(x, x, z, w)
    def xxwx: ConstVec4f = new ConstVec4f(x, x, w, x)
    def xxwy: ConstVec4f = new ConstVec4f(x, x, w, y)
    def xxwz: ConstVec4f = new ConstVec4f(x, x, w, z)
    def xxww: ConstVec4f = new ConstVec4f(x, x, w, w)
    def xyxx: ConstVec4f = new ConstVec4f(x, y, x, x)
    def xyxy: ConstVec4f = new ConstVec4f(x, y, x, y)
    def xyxz: ConstVec4f = new ConstVec4f(x, y, x, z)
    def xyxw: ConstVec4f = new ConstVec4f(x, y, x, w)
    def xyyx: ConstVec4f = new ConstVec4f(x, y, y, x)
    def xyyy: ConstVec4f = new ConstVec4f(x, y, y, y)
    def xyyz: ConstVec4f = new ConstVec4f(x, y, y, z)
    def xyyw: ConstVec4f = new ConstVec4f(x, y, y, w)
    def xyzx: ConstVec4f = new ConstVec4f(x, y, z, x)
    def xyzy: ConstVec4f = new ConstVec4f(x, y, z, y)
    def xyzz: ConstVec4f = new ConstVec4f(x, y, z, z)
    def xyzw: ConstVec4f = new ConstVec4f(x, y, z, w)
    def xywx: ConstVec4f = new ConstVec4f(x, y, w, x)
    def xywy: ConstVec4f = new ConstVec4f(x, y, w, y)
    def xywz: ConstVec4f = new ConstVec4f(x, y, w, z)
    def xyww: ConstVec4f = new ConstVec4f(x, y, w, w)
    def xzxx: ConstVec4f = new ConstVec4f(x, z, x, x)
    def xzxy: ConstVec4f = new ConstVec4f(x, z, x, y)
    def xzxz: ConstVec4f = new ConstVec4f(x, z, x, z)
    def xzxw: ConstVec4f = new ConstVec4f(x, z, x, w)
    def xzyx: ConstVec4f = new ConstVec4f(x, z, y, x)
    def xzyy: ConstVec4f = new ConstVec4f(x, z, y, y)
    def xzyz: ConstVec4f = new ConstVec4f(x, z, y, z)
    def xzyw: ConstVec4f = new ConstVec4f(x, z, y, w)
    def xzzx: ConstVec4f = new ConstVec4f(x, z, z, x)
    def xzzy: ConstVec4f = new ConstVec4f(x, z, z, y)
    def xzzz: ConstVec4f = new ConstVec4f(x, z, z, z)
    def xzzw: ConstVec4f = new ConstVec4f(x, z, z, w)
    def xzwx: ConstVec4f = new ConstVec4f(x, z, w, x)
    def xzwy: ConstVec4f = new ConstVec4f(x, z, w, y)
    def xzwz: ConstVec4f = new ConstVec4f(x, z, w, z)
    def xzww: ConstVec4f = new ConstVec4f(x, z, w, w)
    def xwxx: ConstVec4f = new ConstVec4f(x, w, x, x)
    def xwxy: ConstVec4f = new ConstVec4f(x, w, x, y)
    def xwxz: ConstVec4f = new ConstVec4f(x, w, x, z)
    def xwxw: ConstVec4f = new ConstVec4f(x, w, x, w)
    def xwyx: ConstVec4f = new ConstVec4f(x, w, y, x)
    def xwyy: ConstVec4f = new ConstVec4f(x, w, y, y)
    def xwyz: ConstVec4f = new ConstVec4f(x, w, y, z)
    def xwyw: ConstVec4f = new ConstVec4f(x, w, y, w)
    def xwzx: ConstVec4f = new ConstVec4f(x, w, z, x)
    def xwzy: ConstVec4f = new ConstVec4f(x, w, z, y)
    def xwzz: ConstVec4f = new ConstVec4f(x, w, z, z)
    def xwzw: ConstVec4f = new ConstVec4f(x, w, z, w)
    def xwwx: ConstVec4f = new ConstVec4f(x, w, w, x)
    def xwwy: ConstVec4f = new ConstVec4f(x, w, w, y)
    def xwwz: ConstVec4f = new ConstVec4f(x, w, w, z)
    def xwww: ConstVec4f = new ConstVec4f(x, w, w, w)
    def yxxx: ConstVec4f = new ConstVec4f(y, x, x, x)
    def yxxy: ConstVec4f = new ConstVec4f(y, x, x, y)
    def yxxz: ConstVec4f = new ConstVec4f(y, x, x, z)
    def yxxw: ConstVec4f = new ConstVec4f(y, x, x, w)
    def yxyx: ConstVec4f = new ConstVec4f(y, x, y, x)
    def yxyy: ConstVec4f = new ConstVec4f(y, x, y, y)
    def yxyz: ConstVec4f = new ConstVec4f(y, x, y, z)
    def yxyw: ConstVec4f = new ConstVec4f(y, x, y, w)
    def yxzx: ConstVec4f = new ConstVec4f(y, x, z, x)
    def yxzy: ConstVec4f = new ConstVec4f(y, x, z, y)
    def yxzz: ConstVec4f = new ConstVec4f(y, x, z, z)
    def yxzw: ConstVec4f = new ConstVec4f(y, x, z, w)
    def yxwx: ConstVec4f = new ConstVec4f(y, x, w, x)
    def yxwy: ConstVec4f = new ConstVec4f(y, x, w, y)
    def yxwz: ConstVec4f = new ConstVec4f(y, x, w, z)
    def yxww: ConstVec4f = new ConstVec4f(y, x, w, w)
    def yyxx: ConstVec4f = new ConstVec4f(y, y, x, x)
    def yyxy: ConstVec4f = new ConstVec4f(y, y, x, y)
    def yyxz: ConstVec4f = new ConstVec4f(y, y, x, z)
    def yyxw: ConstVec4f = new ConstVec4f(y, y, x, w)
    def yyyx: ConstVec4f = new ConstVec4f(y, y, y, x)
    def yyyy: ConstVec4f = new ConstVec4f(y, y, y, y)
    def yyyz: ConstVec4f = new ConstVec4f(y, y, y, z)
    def yyyw: ConstVec4f = new ConstVec4f(y, y, y, w)
    def yyzx: ConstVec4f = new ConstVec4f(y, y, z, x)
    def yyzy: ConstVec4f = new ConstVec4f(y, y, z, y)
    def yyzz: ConstVec4f = new ConstVec4f(y, y, z, z)
    def yyzw: ConstVec4f = new ConstVec4f(y, y, z, w)
    def yywx: ConstVec4f = new ConstVec4f(y, y, w, x)
    def yywy: ConstVec4f = new ConstVec4f(y, y, w, y)
    def yywz: ConstVec4f = new ConstVec4f(y, y, w, z)
    def yyww: ConstVec4f = new ConstVec4f(y, y, w, w)
    def yzxx: ConstVec4f = new ConstVec4f(y, z, x, x)
    def yzxy: ConstVec4f = new ConstVec4f(y, z, x, y)
    def yzxz: ConstVec4f = new ConstVec4f(y, z, x, z)
    def yzxw: ConstVec4f = new ConstVec4f(y, z, x, w)
    def yzyx: ConstVec4f = new ConstVec4f(y, z, y, x)
    def yzyy: ConstVec4f = new ConstVec4f(y, z, y, y)
    def yzyz: ConstVec4f = new ConstVec4f(y, z, y, z)
    def yzyw: ConstVec4f = new ConstVec4f(y, z, y, w)
    def yzzx: ConstVec4f = new ConstVec4f(y, z, z, x)
    def yzzy: ConstVec4f = new ConstVec4f(y, z, z, y)
    def yzzz: ConstVec4f = new ConstVec4f(y, z, z, z)
    def yzzw: ConstVec4f = new ConstVec4f(y, z, z, w)
    def yzwx: ConstVec4f = new ConstVec4f(y, z, w, x)
    def yzwy: ConstVec4f = new ConstVec4f(y, z, w, y)
    def yzwz: ConstVec4f = new ConstVec4f(y, z, w, z)
    def yzww: ConstVec4f = new ConstVec4f(y, z, w, w)
    def ywxx: ConstVec4f = new ConstVec4f(y, w, x, x)
    def ywxy: ConstVec4f = new ConstVec4f(y, w, x, y)
    def ywxz: ConstVec4f = new ConstVec4f(y, w, x, z)
    def ywxw: ConstVec4f = new ConstVec4f(y, w, x, w)
    def ywyx: ConstVec4f = new ConstVec4f(y, w, y, x)
    def ywyy: ConstVec4f = new ConstVec4f(y, w, y, y)
    def ywyz: ConstVec4f = new ConstVec4f(y, w, y, z)
    def ywyw: ConstVec4f = new ConstVec4f(y, w, y, w)
    def ywzx: ConstVec4f = new ConstVec4f(y, w, z, x)
    def ywzy: ConstVec4f = new ConstVec4f(y, w, z, y)
    def ywzz: ConstVec4f = new ConstVec4f(y, w, z, z)
    def ywzw: ConstVec4f = new ConstVec4f(y, w, z, w)
    def ywwx: ConstVec4f = new ConstVec4f(y, w, w, x)
    def ywwy: ConstVec4f = new ConstVec4f(y, w, w, y)
    def ywwz: ConstVec4f = new ConstVec4f(y, w, w, z)
    def ywww: ConstVec4f = new ConstVec4f(y, w, w, w)
    def zxxx: ConstVec4f = new ConstVec4f(z, x, x, x)
    def zxxy: ConstVec4f = new ConstVec4f(z, x, x, y)
    def zxxz: ConstVec4f = new ConstVec4f(z, x, x, z)
    def zxxw: ConstVec4f = new ConstVec4f(z, x, x, w)
    def zxyx: ConstVec4f = new ConstVec4f(z, x, y, x)
    def zxyy: ConstVec4f = new ConstVec4f(z, x, y, y)
    def zxyz: ConstVec4f = new ConstVec4f(z, x, y, z)
    def zxyw: ConstVec4f = new ConstVec4f(z, x, y, w)
    def zxzx: ConstVec4f = new ConstVec4f(z, x, z, x)
    def zxzy: ConstVec4f = new ConstVec4f(z, x, z, y)
    def zxzz: ConstVec4f = new ConstVec4f(z, x, z, z)
    def zxzw: ConstVec4f = new ConstVec4f(z, x, z, w)
    def zxwx: ConstVec4f = new ConstVec4f(z, x, w, x)
    def zxwy: ConstVec4f = new ConstVec4f(z, x, w, y)
    def zxwz: ConstVec4f = new ConstVec4f(z, x, w, z)
    def zxww: ConstVec4f = new ConstVec4f(z, x, w, w)
    def zyxx: ConstVec4f = new ConstVec4f(z, y, x, x)
    def zyxy: ConstVec4f = new ConstVec4f(z, y, x, y)
    def zyxz: ConstVec4f = new ConstVec4f(z, y, x, z)
    def zyxw: ConstVec4f = new ConstVec4f(z, y, x, w)
    def zyyx: ConstVec4f = new ConstVec4f(z, y, y, x)
    def zyyy: ConstVec4f = new ConstVec4f(z, y, y, y)
    def zyyz: ConstVec4f = new ConstVec4f(z, y, y, z)
    def zyyw: ConstVec4f = new ConstVec4f(z, y, y, w)
    def zyzx: ConstVec4f = new ConstVec4f(z, y, z, x)
    def zyzy: ConstVec4f = new ConstVec4f(z, y, z, y)
    def zyzz: ConstVec4f = new ConstVec4f(z, y, z, z)
    def zyzw: ConstVec4f = new ConstVec4f(z, y, z, w)
    def zywx: ConstVec4f = new ConstVec4f(z, y, w, x)
    def zywy: ConstVec4f = new ConstVec4f(z, y, w, y)
    def zywz: ConstVec4f = new ConstVec4f(z, y, w, z)
    def zyww: ConstVec4f = new ConstVec4f(z, y, w, w)
    def zzxx: ConstVec4f = new ConstVec4f(z, z, x, x)
    def zzxy: ConstVec4f = new ConstVec4f(z, z, x, y)
    def zzxz: ConstVec4f = new ConstVec4f(z, z, x, z)
    def zzxw: ConstVec4f = new ConstVec4f(z, z, x, w)
    def zzyx: ConstVec4f = new ConstVec4f(z, z, y, x)
    def zzyy: ConstVec4f = new ConstVec4f(z, z, y, y)
    def zzyz: ConstVec4f = new ConstVec4f(z, z, y, z)
    def zzyw: ConstVec4f = new ConstVec4f(z, z, y, w)
    def zzzx: ConstVec4f = new ConstVec4f(z, z, z, x)
    def zzzy: ConstVec4f = new ConstVec4f(z, z, z, y)
    def zzzz: ConstVec4f = new ConstVec4f(z, z, z, z)
    def zzzw: ConstVec4f = new ConstVec4f(z, z, z, w)
    def zzwx: ConstVec4f = new ConstVec4f(z, z, w, x)
    def zzwy: ConstVec4f = new ConstVec4f(z, z, w, y)
    def zzwz: ConstVec4f = new ConstVec4f(z, z, w, z)
    def zzww: ConstVec4f = new ConstVec4f(z, z, w, w)
    def zwxx: ConstVec4f = new ConstVec4f(z, w, x, x)
    def zwxy: ConstVec4f = new ConstVec4f(z, w, x, y)
    def zwxz: ConstVec4f = new ConstVec4f(z, w, x, z)
    def zwxw: ConstVec4f = new ConstVec4f(z, w, x, w)
    def zwyx: ConstVec4f = new ConstVec4f(z, w, y, x)
    def zwyy: ConstVec4f = new ConstVec4f(z, w, y, y)
    def zwyz: ConstVec4f = new ConstVec4f(z, w, y, z)
    def zwyw: ConstVec4f = new ConstVec4f(z, w, y, w)
    def zwzx: ConstVec4f = new ConstVec4f(z, w, z, x)
    def zwzy: ConstVec4f = new ConstVec4f(z, w, z, y)
    def zwzz: ConstVec4f = new ConstVec4f(z, w, z, z)
    def zwzw: ConstVec4f = new ConstVec4f(z, w, z, w)
    def zwwx: ConstVec4f = new ConstVec4f(z, w, w, x)
    def zwwy: ConstVec4f = new ConstVec4f(z, w, w, y)
    def zwwz: ConstVec4f = new ConstVec4f(z, w, w, z)
    def zwww: ConstVec4f = new ConstVec4f(z, w, w, w)
    def wxxx: ConstVec4f = new ConstVec4f(w, x, x, x)
    def wxxy: ConstVec4f = new ConstVec4f(w, x, x, y)
    def wxxz: ConstVec4f = new ConstVec4f(w, x, x, z)
    def wxxw: ConstVec4f = new ConstVec4f(w, x, x, w)
    def wxyx: ConstVec4f = new ConstVec4f(w, x, y, x)
    def wxyy: ConstVec4f = new ConstVec4f(w, x, y, y)
    def wxyz: ConstVec4f = new ConstVec4f(w, x, y, z)
    def wxyw: ConstVec4f = new ConstVec4f(w, x, y, w)
    def wxzx: ConstVec4f = new ConstVec4f(w, x, z, x)
    def wxzy: ConstVec4f = new ConstVec4f(w, x, z, y)
    def wxzz: ConstVec4f = new ConstVec4f(w, x, z, z)
    def wxzw: ConstVec4f = new ConstVec4f(w, x, z, w)
    def wxwx: ConstVec4f = new ConstVec4f(w, x, w, x)
    def wxwy: ConstVec4f = new ConstVec4f(w, x, w, y)
    def wxwz: ConstVec4f = new ConstVec4f(w, x, w, z)
    def wxww: ConstVec4f = new ConstVec4f(w, x, w, w)
    def wyxx: ConstVec4f = new ConstVec4f(w, y, x, x)
    def wyxy: ConstVec4f = new ConstVec4f(w, y, x, y)
    def wyxz: ConstVec4f = new ConstVec4f(w, y, x, z)
    def wyxw: ConstVec4f = new ConstVec4f(w, y, x, w)
    def wyyx: ConstVec4f = new ConstVec4f(w, y, y, x)
    def wyyy: ConstVec4f = new ConstVec4f(w, y, y, y)
    def wyyz: ConstVec4f = new ConstVec4f(w, y, y, z)
    def wyyw: ConstVec4f = new ConstVec4f(w, y, y, w)
    def wyzx: ConstVec4f = new ConstVec4f(w, y, z, x)
    def wyzy: ConstVec4f = new ConstVec4f(w, y, z, y)
    def wyzz: ConstVec4f = new ConstVec4f(w, y, z, z)
    def wyzw: ConstVec4f = new ConstVec4f(w, y, z, w)
    def wywx: ConstVec4f = new ConstVec4f(w, y, w, x)
    def wywy: ConstVec4f = new ConstVec4f(w, y, w, y)
    def wywz: ConstVec4f = new ConstVec4f(w, y, w, z)
    def wyww: ConstVec4f = new ConstVec4f(w, y, w, w)
    def wzxx: ConstVec4f = new ConstVec4f(w, z, x, x)
    def wzxy: ConstVec4f = new ConstVec4f(w, z, x, y)
    def wzxz: ConstVec4f = new ConstVec4f(w, z, x, z)
    def wzxw: ConstVec4f = new ConstVec4f(w, z, x, w)
    def wzyx: ConstVec4f = new ConstVec4f(w, z, y, x)
    def wzyy: ConstVec4f = new ConstVec4f(w, z, y, y)
    def wzyz: ConstVec4f = new ConstVec4f(w, z, y, z)
    def wzyw: ConstVec4f = new ConstVec4f(w, z, y, w)
    def wzzx: ConstVec4f = new ConstVec4f(w, z, z, x)
    def wzzy: ConstVec4f = new ConstVec4f(w, z, z, y)
    def wzzz: ConstVec4f = new ConstVec4f(w, z, z, z)
    def wzzw: ConstVec4f = new ConstVec4f(w, z, z, w)
    def wzwx: ConstVec4f = new ConstVec4f(w, z, w, x)
    def wzwy: ConstVec4f = new ConstVec4f(w, z, w, y)
    def wzwz: ConstVec4f = new ConstVec4f(w, z, w, z)
    def wzww: ConstVec4f = new ConstVec4f(w, z, w, w)
    def wwxx: ConstVec4f = new ConstVec4f(w, w, x, x)
    def wwxy: ConstVec4f = new ConstVec4f(w, w, x, y)
    def wwxz: ConstVec4f = new ConstVec4f(w, w, x, z)
    def wwxw: ConstVec4f = new ConstVec4f(w, w, x, w)
    def wwyx: ConstVec4f = new ConstVec4f(w, w, y, x)
    def wwyy: ConstVec4f = new ConstVec4f(w, w, y, y)
    def wwyz: ConstVec4f = new ConstVec4f(w, w, y, z)
    def wwyw: ConstVec4f = new ConstVec4f(w, w, y, w)
    def wwzx: ConstVec4f = new ConstVec4f(w, w, z, x)
    def wwzy: ConstVec4f = new ConstVec4f(w, w, z, y)
    def wwzz: ConstVec4f = new ConstVec4f(w, w, z, z)
    def wwzw: ConstVec4f = new ConstVec4f(w, w, z, w)
    def wwwx: ConstVec4f = new ConstVec4f(w, w, w, x)
    def wwwy: ConstVec4f = new ConstVec4f(w, w, w, y)
    def wwwz: ConstVec4f = new ConstVec4f(w, w, w, z)
    def wwww: ConstVec4f = new ConstVec4f(w, w, w, w)

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

final class ConstVec4f private[math] (
    val x: Float, val y: Float, val z: Float, val w: Float)
extends AnyVec4f

object ConstVec4f {
    def apply(x: Float, y: Float, z: Float, w: Float) = {
        new ConstVec4f(x, y, z, w)
    }
    def apply(u: AnyVec4f) = new ConstVec4f(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4f) = new ConstVec4f(u.x, u.y, u.z, u.w)
}


final class Vec4f private[math] (
    var x: Float, var y: Float, var z: Float, var w: Float)
extends AnyVec4f
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }
    def b_=(b: Float) { z = b }
    def a_=(a: Float) { w = a }


    def *=(s: Float) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

    def +=(u: AnyVec4f) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4f) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4f) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4f) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

    def *=(m: AnyMat4f) { this := m.transposeMul(this) }

    def :=(u: AnyVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Float, y: Float, z: Float, w: Float) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Float) {
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
    override def xy: ConstVec2f = new ConstVec2f(x, y)
    override def xz: ConstVec2f = new ConstVec2f(x, z)
    override def xw: ConstVec2f = new ConstVec2f(x, w)
    override def yx: ConstVec2f = new ConstVec2f(y, x)
    override def yz: ConstVec2f = new ConstVec2f(y, z)
    override def yw: ConstVec2f = new ConstVec2f(y, w)
    override def zx: ConstVec2f = new ConstVec2f(z, x)
    override def zy: ConstVec2f = new ConstVec2f(z, y)
    override def zw: ConstVec2f = new ConstVec2f(z, w)
    override def wx: ConstVec2f = new ConstVec2f(w, x)
    override def wy: ConstVec2f = new ConstVec2f(w, y)
    override def wz: ConstVec2f = new ConstVec2f(w, z)

    override def xyz: ConstVec3f = new ConstVec3f(x, y, z)
    override def xyw: ConstVec3f = new ConstVec3f(x, y, w)
    override def xzy: ConstVec3f = new ConstVec3f(x, z, y)
    override def xzw: ConstVec3f = new ConstVec3f(x, z, w)
    override def xwy: ConstVec3f = new ConstVec3f(x, w, y)
    override def xwz: ConstVec3f = new ConstVec3f(x, w, z)
    override def yxz: ConstVec3f = new ConstVec3f(y, x, z)
    override def yxw: ConstVec3f = new ConstVec3f(y, x, w)
    override def yzx: ConstVec3f = new ConstVec3f(y, z, x)
    override def yzw: ConstVec3f = new ConstVec3f(y, z, w)
    override def ywx: ConstVec3f = new ConstVec3f(y, w, x)
    override def ywz: ConstVec3f = new ConstVec3f(y, w, z)
    override def zxy: ConstVec3f = new ConstVec3f(z, x, y)
    override def zxw: ConstVec3f = new ConstVec3f(z, x, w)
    override def zyx: ConstVec3f = new ConstVec3f(z, y, x)
    override def zyw: ConstVec3f = new ConstVec3f(z, y, w)
    override def zwx: ConstVec3f = new ConstVec3f(z, w, x)
    override def zwy: ConstVec3f = new ConstVec3f(z, w, y)
    override def wxy: ConstVec3f = new ConstVec3f(w, x, y)
    override def wxz: ConstVec3f = new ConstVec3f(w, x, z)
    override def wyx: ConstVec3f = new ConstVec3f(w, y, x)
    override def wyz: ConstVec3f = new ConstVec3f(w, y, z)
    override def wzx: ConstVec3f = new ConstVec3f(w, z, x)
    override def wzy: ConstVec3f = new ConstVec3f(w, z, y)

    override def xyzw: ConstVec4f = new ConstVec4f(x, y, z, w)
    override def xywz: ConstVec4f = new ConstVec4f(x, y, w, z)
    override def xzyw: ConstVec4f = new ConstVec4f(x, z, y, w)
    override def xzwy: ConstVec4f = new ConstVec4f(x, z, w, y)
    override def xwyz: ConstVec4f = new ConstVec4f(x, w, y, z)
    override def xwzy: ConstVec4f = new ConstVec4f(x, w, z, y)
    override def yxzw: ConstVec4f = new ConstVec4f(y, x, z, w)
    override def yxwz: ConstVec4f = new ConstVec4f(y, x, w, z)
    override def yzxw: ConstVec4f = new ConstVec4f(y, z, x, w)
    override def yzwx: ConstVec4f = new ConstVec4f(y, z, w, x)
    override def ywxz: ConstVec4f = new ConstVec4f(y, w, x, z)
    override def ywzx: ConstVec4f = new ConstVec4f(y, w, z, x)
    override def zxyw: ConstVec4f = new ConstVec4f(z, x, y, w)
    override def zxwy: ConstVec4f = new ConstVec4f(z, x, w, y)
    override def zyxw: ConstVec4f = new ConstVec4f(z, y, x, w)
    override def zywx: ConstVec4f = new ConstVec4f(z, y, w, x)
    override def zwxy: ConstVec4f = new ConstVec4f(z, w, x, y)
    override def zwyx: ConstVec4f = new ConstVec4f(z, w, y, x)
    override def wxyz: ConstVec4f = new ConstVec4f(w, x, y, z)
    override def wxzy: ConstVec4f = new ConstVec4f(w, x, z, y)
    override def wyxz: ConstVec4f = new ConstVec4f(w, y, x, z)
    override def wyzx: ConstVec4f = new ConstVec4f(w, y, z, x)
    override def wzxy: ConstVec4f = new ConstVec4f(w, z, x, y)
    override def wzyx: ConstVec4f = new ConstVec4f(w, z, y, x)

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


    def xy_=(u: AnyVec2f) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2f) { x = u.x; z = u.y }
    def xw_=(u: AnyVec2f) { x = u.x; w = u.y }
    def yx_=(u: AnyVec2f) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2f) { y = u.x; z = u.y }
    def yw_=(u: AnyVec2f) { y = u.x; w = u.y }
    def zx_=(u: AnyVec2f) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2f) { z = u.x; y = u.y }
    def zw_=(u: AnyVec2f) { z = u.x; w = u.y }
    def wx_=(u: AnyVec2f) { w = u.x; x = u.y }
    def wy_=(u: AnyVec2f) { w = u.x; y = u.y }
    def wz_=(u: AnyVec2f) { w = u.x; z = u.y }

    def xyz_=(u: AnyVec3f) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: AnyVec3f) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: AnyVec3f) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: AnyVec3f) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: AnyVec3f) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: AnyVec3f) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: AnyVec3f) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: AnyVec3f) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: AnyVec3f) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: AnyVec3f) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: AnyVec3f) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: AnyVec3f) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: AnyVec3f) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: AnyVec3f) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: AnyVec3f) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: AnyVec3f) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: AnyVec3f) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: AnyVec3f) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: AnyVec3f) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: AnyVec3f) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: AnyVec3f) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: AnyVec3f) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: AnyVec3f) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: AnyVec3f) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: AnyVec4f) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: AnyVec4f) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: AnyVec4f) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: AnyVec4f) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: AnyVec4f) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: AnyVec4f) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: AnyVec4f) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: AnyVec4f) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z;z=t}
    def yzxw_=(u: AnyVec4f) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: AnyVec4f) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: AnyVec4f) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: AnyVec4f) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: AnyVec4f) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: AnyVec4f) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: AnyVec4f) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: AnyVec4f) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: AnyVec4f) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y;y=t}
    def zwyx_=(u: AnyVec4f) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: AnyVec4f) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: AnyVec4f) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: AnyVec4f) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: AnyVec4f) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: AnyVec4f) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: AnyVec4f) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y;y=t}

    def rg_=(u: AnyVec2f) { xy_=(u) }
    def rb_=(u: AnyVec2f) { xz_=(u) }
    def ra_=(u: AnyVec2f) { xw_=(u) }
    def gr_=(u: AnyVec2f) { yx_=(u) }
    def gb_=(u: AnyVec2f) { yz_=(u) }
    def ga_=(u: AnyVec2f) { yw_=(u) }
    def br_=(u: AnyVec2f) { zx_=(u) }
    def bg_=(u: AnyVec2f) { zy_=(u) }
    def ba_=(u: AnyVec2f) { zw_=(u) }
    def ar_=(u: AnyVec2f) { wx_=(u) }
    def ag_=(u: AnyVec2f) { wy_=(u) }
    def ab_=(u: AnyVec2f) { wz_=(u) }

    def rgb_=(u: AnyVec3f) { xyz_=(u) }
    def rga_=(u: AnyVec3f) { xyw_=(u) }
    def rbg_=(u: AnyVec3f) { xzy_=(u) }
    def rba_=(u: AnyVec3f) { xzw_=(u) }
    def rag_=(u: AnyVec3f) { xwy_=(u) }
    def rab_=(u: AnyVec3f) { xwz_=(u) }
    def grb_=(u: AnyVec3f) { yxz_=(u) }
    def gra_=(u: AnyVec3f) { yxw_=(u) }
    def gbr_=(u: AnyVec3f) { yzx_=(u) }
    def gba_=(u: AnyVec3f) { yzw_=(u) }
    def gar_=(u: AnyVec3f) { ywx_=(u) }
    def gab_=(u: AnyVec3f) { ywz_=(u) }
    def brg_=(u: AnyVec3f) { zxy_=(u) }
    def bra_=(u: AnyVec3f) { zxw_=(u) }
    def bgr_=(u: AnyVec3f) { zyx_=(u) }
    def bga_=(u: AnyVec3f) { zyw_=(u) }
    def bar_=(u: AnyVec3f) { zwx_=(u) }
    def bag_=(u: AnyVec3f) { zwy_=(u) }
    def arg_=(u: AnyVec3f) { wxy_=(u) }
    def arb_=(u: AnyVec3f) { wxz_=(u) }
    def agr_=(u: AnyVec3f) { wyx_=(u) }
    def agb_=(u: AnyVec3f) { wyz_=(u) }
    def abr_=(u: AnyVec3f) { wzx_=(u) }
    def abg_=(u: AnyVec3f) { wzy_=(u) }

    def rgba_=(u: AnyVec4f) { xyzw_=(u) }
    def rgab_=(u: AnyVec4f) { xywz_=(u) }
    def rbga_=(u: AnyVec4f) { xzyw_=(u) }
    def rbag_=(u: AnyVec4f) { xzwy_=(u) }
    def ragb_=(u: AnyVec4f) { xwyz_=(u) }
    def rabg_=(u: AnyVec4f) { xwzy_=(u) }
    def grba_=(u: AnyVec4f) { yxzw_=(u) }
    def grab_=(u: AnyVec4f) { yxwz_=(u) }
    def gbra_=(u: AnyVec4f) { yzxw_=(u) }
    def gbar_=(u: AnyVec4f) { yzwx_=(u) }
    def garb_=(u: AnyVec4f) { ywxz_=(u) }
    def gabr_=(u: AnyVec4f) { ywzx_=(u) }
    def brga_=(u: AnyVec4f) { zxyw_=(u) }
    def brag_=(u: AnyVec4f) { zxwy_=(u) }
    def bgra_=(u: AnyVec4f) { zyxw_=(u) }
    def bgar_=(u: AnyVec4f) { zywx_=(u) }
    def barg_=(u: AnyVec4f) { zwxy_=(u) }
    def bagr_=(u: AnyVec4f) { zwyx_=(u) }
    def argb_=(u: AnyVec4f) { wxyz_=(u) }
    def arbg_=(u: AnyVec4f) { wxzy_=(u) }
    def agrb_=(u: AnyVec4f) { wyxz_=(u) }
    def agbr_=(u: AnyVec4f) { wyzx_=(u) }
    def abrg_=(u: AnyVec4f) { wzxy_=(u) }
    def abgr_=(u: AnyVec4f) { wzyx_=(u) }
}

object Vec4f {
    val Origin = new ConstVec4f(0, 0, 0, 0)
    val UnitX = new ConstVec4f(1, 0, 0, 0)
    val UnitY = new ConstVec4f(0, 1, 0, 0)
    val UnitZ = new ConstVec4f(0, 0, 1, 0)
    val UnitW = new ConstVec4f(0, 0, 0, 1)

    def apply(s: Float) =
        new Vec4f(s, s, s, s)

    def apply(x: Float, y: Float, z: Float, w: Float) =
        new Vec4f(x, y, z, w)

    def apply(u: AnyVec4f) =
        new Vec4f(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2f, z: Float, w: Float) =
        new Vec4f(xy.x, xy.y, z, w)

    def apply(x: Float, yz: AnyVec2f, w: Float) =
        new Vec4f(x, yz.x, yz.y, w)

    def apply(x: Float, y: Float, zw: AnyVec2f) =
        new Vec4f(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2f, zw: AnyVec2f) =
        new Vec4f(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3f, w: Float) =
        new Vec4f(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Float, yzw: AnyVec3f) =
        new Vec4f(x, yzw.x, yzw.y, yzw.z)

    def apply(m: AnyMat2f) =
        new Vec4f(m.m00, m.m10, m.m01, m.m11)

    def apply(u: Read4Int) =
        new Vec4f(u.x, u.y, u.z, u.w)

    def apply(u: Read4Double) =
        new Vec4f(float(u.x), float(u.y), float(u.z), float(u.w))

    implicit def constToMutable(u: ConstVec4f) = Vec4f(u)
}
