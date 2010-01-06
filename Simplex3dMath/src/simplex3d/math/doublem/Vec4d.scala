/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4d extends Read4Double {

    def r = x
    def g = y
    def b = z
    def a = w


    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def unary_-() = new Vec4d(-x, -y, -z, -w)
    def *(s: Double) = new Vec4d(x * s, y * s, z * s, w * s)
    def /(s: Double) = { val inv = 1/s;
        new Vec4d(x * inv, y * inv, z * inv, w * inv)
    }
    private[math] def divideByComponent(s: Double) = {
        new Vec4d(s / x, s / y, s / z, s / w)
    }

    def +(u: AnyVec4d) = new Vec4d(x + u.x, y + u.y, z + u.z, w + u.w)
    def -(u: AnyVec4d) = new Vec4d(x - u.x, y - u.y, z - u.z, w - u.w)
    def *(u: AnyVec4d) = new Vec4d(x * u.x, y * u.y, z * u.z, w * u.w)
    def /(u: AnyVec4d) = new Vec4d(x / u.x, y / u.y, z / u.z, w / u.w)

    def *(m: AnyMat4x2d) :Vec2d = m.transposeMul(this)
    def *(m: AnyMat4x3d) :Vec3d = m.transposeMul(this)
    def *(m: AnyMat4d) :Vec4d = m.transposeMul(this)

    def ==(u: AnyVec4d) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4d) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y) ||
            isNaN(z) || isInfinite(z) ||
            isNaN(w) || isInfinite(w)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec4d => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        41 * (
            41 * (
                41 * (
                    41 + x.hashCode
                ) + y.hashCode
            ) + z.hashCode
        ) + w.hashCode
    }

    override def toString = {
        this.getClass.getSimpleName +
        "(" + x + ", " + y + ", " + z + ", " + w + ")"
    }

    // Swizzling
    def xx: ConstVec2d = new ConstVec2d(x, x)
    def xy: ConstVec2d = new ConstVec2d(x, y)
    def xz: ConstVec2d = new ConstVec2d(x, z)
    def xw: ConstVec2d = new ConstVec2d(x, w)
    def yx: ConstVec2d = new ConstVec2d(y, x)
    def yy: ConstVec2d = new ConstVec2d(y, y)
    def yz: ConstVec2d = new ConstVec2d(y, z)
    def yw: ConstVec2d = new ConstVec2d(y, w)
    def zx: ConstVec2d = new ConstVec2d(z, x)
    def zy: ConstVec2d = new ConstVec2d(z, y)
    def zz: ConstVec2d = new ConstVec2d(z, z)
    def zw: ConstVec2d = new ConstVec2d(z, w)
    def wx: ConstVec2d = new ConstVec2d(w, x)
    def wy: ConstVec2d = new ConstVec2d(w, y)
    def wz: ConstVec2d = new ConstVec2d(w, z)
    def ww: ConstVec2d = new ConstVec2d(w, w)

    def xxx: ConstVec3d = new ConstVec3d(x, x, x)
    def xxy: ConstVec3d = new ConstVec3d(x, x, y)
    def xxz: ConstVec3d = new ConstVec3d(x, x, z)
    def xxw: ConstVec3d = new ConstVec3d(x, x, w)
    def xyx: ConstVec3d = new ConstVec3d(x, y, x)
    def xyy: ConstVec3d = new ConstVec3d(x, y, y)
    def xyz: ConstVec3d = new ConstVec3d(x, y, z)
    def xyw: ConstVec3d = new ConstVec3d(x, y, w)
    def xzx: ConstVec3d = new ConstVec3d(x, z, x)
    def xzy: ConstVec3d = new ConstVec3d(x, z, y)
    def xzz: ConstVec3d = new ConstVec3d(x, z, z)
    def xzw: ConstVec3d = new ConstVec3d(x, z, w)
    def xwx: ConstVec3d = new ConstVec3d(x, w, x)
    def xwy: ConstVec3d = new ConstVec3d(x, w, y)
    def xwz: ConstVec3d = new ConstVec3d(x, w, z)
    def xww: ConstVec3d = new ConstVec3d(x, w, w)
    def yxx: ConstVec3d = new ConstVec3d(y, x, x)
    def yxy: ConstVec3d = new ConstVec3d(y, x, y)
    def yxz: ConstVec3d = new ConstVec3d(y, x, z)
    def yxw: ConstVec3d = new ConstVec3d(y, x, w)
    def yyx: ConstVec3d = new ConstVec3d(y, y, x)
    def yyy: ConstVec3d = new ConstVec3d(y, y, y)
    def yyz: ConstVec3d = new ConstVec3d(y, y, z)
    def yyw: ConstVec3d = new ConstVec3d(y, y, w)
    def yzx: ConstVec3d = new ConstVec3d(y, z, x)
    def yzy: ConstVec3d = new ConstVec3d(y, z, y)
    def yzz: ConstVec3d = new ConstVec3d(y, z, z)
    def yzw: ConstVec3d = new ConstVec3d(y, z, w)
    def ywx: ConstVec3d = new ConstVec3d(y, w, x)
    def ywy: ConstVec3d = new ConstVec3d(y, w, y)
    def ywz: ConstVec3d = new ConstVec3d(y, w, z)
    def yww: ConstVec3d = new ConstVec3d(y, w, w)
    def zxx: ConstVec3d = new ConstVec3d(z, x, x)
    def zxy: ConstVec3d = new ConstVec3d(z, x, y)
    def zxz: ConstVec3d = new ConstVec3d(z, x, z)
    def zxw: ConstVec3d = new ConstVec3d(z, x, w)
    def zyx: ConstVec3d = new ConstVec3d(z, y, x)
    def zyy: ConstVec3d = new ConstVec3d(z, y, y)
    def zyz: ConstVec3d = new ConstVec3d(z, y, z)
    def zyw: ConstVec3d = new ConstVec3d(z, y, w)
    def zzx: ConstVec3d = new ConstVec3d(z, z, x)
    def zzy: ConstVec3d = new ConstVec3d(z, z, y)
    def zzz: ConstVec3d = new ConstVec3d(z, z, z)
    def zzw: ConstVec3d = new ConstVec3d(z, z, w)
    def zwx: ConstVec3d = new ConstVec3d(z, w, x)
    def zwy: ConstVec3d = new ConstVec3d(z, w, y)
    def zwz: ConstVec3d = new ConstVec3d(z, w, z)
    def zww: ConstVec3d = new ConstVec3d(z, w, w)
    def wxx: ConstVec3d = new ConstVec3d(w, x, x)
    def wxy: ConstVec3d = new ConstVec3d(w, x, y)
    def wxz: ConstVec3d = new ConstVec3d(w, x, z)
    def wxw: ConstVec3d = new ConstVec3d(w, x, w)
    def wyx: ConstVec3d = new ConstVec3d(w, y, x)
    def wyy: ConstVec3d = new ConstVec3d(w, y, y)
    def wyz: ConstVec3d = new ConstVec3d(w, y, z)
    def wyw: ConstVec3d = new ConstVec3d(w, y, w)
    def wzx: ConstVec3d = new ConstVec3d(w, z, x)
    def wzy: ConstVec3d = new ConstVec3d(w, z, y)
    def wzz: ConstVec3d = new ConstVec3d(w, z, z)
    def wzw: ConstVec3d = new ConstVec3d(w, z, w)
    def wwx: ConstVec3d = new ConstVec3d(w, w, x)
    def wwy: ConstVec3d = new ConstVec3d(w, w, y)
    def wwz: ConstVec3d = new ConstVec3d(w, w, z)
    def www: ConstVec3d = new ConstVec3d(w, w, w)

    def xxxx: ConstVec4d = new ConstVec4d(x, x, x, x)
    def xxxy: ConstVec4d = new ConstVec4d(x, x, x, y)
    def xxxz: ConstVec4d = new ConstVec4d(x, x, x, z)
    def xxxw: ConstVec4d = new ConstVec4d(x, x, x, w)
    def xxyx: ConstVec4d = new ConstVec4d(x, x, y, x)
    def xxyy: ConstVec4d = new ConstVec4d(x, x, y, y)
    def xxyz: ConstVec4d = new ConstVec4d(x, x, y, z)
    def xxyw: ConstVec4d = new ConstVec4d(x, x, y, w)
    def xxzx: ConstVec4d = new ConstVec4d(x, x, z, x)
    def xxzy: ConstVec4d = new ConstVec4d(x, x, z, y)
    def xxzz: ConstVec4d = new ConstVec4d(x, x, z, z)
    def xxzw: ConstVec4d = new ConstVec4d(x, x, z, w)
    def xxwx: ConstVec4d = new ConstVec4d(x, x, w, x)
    def xxwy: ConstVec4d = new ConstVec4d(x, x, w, y)
    def xxwz: ConstVec4d = new ConstVec4d(x, x, w, z)
    def xxww: ConstVec4d = new ConstVec4d(x, x, w, w)
    def xyxx: ConstVec4d = new ConstVec4d(x, y, x, x)
    def xyxy: ConstVec4d = new ConstVec4d(x, y, x, y)
    def xyxz: ConstVec4d = new ConstVec4d(x, y, x, z)
    def xyxw: ConstVec4d = new ConstVec4d(x, y, x, w)
    def xyyx: ConstVec4d = new ConstVec4d(x, y, y, x)
    def xyyy: ConstVec4d = new ConstVec4d(x, y, y, y)
    def xyyz: ConstVec4d = new ConstVec4d(x, y, y, z)
    def xyyw: ConstVec4d = new ConstVec4d(x, y, y, w)
    def xyzx: ConstVec4d = new ConstVec4d(x, y, z, x)
    def xyzy: ConstVec4d = new ConstVec4d(x, y, z, y)
    def xyzz: ConstVec4d = new ConstVec4d(x, y, z, z)
    def xyzw: ConstVec4d = new ConstVec4d(x, y, z, w)
    def xywx: ConstVec4d = new ConstVec4d(x, y, w, x)
    def xywy: ConstVec4d = new ConstVec4d(x, y, w, y)
    def xywz: ConstVec4d = new ConstVec4d(x, y, w, z)
    def xyww: ConstVec4d = new ConstVec4d(x, y, w, w)
    def xzxx: ConstVec4d = new ConstVec4d(x, z, x, x)
    def xzxy: ConstVec4d = new ConstVec4d(x, z, x, y)
    def xzxz: ConstVec4d = new ConstVec4d(x, z, x, z)
    def xzxw: ConstVec4d = new ConstVec4d(x, z, x, w)
    def xzyx: ConstVec4d = new ConstVec4d(x, z, y, x)
    def xzyy: ConstVec4d = new ConstVec4d(x, z, y, y)
    def xzyz: ConstVec4d = new ConstVec4d(x, z, y, z)
    def xzyw: ConstVec4d = new ConstVec4d(x, z, y, w)
    def xzzx: ConstVec4d = new ConstVec4d(x, z, z, x)
    def xzzy: ConstVec4d = new ConstVec4d(x, z, z, y)
    def xzzz: ConstVec4d = new ConstVec4d(x, z, z, z)
    def xzzw: ConstVec4d = new ConstVec4d(x, z, z, w)
    def xzwx: ConstVec4d = new ConstVec4d(x, z, w, x)
    def xzwy: ConstVec4d = new ConstVec4d(x, z, w, y)
    def xzwz: ConstVec4d = new ConstVec4d(x, z, w, z)
    def xzww: ConstVec4d = new ConstVec4d(x, z, w, w)
    def xwxx: ConstVec4d = new ConstVec4d(x, w, x, x)
    def xwxy: ConstVec4d = new ConstVec4d(x, w, x, y)
    def xwxz: ConstVec4d = new ConstVec4d(x, w, x, z)
    def xwxw: ConstVec4d = new ConstVec4d(x, w, x, w)
    def xwyx: ConstVec4d = new ConstVec4d(x, w, y, x)
    def xwyy: ConstVec4d = new ConstVec4d(x, w, y, y)
    def xwyz: ConstVec4d = new ConstVec4d(x, w, y, z)
    def xwyw: ConstVec4d = new ConstVec4d(x, w, y, w)
    def xwzx: ConstVec4d = new ConstVec4d(x, w, z, x)
    def xwzy: ConstVec4d = new ConstVec4d(x, w, z, y)
    def xwzz: ConstVec4d = new ConstVec4d(x, w, z, z)
    def xwzw: ConstVec4d = new ConstVec4d(x, w, z, w)
    def xwwx: ConstVec4d = new ConstVec4d(x, w, w, x)
    def xwwy: ConstVec4d = new ConstVec4d(x, w, w, y)
    def xwwz: ConstVec4d = new ConstVec4d(x, w, w, z)
    def xwww: ConstVec4d = new ConstVec4d(x, w, w, w)
    def yxxx: ConstVec4d = new ConstVec4d(y, x, x, x)
    def yxxy: ConstVec4d = new ConstVec4d(y, x, x, y)
    def yxxz: ConstVec4d = new ConstVec4d(y, x, x, z)
    def yxxw: ConstVec4d = new ConstVec4d(y, x, x, w)
    def yxyx: ConstVec4d = new ConstVec4d(y, x, y, x)
    def yxyy: ConstVec4d = new ConstVec4d(y, x, y, y)
    def yxyz: ConstVec4d = new ConstVec4d(y, x, y, z)
    def yxyw: ConstVec4d = new ConstVec4d(y, x, y, w)
    def yxzx: ConstVec4d = new ConstVec4d(y, x, z, x)
    def yxzy: ConstVec4d = new ConstVec4d(y, x, z, y)
    def yxzz: ConstVec4d = new ConstVec4d(y, x, z, z)
    def yxzw: ConstVec4d = new ConstVec4d(y, x, z, w)
    def yxwx: ConstVec4d = new ConstVec4d(y, x, w, x)
    def yxwy: ConstVec4d = new ConstVec4d(y, x, w, y)
    def yxwz: ConstVec4d = new ConstVec4d(y, x, w, z)
    def yxww: ConstVec4d = new ConstVec4d(y, x, w, w)
    def yyxx: ConstVec4d = new ConstVec4d(y, y, x, x)
    def yyxy: ConstVec4d = new ConstVec4d(y, y, x, y)
    def yyxz: ConstVec4d = new ConstVec4d(y, y, x, z)
    def yyxw: ConstVec4d = new ConstVec4d(y, y, x, w)
    def yyyx: ConstVec4d = new ConstVec4d(y, y, y, x)
    def yyyy: ConstVec4d = new ConstVec4d(y, y, y, y)
    def yyyz: ConstVec4d = new ConstVec4d(y, y, y, z)
    def yyyw: ConstVec4d = new ConstVec4d(y, y, y, w)
    def yyzx: ConstVec4d = new ConstVec4d(y, y, z, x)
    def yyzy: ConstVec4d = new ConstVec4d(y, y, z, y)
    def yyzz: ConstVec4d = new ConstVec4d(y, y, z, z)
    def yyzw: ConstVec4d = new ConstVec4d(y, y, z, w)
    def yywx: ConstVec4d = new ConstVec4d(y, y, w, x)
    def yywy: ConstVec4d = new ConstVec4d(y, y, w, y)
    def yywz: ConstVec4d = new ConstVec4d(y, y, w, z)
    def yyww: ConstVec4d = new ConstVec4d(y, y, w, w)
    def yzxx: ConstVec4d = new ConstVec4d(y, z, x, x)
    def yzxy: ConstVec4d = new ConstVec4d(y, z, x, y)
    def yzxz: ConstVec4d = new ConstVec4d(y, z, x, z)
    def yzxw: ConstVec4d = new ConstVec4d(y, z, x, w)
    def yzyx: ConstVec4d = new ConstVec4d(y, z, y, x)
    def yzyy: ConstVec4d = new ConstVec4d(y, z, y, y)
    def yzyz: ConstVec4d = new ConstVec4d(y, z, y, z)
    def yzyw: ConstVec4d = new ConstVec4d(y, z, y, w)
    def yzzx: ConstVec4d = new ConstVec4d(y, z, z, x)
    def yzzy: ConstVec4d = new ConstVec4d(y, z, z, y)
    def yzzz: ConstVec4d = new ConstVec4d(y, z, z, z)
    def yzzw: ConstVec4d = new ConstVec4d(y, z, z, w)
    def yzwx: ConstVec4d = new ConstVec4d(y, z, w, x)
    def yzwy: ConstVec4d = new ConstVec4d(y, z, w, y)
    def yzwz: ConstVec4d = new ConstVec4d(y, z, w, z)
    def yzww: ConstVec4d = new ConstVec4d(y, z, w, w)
    def ywxx: ConstVec4d = new ConstVec4d(y, w, x, x)
    def ywxy: ConstVec4d = new ConstVec4d(y, w, x, y)
    def ywxz: ConstVec4d = new ConstVec4d(y, w, x, z)
    def ywxw: ConstVec4d = new ConstVec4d(y, w, x, w)
    def ywyx: ConstVec4d = new ConstVec4d(y, w, y, x)
    def ywyy: ConstVec4d = new ConstVec4d(y, w, y, y)
    def ywyz: ConstVec4d = new ConstVec4d(y, w, y, z)
    def ywyw: ConstVec4d = new ConstVec4d(y, w, y, w)
    def ywzx: ConstVec4d = new ConstVec4d(y, w, z, x)
    def ywzy: ConstVec4d = new ConstVec4d(y, w, z, y)
    def ywzz: ConstVec4d = new ConstVec4d(y, w, z, z)
    def ywzw: ConstVec4d = new ConstVec4d(y, w, z, w)
    def ywwx: ConstVec4d = new ConstVec4d(y, w, w, x)
    def ywwy: ConstVec4d = new ConstVec4d(y, w, w, y)
    def ywwz: ConstVec4d = new ConstVec4d(y, w, w, z)
    def ywww: ConstVec4d = new ConstVec4d(y, w, w, w)
    def zxxx: ConstVec4d = new ConstVec4d(z, x, x, x)
    def zxxy: ConstVec4d = new ConstVec4d(z, x, x, y)
    def zxxz: ConstVec4d = new ConstVec4d(z, x, x, z)
    def zxxw: ConstVec4d = new ConstVec4d(z, x, x, w)
    def zxyx: ConstVec4d = new ConstVec4d(z, x, y, x)
    def zxyy: ConstVec4d = new ConstVec4d(z, x, y, y)
    def zxyz: ConstVec4d = new ConstVec4d(z, x, y, z)
    def zxyw: ConstVec4d = new ConstVec4d(z, x, y, w)
    def zxzx: ConstVec4d = new ConstVec4d(z, x, z, x)
    def zxzy: ConstVec4d = new ConstVec4d(z, x, z, y)
    def zxzz: ConstVec4d = new ConstVec4d(z, x, z, z)
    def zxzw: ConstVec4d = new ConstVec4d(z, x, z, w)
    def zxwx: ConstVec4d = new ConstVec4d(z, x, w, x)
    def zxwy: ConstVec4d = new ConstVec4d(z, x, w, y)
    def zxwz: ConstVec4d = new ConstVec4d(z, x, w, z)
    def zxww: ConstVec4d = new ConstVec4d(z, x, w, w)
    def zyxx: ConstVec4d = new ConstVec4d(z, y, x, x)
    def zyxy: ConstVec4d = new ConstVec4d(z, y, x, y)
    def zyxz: ConstVec4d = new ConstVec4d(z, y, x, z)
    def zyxw: ConstVec4d = new ConstVec4d(z, y, x, w)
    def zyyx: ConstVec4d = new ConstVec4d(z, y, y, x)
    def zyyy: ConstVec4d = new ConstVec4d(z, y, y, y)
    def zyyz: ConstVec4d = new ConstVec4d(z, y, y, z)
    def zyyw: ConstVec4d = new ConstVec4d(z, y, y, w)
    def zyzx: ConstVec4d = new ConstVec4d(z, y, z, x)
    def zyzy: ConstVec4d = new ConstVec4d(z, y, z, y)
    def zyzz: ConstVec4d = new ConstVec4d(z, y, z, z)
    def zyzw: ConstVec4d = new ConstVec4d(z, y, z, w)
    def zywx: ConstVec4d = new ConstVec4d(z, y, w, x)
    def zywy: ConstVec4d = new ConstVec4d(z, y, w, y)
    def zywz: ConstVec4d = new ConstVec4d(z, y, w, z)
    def zyww: ConstVec4d = new ConstVec4d(z, y, w, w)
    def zzxx: ConstVec4d = new ConstVec4d(z, z, x, x)
    def zzxy: ConstVec4d = new ConstVec4d(z, z, x, y)
    def zzxz: ConstVec4d = new ConstVec4d(z, z, x, z)
    def zzxw: ConstVec4d = new ConstVec4d(z, z, x, w)
    def zzyx: ConstVec4d = new ConstVec4d(z, z, y, x)
    def zzyy: ConstVec4d = new ConstVec4d(z, z, y, y)
    def zzyz: ConstVec4d = new ConstVec4d(z, z, y, z)
    def zzyw: ConstVec4d = new ConstVec4d(z, z, y, w)
    def zzzx: ConstVec4d = new ConstVec4d(z, z, z, x)
    def zzzy: ConstVec4d = new ConstVec4d(z, z, z, y)
    def zzzz: ConstVec4d = new ConstVec4d(z, z, z, z)
    def zzzw: ConstVec4d = new ConstVec4d(z, z, z, w)
    def zzwx: ConstVec4d = new ConstVec4d(z, z, w, x)
    def zzwy: ConstVec4d = new ConstVec4d(z, z, w, y)
    def zzwz: ConstVec4d = new ConstVec4d(z, z, w, z)
    def zzww: ConstVec4d = new ConstVec4d(z, z, w, w)
    def zwxx: ConstVec4d = new ConstVec4d(z, w, x, x)
    def zwxy: ConstVec4d = new ConstVec4d(z, w, x, y)
    def zwxz: ConstVec4d = new ConstVec4d(z, w, x, z)
    def zwxw: ConstVec4d = new ConstVec4d(z, w, x, w)
    def zwyx: ConstVec4d = new ConstVec4d(z, w, y, x)
    def zwyy: ConstVec4d = new ConstVec4d(z, w, y, y)
    def zwyz: ConstVec4d = new ConstVec4d(z, w, y, z)
    def zwyw: ConstVec4d = new ConstVec4d(z, w, y, w)
    def zwzx: ConstVec4d = new ConstVec4d(z, w, z, x)
    def zwzy: ConstVec4d = new ConstVec4d(z, w, z, y)
    def zwzz: ConstVec4d = new ConstVec4d(z, w, z, z)
    def zwzw: ConstVec4d = new ConstVec4d(z, w, z, w)
    def zwwx: ConstVec4d = new ConstVec4d(z, w, w, x)
    def zwwy: ConstVec4d = new ConstVec4d(z, w, w, y)
    def zwwz: ConstVec4d = new ConstVec4d(z, w, w, z)
    def zwww: ConstVec4d = new ConstVec4d(z, w, w, w)
    def wxxx: ConstVec4d = new ConstVec4d(w, x, x, x)
    def wxxy: ConstVec4d = new ConstVec4d(w, x, x, y)
    def wxxz: ConstVec4d = new ConstVec4d(w, x, x, z)
    def wxxw: ConstVec4d = new ConstVec4d(w, x, x, w)
    def wxyx: ConstVec4d = new ConstVec4d(w, x, y, x)
    def wxyy: ConstVec4d = new ConstVec4d(w, x, y, y)
    def wxyz: ConstVec4d = new ConstVec4d(w, x, y, z)
    def wxyw: ConstVec4d = new ConstVec4d(w, x, y, w)
    def wxzx: ConstVec4d = new ConstVec4d(w, x, z, x)
    def wxzy: ConstVec4d = new ConstVec4d(w, x, z, y)
    def wxzz: ConstVec4d = new ConstVec4d(w, x, z, z)
    def wxzw: ConstVec4d = new ConstVec4d(w, x, z, w)
    def wxwx: ConstVec4d = new ConstVec4d(w, x, w, x)
    def wxwy: ConstVec4d = new ConstVec4d(w, x, w, y)
    def wxwz: ConstVec4d = new ConstVec4d(w, x, w, z)
    def wxww: ConstVec4d = new ConstVec4d(w, x, w, w)
    def wyxx: ConstVec4d = new ConstVec4d(w, y, x, x)
    def wyxy: ConstVec4d = new ConstVec4d(w, y, x, y)
    def wyxz: ConstVec4d = new ConstVec4d(w, y, x, z)
    def wyxw: ConstVec4d = new ConstVec4d(w, y, x, w)
    def wyyx: ConstVec4d = new ConstVec4d(w, y, y, x)
    def wyyy: ConstVec4d = new ConstVec4d(w, y, y, y)
    def wyyz: ConstVec4d = new ConstVec4d(w, y, y, z)
    def wyyw: ConstVec4d = new ConstVec4d(w, y, y, w)
    def wyzx: ConstVec4d = new ConstVec4d(w, y, z, x)
    def wyzy: ConstVec4d = new ConstVec4d(w, y, z, y)
    def wyzz: ConstVec4d = new ConstVec4d(w, y, z, z)
    def wyzw: ConstVec4d = new ConstVec4d(w, y, z, w)
    def wywx: ConstVec4d = new ConstVec4d(w, y, w, x)
    def wywy: ConstVec4d = new ConstVec4d(w, y, w, y)
    def wywz: ConstVec4d = new ConstVec4d(w, y, w, z)
    def wyww: ConstVec4d = new ConstVec4d(w, y, w, w)
    def wzxx: ConstVec4d = new ConstVec4d(w, z, x, x)
    def wzxy: ConstVec4d = new ConstVec4d(w, z, x, y)
    def wzxz: ConstVec4d = new ConstVec4d(w, z, x, z)
    def wzxw: ConstVec4d = new ConstVec4d(w, z, x, w)
    def wzyx: ConstVec4d = new ConstVec4d(w, z, y, x)
    def wzyy: ConstVec4d = new ConstVec4d(w, z, y, y)
    def wzyz: ConstVec4d = new ConstVec4d(w, z, y, z)
    def wzyw: ConstVec4d = new ConstVec4d(w, z, y, w)
    def wzzx: ConstVec4d = new ConstVec4d(w, z, z, x)
    def wzzy: ConstVec4d = new ConstVec4d(w, z, z, y)
    def wzzz: ConstVec4d = new ConstVec4d(w, z, z, z)
    def wzzw: ConstVec4d = new ConstVec4d(w, z, z, w)
    def wzwx: ConstVec4d = new ConstVec4d(w, z, w, x)
    def wzwy: ConstVec4d = new ConstVec4d(w, z, w, y)
    def wzwz: ConstVec4d = new ConstVec4d(w, z, w, z)
    def wzww: ConstVec4d = new ConstVec4d(w, z, w, w)
    def wwxx: ConstVec4d = new ConstVec4d(w, w, x, x)
    def wwxy: ConstVec4d = new ConstVec4d(w, w, x, y)
    def wwxz: ConstVec4d = new ConstVec4d(w, w, x, z)
    def wwxw: ConstVec4d = new ConstVec4d(w, w, x, w)
    def wwyx: ConstVec4d = new ConstVec4d(w, w, y, x)
    def wwyy: ConstVec4d = new ConstVec4d(w, w, y, y)
    def wwyz: ConstVec4d = new ConstVec4d(w, w, y, z)
    def wwyw: ConstVec4d = new ConstVec4d(w, w, y, w)
    def wwzx: ConstVec4d = new ConstVec4d(w, w, z, x)
    def wwzy: ConstVec4d = new ConstVec4d(w, w, z, y)
    def wwzz: ConstVec4d = new ConstVec4d(w, w, z, z)
    def wwzw: ConstVec4d = new ConstVec4d(w, w, z, w)
    def wwwx: ConstVec4d = new ConstVec4d(w, w, w, x)
    def wwwy: ConstVec4d = new ConstVec4d(w, w, w, y)
    def wwwz: ConstVec4d = new ConstVec4d(w, w, w, z)
    def wwww: ConstVec4d = new ConstVec4d(w, w, w, w)

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

final class ConstVec4d private[math] (
    val x: Double, val y: Double, val z: Double, val w: Double
) extends AnyVec4d

object ConstVec4d {
    def apply(x: Double, y: Double, z: Double, w: Double) = {
        new ConstVec4d(x, y, z, w)
    }
    def apply(u: AnyVec4d) = new ConstVec4d(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4d) = new ConstVec4d(u.x, u.y, u.z, u.w)
}


final class Vec4d private[math] (
    var x: Double, var y: Double, var z: Double, var w: Double
) extends AnyVec4d
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }
    def b_=(b: Double) { z = b }
    def a_=(a: Double) { w = a }


    def *=(s: Double) { x *= s; y *= s; z *= s; w *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv; z *= inv; w *= inv }

    def +=(u: AnyVec4d) { x += u.x; y += u.y; z += u.z; w += u.w }
    def -=(u: AnyVec4d) { x -= u.x; y -= u.y; z -= u.z; w -= u.w }
    def *=(u: AnyVec4d) { x *= u.x; y *= u.y; z *= u.z; w *= u.w }
    def /=(u: AnyVec4d) { x /= u.x; y /= u.y; z /= u.z; w /= u.w }

    def *=(m: AnyMat4d) { this := m.transposeMul(this) }

    def :=(u: AnyVec4d) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Double, y: Double, z: Double, w: Double) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Double) {
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
    override def xy: ConstVec2d = new ConstVec2d(x, y)
    override def xz: ConstVec2d = new ConstVec2d(x, z)
    override def xw: ConstVec2d = new ConstVec2d(x, w)
    override def yx: ConstVec2d = new ConstVec2d(y, x)
    override def yz: ConstVec2d = new ConstVec2d(y, z)
    override def yw: ConstVec2d = new ConstVec2d(y, w)
    override def zx: ConstVec2d = new ConstVec2d(z, x)
    override def zy: ConstVec2d = new ConstVec2d(z, y)
    override def zw: ConstVec2d = new ConstVec2d(z, w)
    override def wx: ConstVec2d = new ConstVec2d(w, x)
    override def wy: ConstVec2d = new ConstVec2d(w, y)
    override def wz: ConstVec2d = new ConstVec2d(w, z)

    override def xyz: ConstVec3d = new ConstVec3d(x, y, z)
    override def xyw: ConstVec3d = new ConstVec3d(x, y, w)
    override def xzy: ConstVec3d = new ConstVec3d(x, z, y)
    override def xzw: ConstVec3d = new ConstVec3d(x, z, w)
    override def xwy: ConstVec3d = new ConstVec3d(x, w, y)
    override def xwz: ConstVec3d = new ConstVec3d(x, w, z)
    override def yxz: ConstVec3d = new ConstVec3d(y, x, z)
    override def yxw: ConstVec3d = new ConstVec3d(y, x, w)
    override def yzx: ConstVec3d = new ConstVec3d(y, z, x)
    override def yzw: ConstVec3d = new ConstVec3d(y, z, w)
    override def ywx: ConstVec3d = new ConstVec3d(y, w, x)
    override def ywz: ConstVec3d = new ConstVec3d(y, w, z)
    override def zxy: ConstVec3d = new ConstVec3d(z, x, y)
    override def zxw: ConstVec3d = new ConstVec3d(z, x, w)
    override def zyx: ConstVec3d = new ConstVec3d(z, y, x)
    override def zyw: ConstVec3d = new ConstVec3d(z, y, w)
    override def zwx: ConstVec3d = new ConstVec3d(z, w, x)
    override def zwy: ConstVec3d = new ConstVec3d(z, w, y)
    override def wxy: ConstVec3d = new ConstVec3d(w, x, y)
    override def wxz: ConstVec3d = new ConstVec3d(w, x, z)
    override def wyx: ConstVec3d = new ConstVec3d(w, y, x)
    override def wyz: ConstVec3d = new ConstVec3d(w, y, z)
    override def wzx: ConstVec3d = new ConstVec3d(w, z, x)
    override def wzy: ConstVec3d = new ConstVec3d(w, z, y)

    override def xyzw: ConstVec4d = new ConstVec4d(x, y, z, w)
    override def xywz: ConstVec4d = new ConstVec4d(x, y, w, z)
    override def xzyw: ConstVec4d = new ConstVec4d(x, z, y, w)
    override def xzwy: ConstVec4d = new ConstVec4d(x, z, w, y)
    override def xwyz: ConstVec4d = new ConstVec4d(x, w, y, z)
    override def xwzy: ConstVec4d = new ConstVec4d(x, w, z, y)
    override def yxzw: ConstVec4d = new ConstVec4d(y, x, z, w)
    override def yxwz: ConstVec4d = new ConstVec4d(y, x, w, z)
    override def yzxw: ConstVec4d = new ConstVec4d(y, z, x, w)
    override def yzwx: ConstVec4d = new ConstVec4d(y, z, w, x)
    override def ywxz: ConstVec4d = new ConstVec4d(y, w, x, z)
    override def ywzx: ConstVec4d = new ConstVec4d(y, w, z, x)
    override def zxyw: ConstVec4d = new ConstVec4d(z, x, y, w)
    override def zxwy: ConstVec4d = new ConstVec4d(z, x, w, y)
    override def zyxw: ConstVec4d = new ConstVec4d(z, y, x, w)
    override def zywx: ConstVec4d = new ConstVec4d(z, y, w, x)
    override def zwxy: ConstVec4d = new ConstVec4d(z, w, x, y)
    override def zwyx: ConstVec4d = new ConstVec4d(z, w, y, x)
    override def wxyz: ConstVec4d = new ConstVec4d(w, x, y, z)
    override def wxzy: ConstVec4d = new ConstVec4d(w, x, z, y)
    override def wyxz: ConstVec4d = new ConstVec4d(w, y, x, z)
    override def wyzx: ConstVec4d = new ConstVec4d(w, y, z, x)
    override def wzxy: ConstVec4d = new ConstVec4d(w, z, x, y)
    override def wzyx: ConstVec4d = new ConstVec4d(w, z, y, x)

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


    def xy_=(u: AnyVec2d) { x = u.x; y = u.y }
    def xz_=(u: AnyVec2d) { x = u.x; z = u.y }
    def xw_=(u: AnyVec2d) { x = u.x; w = u.y }
    def yx_=(u: AnyVec2d) { y = u.x; x = u.y }
    def yz_=(u: AnyVec2d) { y = u.x; z = u.y }
    def yw_=(u: AnyVec2d) { y = u.x; w = u.y }
    def zx_=(u: AnyVec2d) { z = u.x; x = u.y }
    def zy_=(u: AnyVec2d) { z = u.x; y = u.y }
    def zw_=(u: AnyVec2d) { z = u.x; w = u.y }
    def wx_=(u: AnyVec2d) { w = u.x; x = u.y }
    def wy_=(u: AnyVec2d) { w = u.x; y = u.y }
    def wz_=(u: AnyVec2d) { w = u.x; z = u.y }

    def xyz_=(u: AnyVec3d) { x = u.x; y = u.y; z = u.z }
    def xyw_=(u: AnyVec3d) { x = u.x; y = u.y; w = u.z }
    def xzy_=(u: AnyVec3d) { x = u.x; z = u.y; y = u.z }
    def xzw_=(u: AnyVec3d) { x = u.x; z = u.y; w = u.z }
    def xwy_=(u: AnyVec3d) { x = u.x; w = u.y; y = u.z }
    def xwz_=(u: AnyVec3d) { x = u.x; w = u.y; z = u.z }
    def yxz_=(u: AnyVec3d) { y = u.x; x = u.y; z = u.z }
    def yxw_=(u: AnyVec3d) { y = u.x; x = u.y; w = u.z }
    def yzx_=(u: AnyVec3d) { y = u.x; z = u.y; x = u.z }
    def yzw_=(u: AnyVec3d) { y = u.x; z = u.y; w = u.z }
    def ywx_=(u: AnyVec3d) { y = u.x; w = u.y; x = u.z }
    def ywz_=(u: AnyVec3d) { y = u.x; w = u.y; z = u.z }
    def zxy_=(u: AnyVec3d) { z = u.x; x = u.y; y = u.z }
    def zxw_=(u: AnyVec3d) { z = u.x; x = u.y; w = u.z }
    def zyx_=(u: AnyVec3d) { z = u.x; y = u.y; x = u.z }
    def zyw_=(u: AnyVec3d) { z = u.x; y = u.y; w = u.z }
    def zwx_=(u: AnyVec3d) { z = u.x; w = u.y; x = u.z }
    def zwy_=(u: AnyVec3d) { z = u.x; w = u.y; y = u.z }
    def wxy_=(u: AnyVec3d) { w = u.x; x = u.y; y = u.z }
    def wxz_=(u: AnyVec3d) { w = u.x; x = u.y; z = u.z }
    def wyx_=(u: AnyVec3d) { w = u.x; y = u.y; x = u.z }
    def wyz_=(u: AnyVec3d) { w = u.x; y = u.y; z = u.z }
    def wzx_=(u: AnyVec3d) { w = u.x; z = u.y; x = u.z }
    def wzy_=(u: AnyVec3d) { w = u.x; z = u.y; y = u.z }

    def xyzw_=(u: AnyVec4d) { x = u.x; y = u.y; z = u.z; w = u.w }
    def xywz_=(u: AnyVec4d) { x = u.x; y = u.y; var t = u.w; w = u.z; z = t }
    def xzyw_=(u: AnyVec4d) { x = u.x; var t = u.z; z = u.y; y = t; w = u.w }
    def xzwy_=(u: AnyVec4d) { x = u.x; var t = u.z; z = u.y; y = u.w; w = t }
    def xwyz_=(u: AnyVec4d) { x = u.x; var t = u.w; w = u.y; y = u.z; z = t }
    def xwzy_=(u: AnyVec4d) { x = u.x; var t = u.w; w = u.y; y = t; z = u.z }
    def yxzw_=(u: AnyVec4d) { var t = u.y; y = u.x; x = t; z = u.z; w = u.w }
    def yxwz_=(u: AnyVec4d) { var t = u.y; y = u.x; x = t; t = u.w; w = u.z;z=t}
    def yzxw_=(u: AnyVec4d) { var t = u.y; y = u.x; x = u.z; z = t; w = u.w }
    def yzwx_=(u: AnyVec4d) { var t = u.y; y = u.x; x = u.w; w = u.z; z = t }
    def ywxz_=(u: AnyVec4d) { var t = u.y; y = u.x; x = u.z; z = u.w; w = t }
    def ywzx_=(u: AnyVec4d) { var t = u.y; y = u.x; x = u.w; w = t; z = u.z }
    def zxyw_=(u: AnyVec4d) { var t = u.z; z = u.x; x = u.y; y = t; w = u.w }
    def zxwy_=(u: AnyVec4d) { var t = u.z; z = u.x; x = u.y; y = u.w; w = t }
    def zyxw_=(u: AnyVec4d) { var t = u.z; z = u.x; x = t; y = u.y; w = u.w }
    def zywx_=(u: AnyVec4d) { var t = u.z; z = u.x; x = u.w; w = t; y = u.y }
    def zwxy_=(u: AnyVec4d) { var t = u.z; z = u.x; x = t; t = u.w; w = u.y;y=t}
    def zwyx_=(u: AnyVec4d) { var t = u.z; z = u.x; x = u.w; w = u.y; y = t }
    def wxyz_=(u: AnyVec4d) { var t = u.w; w = u.x; x = u.y; y = u.z; z = t }
    def wxzy_=(u: AnyVec4d) { var t = u.w; w = u.x; x = u.y; y = t; z = u.z }
    def wyxz_=(u: AnyVec4d) { var t = u.w; w = u.x; x = u.z; z = t; y = u.y }
    def wyzx_=(u: AnyVec4d) { var t = u.w; w = u.x; x = t; y = u.y; z = u.z }
    def wzxy_=(u: AnyVec4d) { var t = u.w; w = u.x; x = u.z; z = u.y; y = t }
    def wzyx_=(u: AnyVec4d) { var t = u.w; w = u.x; x = t; t = u.z; z = u.y;y=t}

    def rg_=(u: AnyVec2d) { xy_=(u) }
    def rb_=(u: AnyVec2d) { xz_=(u) }
    def ra_=(u: AnyVec2d) { xw_=(u) }
    def gr_=(u: AnyVec2d) { yx_=(u) }
    def gb_=(u: AnyVec2d) { yz_=(u) }
    def ga_=(u: AnyVec2d) { yw_=(u) }
    def br_=(u: AnyVec2d) { zx_=(u) }
    def bg_=(u: AnyVec2d) { zy_=(u) }
    def ba_=(u: AnyVec2d) { zw_=(u) }
    def ar_=(u: AnyVec2d) { wx_=(u) }
    def ag_=(u: AnyVec2d) { wy_=(u) }
    def ab_=(u: AnyVec2d) { wz_=(u) }

    def rgb_=(u: AnyVec3d) { xyz_=(u) }
    def rga_=(u: AnyVec3d) { xyw_=(u) }
    def rbg_=(u: AnyVec3d) { xzy_=(u) }
    def rba_=(u: AnyVec3d) { xzw_=(u) }
    def rag_=(u: AnyVec3d) { xwy_=(u) }
    def rab_=(u: AnyVec3d) { xwz_=(u) }
    def grb_=(u: AnyVec3d) { yxz_=(u) }
    def gra_=(u: AnyVec3d) { yxw_=(u) }
    def gbr_=(u: AnyVec3d) { yzx_=(u) }
    def gba_=(u: AnyVec3d) { yzw_=(u) }
    def gar_=(u: AnyVec3d) { ywx_=(u) }
    def gab_=(u: AnyVec3d) { ywz_=(u) }
    def brg_=(u: AnyVec3d) { zxy_=(u) }
    def bra_=(u: AnyVec3d) { zxw_=(u) }
    def bgr_=(u: AnyVec3d) { zyx_=(u) }
    def bga_=(u: AnyVec3d) { zyw_=(u) }
    def bar_=(u: AnyVec3d) { zwx_=(u) }
    def bag_=(u: AnyVec3d) { zwy_=(u) }
    def arg_=(u: AnyVec3d) { wxy_=(u) }
    def arb_=(u: AnyVec3d) { wxz_=(u) }
    def agr_=(u: AnyVec3d) { wyx_=(u) }
    def agb_=(u: AnyVec3d) { wyz_=(u) }
    def abr_=(u: AnyVec3d) { wzx_=(u) }
    def abg_=(u: AnyVec3d) { wzy_=(u) }

    def rgba_=(u: AnyVec4d) { xyzw_=(u) }
    def rgab_=(u: AnyVec4d) { xywz_=(u) }
    def rbga_=(u: AnyVec4d) { xzyw_=(u) }
    def rbag_=(u: AnyVec4d) { xzwy_=(u) }
    def ragb_=(u: AnyVec4d) { xwyz_=(u) }
    def rabg_=(u: AnyVec4d) { xwzy_=(u) }
    def grba_=(u: AnyVec4d) { yxzw_=(u) }
    def grab_=(u: AnyVec4d) { yxwz_=(u) }
    def gbra_=(u: AnyVec4d) { yzxw_=(u) }
    def gbar_=(u: AnyVec4d) { yzwx_=(u) }
    def garb_=(u: AnyVec4d) { ywxz_=(u) }
    def gabr_=(u: AnyVec4d) { ywzx_=(u) }
    def brga_=(u: AnyVec4d) { zxyw_=(u) }
    def brag_=(u: AnyVec4d) { zxwy_=(u) }
    def bgra_=(u: AnyVec4d) { zyxw_=(u) }
    def bgar_=(u: AnyVec4d) { zywx_=(u) }
    def barg_=(u: AnyVec4d) { zwxy_=(u) }
    def bagr_=(u: AnyVec4d) { zwyx_=(u) }
    def argb_=(u: AnyVec4d) { wxyz_=(u) }
    def arbg_=(u: AnyVec4d) { wxzy_=(u) }
    def agrb_=(u: AnyVec4d) { wyxz_=(u) }
    def agbr_=(u: AnyVec4d) { wyzx_=(u) }
    def abrg_=(u: AnyVec4d) { wzxy_=(u) }
    def abgr_=(u: AnyVec4d) { wzyx_=(u) }
}

object Vec4d {
    val Origin = new ConstVec4d(0, 0, 0, 0)
    val UnitX = new ConstVec4d(1, 0, 0, 0)
    val UnitY = new ConstVec4d(0, 1, 0, 0)
    val UnitZ = new ConstVec4d(0, 0, 1, 0)
    val UnitW = new ConstVec4d(0, 0, 0, 1)

    def apply(s: Double) =
        new Vec4d(s, s, s, s)

    def apply(x: Double, y: Double, z: Double, w: Double) =
        new Vec4d(x, y, z, w)

    def apply(u: AnyVec4d) =
        new Vec4d(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2d, z: Double, w: Double) =
        new Vec4d(xy.x, xy.y, z, w)

    def apply(x: Double, yz: AnyVec2d, w: Double) =
        new Vec4d(x, yz.x, yz.y, w)

    def apply(x: Double, y: Double, zw: AnyVec2d) =
        new Vec4d(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2d, zw: AnyVec2d) =
        new Vec4d(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3d, w: Double) =
        new Vec4d(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Double, yzw: AnyVec3d) =
        new Vec4d(x, yzw.x, yzw.y, yzw.z)

    def apply(m: AnyMat2d) =
        new Vec4d(m.m00, m.m10, m.m01, m.m11)

    def apply(u: Read4Int) =
        new Vec4d(u.x, u.y, u.z, u.w)

    def apply(u: Read4Float) =
        new Vec4d(u.x, u.y, u.z, u.w)

    implicit def constToMutable(u: ConstVec4d) = Vec4d(u)
}
