/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2d extends Read2Double {

    def r = x
    def g = y

    
    def apply(i: Int) :Double = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = new Vec2d(-x, -y)
    def *(s: Double) = new Vec2d(x * s, y * s)
    def /(s: Double) = { val inv = 1/s; new Vec2d(x * inv, y * inv) }
    private[math] def divideByComponent(s: Double) = new Vec2d(s / x, s / y)

    def +(u: AnyVec2d) = new Vec2d(x + u.x, y + u.y)
    def -(u: AnyVec2d) = new Vec2d(x - u.x, y - u.y)
    def *(u: AnyVec2d) = new Vec2d(x * u.x, y * u.y)
    def /(u: AnyVec2d) = new Vec2d(x / u.x, y / u.y)

    def *(m: AnyMat2d) :Vec2d = m.transposeMul(this)
    def *(m: AnyMat2x3d) :Vec3d = m.transposeMul(this)
    def *(m: AnyMat2x4d) :Vec4d = m.transposeMul(this)

    def ==(u: AnyVec2d) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2d) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Double._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec2d => this == u
            case _ => false
        }
    }

    override def hashCode :Int = {
        41 * (
            41 + x.hashCode
        ) + y.hashCode
    }

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
    
    // Swizzling
    def xx: ConstVec2d = new ConstVec2d(x, x)
    def xy: ConstVec2d = new ConstVec2d(x, y)
    def yx: ConstVec2d = new ConstVec2d(y, x)
    def yy: ConstVec2d = new ConstVec2d(y, y)

    def xxx: ConstVec3d = new ConstVec3d(x, x, x)
    def xxy: ConstVec3d = new ConstVec3d(x, x, y)
    def xyx: ConstVec3d = new ConstVec3d(x, y, x)
    def xyy: ConstVec3d = new ConstVec3d(x, y, y)
    def yxx: ConstVec3d = new ConstVec3d(y, x, x)
    def yxy: ConstVec3d = new ConstVec3d(y, x, y)
    def yyx: ConstVec3d = new ConstVec3d(y, y, x)
    def yyy: ConstVec3d = new ConstVec3d(y, y, y)

    def xxxx: ConstVec4d = new ConstVec4d(x, x, x, x)
    def xxxy: ConstVec4d = new ConstVec4d(x, x, x, y)
    def xxyx: ConstVec4d = new ConstVec4d(x, x, y, x)
    def xxyy: ConstVec4d = new ConstVec4d(x, x, y, y)
    def xyxx: ConstVec4d = new ConstVec4d(x, y, x, x)
    def xyxy: ConstVec4d = new ConstVec4d(x, y, x, y)
    def xyyx: ConstVec4d = new ConstVec4d(x, y, y, x)
    def xyyy: ConstVec4d = new ConstVec4d(x, y, y, y)
    def yxxx: ConstVec4d = new ConstVec4d(y, x, x, x)
    def yxxy: ConstVec4d = new ConstVec4d(y, x, x, y)
    def yxyx: ConstVec4d = new ConstVec4d(y, x, y, x)
    def yxyy: ConstVec4d = new ConstVec4d(y, x, y, y)
    def yyxx: ConstVec4d = new ConstVec4d(y, y, x, x)
    def yyxy: ConstVec4d = new ConstVec4d(y, y, x, y)
    def yyyx: ConstVec4d = new ConstVec4d(y, y, y, x)
    def yyyy: ConstVec4d = new ConstVec4d(y, y, y, y)

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
}

final class ConstVec2d private[math] (val x: Double, val y: Double)
extends AnyVec2d

object ConstVec2d {
    def apply(x: Double, y: Double) = new ConstVec2d(x, y)
    def apply(u: AnyVec2d) = new ConstVec2d(u.x, u.y)

    implicit def mutableToConst(u: Vec2d) = new ConstVec2d(u.x, u.y)
}


final class Vec2d private[math] (var x: Double, var y: Double)
extends AnyVec2d
{

    override def r = x
    override def g = y

    def r_=(r: Double) { x = r }
    def g_=(g: Double) { y = g }

    
    def *=(s: Double) { x *= s; y *= s }
    def /=(s: Double) { val inv = 1/s; x *= inv; y *= inv }

    def +=(u: AnyVec2d) { x += u.x; y += u.y }
    def -=(u: AnyVec2d) { x -= u.x; y -= u.y }
    def *=(u: AnyVec2d) { x *= u.x; y *= u.y }
    def /=(u: AnyVec2d) { x /= u.x; y /= u.y }

    def *=(m: AnyMat2d) { this := m.transposeMul(this) }

    def :=(u: AnyVec2d) { x = u.x; y = u.y }
    def set(x: Double, y: Double) { this.x = x; this.y = y }

    def update(i: Int, s: Double) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2d = new ConstVec2d(x, y)
    override def yx: ConstVec2d = new ConstVec2d(y, x)

    override def rg = xy
    override def gr = yx

    def xy_=(u: AnyVec2d) { x = u.x; y = u.y }
    def yx_=(u: AnyVec2d) { var t = u.y; y = u.x; x = t }

    def rg_=(u: AnyVec2d) { xy_=(u) }
    def gr_=(u: AnyVec2d) { yx_=(u) }
}

object Vec2d {
    val Origin = new ConstVec2d(0, 0)
    val UnitX = new ConstVec2d(1, 0)
    val UnitY = new ConstVec2d(0, 1)

    def apply(s: Double) = new Vec2d(s, s)
    def apply(x: Double, y: Double) = new Vec2d(x, y)
    def apply(u: AnyVec2d) = new Vec2d(u.x, u.y)
    def apply(u: AnyVec3d) = new Vec2d(u.x, u.y)
    def apply(u: AnyVec4d) = new Vec2d(u.x, u.y)
    def apply(u: AnyVec2b) = new Vec2d(double(u.x), double(u.y))
    def apply(u: AnyVec3b) = new Vec2d(double(u.x), double(u.y))
    def apply(u: AnyVec4b) = new Vec2d(double(u.x), double(u.y))
    def apply(u: Read2Int) = new Vec2d(u.x, u.y)
    def apply(u: Read3Int) = new Vec2d(u.x, u.y)
    def apply(u: Read4Int) = new Vec2d(u.x, u.y)
    def apply(u: Read2Float) = new Vec2d(u.x, u.y)
    def apply(u: Read3Float) = new Vec2d(u.x, u.y)
    def apply(u: Read4Float) = new Vec2d(u.x, u.y)

    implicit def constToMutable(u: ConstVec2d) = Vec2d(u)
}
