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
sealed abstract class AnyVec2f extends Read2Float {

    def r = x
    def g = y

    
    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = new Vec2f(-x, -y)
    def *(s: Float) = new Vec2f(x * s, y * s)
    def /(s: Float) = { val inv = 1/s; new Vec2f(x * inv, y * inv) }
    private[math] def divideByComponent(s: Float) = new Vec2f(s / x, s / y)

    def +(u: AnyVec2f) = new Vec2f(x + u.x, y + u.y)
    def -(u: AnyVec2f) = new Vec2f(x - u.x, y - u.y)
    def *(u: AnyVec2f) = new Vec2f(x * u.x, y * u.y)
    def /(u: AnyVec2f) = new Vec2f(x / u.x, y / u.y)

    def *(m: AnyMat2f) :Vec2f = m.transposeMul(this)
    def *(m: AnyMat2x3f) :Vec3f = m.transposeMul(this)
    def *(m: AnyMat2x4f) :Vec4f = m.transposeMul(this)

    def ==(u: AnyVec2f) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2f) :Boolean = !(this == u)

    private[math] def hasErrors: Boolean = {
        import java.lang.Float._
        (
            isNaN(x) || isInfinite(x) ||
            isNaN(y) || isInfinite(y)
        )
    }

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec2f => this == u
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
    def xx: ConstVec2f = new ConstVec2f(x, x)
    def xy: ConstVec2f = new ConstVec2f(x, y)
    def yx: ConstVec2f = new ConstVec2f(y, x)
    def yy: ConstVec2f = new ConstVec2f(y, y)

    def xxx: ConstVec3f = new ConstVec3f(x, x, x)
    def xxy: ConstVec3f = new ConstVec3f(x, x, y)
    def xyx: ConstVec3f = new ConstVec3f(x, y, x)
    def xyy: ConstVec3f = new ConstVec3f(x, y, y)
    def yxx: ConstVec3f = new ConstVec3f(y, x, x)
    def yxy: ConstVec3f = new ConstVec3f(y, x, y)
    def yyx: ConstVec3f = new ConstVec3f(y, y, x)
    def yyy: ConstVec3f = new ConstVec3f(y, y, y)

    def xxxx: ConstVec4f = new ConstVec4f(x, x, x, x)
    def xxxy: ConstVec4f = new ConstVec4f(x, x, x, y)
    def xxyx: ConstVec4f = new ConstVec4f(x, x, y, x)
    def xxyy: ConstVec4f = new ConstVec4f(x, x, y, y)
    def xyxx: ConstVec4f = new ConstVec4f(x, y, x, x)
    def xyxy: ConstVec4f = new ConstVec4f(x, y, x, y)
    def xyyx: ConstVec4f = new ConstVec4f(x, y, y, x)
    def xyyy: ConstVec4f = new ConstVec4f(x, y, y, y)
    def yxxx: ConstVec4f = new ConstVec4f(y, x, x, x)
    def yxxy: ConstVec4f = new ConstVec4f(y, x, x, y)
    def yxyx: ConstVec4f = new ConstVec4f(y, x, y, x)
    def yxyy: ConstVec4f = new ConstVec4f(y, x, y, y)
    def yyxx: ConstVec4f = new ConstVec4f(y, y, x, x)
    def yyxy: ConstVec4f = new ConstVec4f(y, y, x, y)
    def yyyx: ConstVec4f = new ConstVec4f(y, y, y, x)
    def yyyy: ConstVec4f = new ConstVec4f(y, y, y, y)

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

final class ConstVec2f private[math] (val x: Float, val y: Float)
extends AnyVec2f

object ConstVec2f {
    def apply(x: Float, y: Float) = new ConstVec2f(x, y)
    def apply(u: AnyVec2f) = new ConstVec2f(u.x, u.y)

    implicit def mutableToConst(u: Vec2f) = new ConstVec2f(u.x, u.y)
}

final class Vec2f private[math] (var x: Float, var y: Float) extends AnyVec2f {

    override def r = x
    override def g = y

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }

    
    def *=(s: Float) { x *= s; y *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv }

    def +=(u: AnyVec2f) { x += u.x; y += u.y }
    def -=(u: AnyVec2f) { x -= u.x; y -= u.y }
    def *=(u: AnyVec2f) { x *= u.x; y *= u.y }
    def /=(u: AnyVec2f) { x /= u.x; y /= u.y }

    def *=(m: AnyMat2f) { this := m.transposeMul(this) }

    def :=(u: AnyVec2f) { x = u.x; y = u.y }
    def set(x: Float, y: Float) { this.x = x; this.y = y }

    def update(i: Int, s: Float) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    // Swizzling
    override def xy: ConstVec2f = new ConstVec2f(x, y)
    override def yx: ConstVec2f = new ConstVec2f(y, x)

    override def rg = xy
    override def gr = yx

    def xy_=(u: AnyVec2f) { x = u.x; y = u.y }
    def yx_=(u: AnyVec2f) { var t = u.y; y = u.x; x = t }

    def rg_=(u: AnyVec2f) { xy_=(u) }
    def gr_=(u: AnyVec2f) { yx_=(u) }
}

object Vec2f {
    val Origin = new ConstVec2f(0, 0)
    val UnitX = new ConstVec2f(1, 0)
    val UnitY = new ConstVec2f(0, 1)

    def apply(s: Float) = new Vec2f(s, s)
    def apply(x: Float, y: Float) = new Vec2f(x, y)
    def apply(u: AnyVec2f) = new Vec2f(u.x, u.y)
    def apply(u: AnyVec3f) = new Vec2f(u.x, u.y)
    def apply(u: AnyVec4f) = new Vec2f(u.x, u.y)
    def apply(u: Read2Int) = new Vec2f(u.x, u.y)
    def apply(u: Read3Int) = new Vec2f(u.x, u.y)
    def apply(u: Read4Int) = new Vec2f(u.x, u.y)
    def apply(u: Read2Double) = new Vec2f(float(u.x), float(u.y))
    def apply(u: Read3Double) = new Vec2f(float(u.x), float(u.y))
    def apply(u: Read4Double) = new Vec2f(float(u.x), float(u.y))

    implicit def constToMutable(u: ConstVec2f) = Vec2f(u)
}
