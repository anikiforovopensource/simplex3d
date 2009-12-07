/*
 * Simplex3D, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2f extends Read2Float {

    def x: Float
    def y: Float

    def r = x
    def g = y

    def s = x
    def t = y

    
    def apply(i: Int) :Float = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def unary_-() = Vec2f(-x, -y)
    def *(s: Float) = Vec2f(x*s, y*s)
    def /(s: Float) = { val inv = 1/s; Vec2f(x*inv, y*inv) }
    private[math] def divByComponent(s: Float) = Vec2f(s/x, s/y)

    def +(u: AnyVec2f) = Vec2f(x + u.x, y + u.y)
    def -(u: AnyVec2f) = Vec2f(x - u.x, y - u.y)
    def *(u: AnyVec2f) = Vec2f(x * u.x, y * u.y)
    def /(u: AnyVec2f) = Vec2f(x / u.x, y / u.y)

    def *(m: AnyMat2f) :Vec2f = m.transposeMul(x, y, new Vec2f)
    def *(m: AnyMat2x3f) :Vec3f = m.transposeMul(x, y, new Vec3f)
    def *(m: AnyMat2x4f) :Vec4f = m.transposeMul(x, y, new Vec4f)

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

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2f private[math] (val x: Float, val y: Float)
extends AnyVec2f


final class Vec2f private[math] (var x: Float, var y: Float) extends AnyVec2f {
    private[math] def this() = this(0, 0)
    
    override def r = x
    override def g = y

    override def s = x
    override def t = y

    def r_=(r: Float) { x = r }
    def g_=(g: Float) { y = g }

    def s_=(s: Float) { x = s }
    def t_=(t: Float) { y = t }

    
    def *=(s: Float) { x *= s; y *= s }
    def /=(s: Float) { val inv = 1/s; x *= inv; y *= inv }

    def +=(u: AnyVec2f) { x += u.x; y += u.y }
    def -=(u: AnyVec2f) { x -= u.x; y -= u.y }
    def *=(u: AnyVec2f) { x *= u.x; y *= u.y }
    def /=(u: AnyVec2f) { x /= u.x; y /= u.y }

    def *=(m: AnyMat2f) { m.transposeMul(x, y, this) }

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
}

object Vec2f {
    val Origin = const(Vec2f(0))
    val UnitX = const(Vec2f(1, 0))
    val UnitY = const(Vec2f(0, 1))

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
    implicit def vec2ToSwizzled(u: Vec2f) = new Vec2fSwizzled(u)
}

private[math] class ConstVec2fSwizzled(u: AnyVec2f) extends FloatVecFactory
with Swizzle2Read[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    def x = u.x
    def y = u.y
}

private[math] class Vec2fSwizzled(u: Vec2f) extends ConstVec2fSwizzled(u)
with Swizzle2Write[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    def x_=(x: Float) { u.x = x }
    def y_=(y: Float) { u.y = y }
}
