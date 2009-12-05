/*
 * Simplex3D, Math module
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
    private[math] def divideByComponent(s: Float) = Vec2f(s/x, s/y)

    def +(u: AnyVec2f) = Vec2f(x + u.x, y + u.y)
    def -(u: AnyVec2f) = Vec2f(x - u.x, y - u.y)
    def *(u: AnyVec2f) = Vec2f(x * u.x, y * u.y)
    def /(u: AnyVec2f) = Vec2f(x / u.x, y / u.y)

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

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2f private (val x: Float, val y: Float) extends AnyVec2f

object ConstVec2f {
    def apply(s: Float) = new ConstVec2f(s, s)
    def apply(x: Float, y: Float) = new ConstVec2f(x, y)
    def apply(u: AnyVec2f) = new ConstVec2f(u.x, u.y)
    def apply(u: AnyVec3f) = new ConstVec2f(u.x, u.y)
    def apply(u: AnyVec4f) = new ConstVec2f(u.x, u.y)
    def apply(u: Read2Int) = new ConstVec2f(u.x, u.y)
    def apply(u: Read3Int) = new ConstVec2f(u.x, u.y)
    def apply(u: Read4Int) = new ConstVec2f(u.x, u.y)

    implicit def mutableToConst(u: Vec2f) = ConstVec2f(u)
    implicit def constVec2fToSwizzled(u: ConstVec2f) = new ConstVec2fSwizzled(u)
}

final class Vec2f private (var x: Float, var y: Float) extends AnyVec2f {

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
}

object Vec2f {
    val Origin = ConstVec2f(0)
    val UnitX = ConstVec2f(1, 0)
    val UnitY = ConstVec2f(0, 1)

    def apply(s: Float) = new Vec2f(s, s)
    def apply(x: Float, y: Float) = new Vec2f(x, y)
    def apply(u: AnyVec2f) = new Vec2f(u.x, u.y)
    def apply(u: AnyVec3f) = new Vec2f(u.x, u.y)
    def apply(u: AnyVec4f) = new Vec2f(u.x, u.y)
    def apply(u: Read2Int) = new Vec2f(u.x, u.y)
    def apply(u: Read3Int) = new Vec2f(u.x, u.y)
    def apply(u: Read4Int) = new Vec2f(u.x, u.y)

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
