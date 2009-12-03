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

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2b extends Read2[Boolean] {

    def x: Boolean
    def y: Boolean

    def r = x
    def g = y

    def s = x
    def t = y


    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }

    def ==(u: AnyVec2b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y
    }

    def !=(u: AnyVec2b) :Boolean = !(this == u)

    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ")"
    }
}

final class ConstVec2b private (val x: Boolean, val y: Boolean) extends AnyVec2b

object ConstVec2b {
    def apply(s: Boolean) = new ConstVec2b(s, s)
    def apply(x: Boolean, y: Boolean) = new ConstVec2b(x, y)
    def apply(u: AnyVec2b) = new ConstVec2b(u.x, u.y)
    def apply(u: AnyVec3b) = new ConstVec2b(u.x, u.y)
    def apply(u: AnyVec4b) = new ConstVec2b(u.x, u.y)

    implicit def mutableToConst(u: Vec2b) = ConstVec2b(u)
    implicit def constVec2bToSwizzled(u: ConstVec2b) = new ConstVec2bSwizzled(u)
}

final class Vec2b private (var x: Boolean, var y: Boolean) extends AnyVec2b {

    override def r = x
    override def g = y

    override def s = x
    override def t = y

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }

    def s_=(s: Boolean) { x = s }
    def t_=(t: Boolean) { y = t }


    def :=(u: AnyVec2b) { x = u.x; y = u.y }
    def set(x: Boolean, y: Boolean) { this.x = x; this.y = y }

    def update(i: Int, s: Boolean) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 1, got " + j)
        }
    }
}

object Vec2b {
    def apply(s: Boolean) = new Vec2b(s, s)
    def apply(x: Boolean, y: Boolean) = new Vec2b(x, y)
    def apply(u: AnyVec2b) = new Vec2b(u.x, u.y)
    def apply(u: AnyVec3b) = new Vec2b(u.x, u.y)
    def apply(u: AnyVec4b) = new Vec2b(u.x, u.y)

    implicit def vec2bToSwizzled(u: Vec2b) = new Vec2bSwizzled(u)
}

private[math] class ConstVec2bSwizzled(u: AnyVec2b) extends BooleanVecFactory
with Swizzle2Read[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    def x = u.x
    def y = u.y
}

private[math] class Vec2bSwizzled(u: Vec2b) extends ConstVec2bSwizzled(u)
with Swizzle2Write[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    def x_=(x: Boolean) { u.x = x }
    def y_=(y: Boolean) { u.y = y }
}
