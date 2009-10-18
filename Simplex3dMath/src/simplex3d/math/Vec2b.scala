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

import simplex3d.math.VecMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2b extends Read2[Boolean] {

    def x: Boolean
    def y: Boolean


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

class ConstVec2bSwizzled(u: AnyVec2b) extends VecbSwizzled
with Swizzle2Read[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    def x = u.x
    def y = u.y
}

class Vec2bSwizzled(u: Vec2b) extends ConstVec2bSwizzled(u)
with Swizzle2Write[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    def x_=(x: Boolean) { u.x = x }
    def y_=(y: Boolean) { u.y = y }
}
