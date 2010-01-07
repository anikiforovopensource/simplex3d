/*
 * Simplex3d, BaseMath module
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

package simplex3d.math

import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec2b {

    def x: Boolean
    def y: Boolean

    def r = x
    def g = y


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

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec2b => this == u
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
}

final class ConstVec2b private[math] (val x: Boolean, val y: Boolean)
extends AnyVec2b

object ConstVec2b {
    def apply(x: Boolean, y: Boolean) = new ConstVec2b(x, y)
    def apply(u: AnyVec2b) = new ConstVec2b(u.x, u.y)
    
    implicit def mutableToConst(u: Vec2b) = new ConstVec2b(u.x, u.y)
}


final class Vec2b private[math] (var x: Boolean, var y: Boolean)
extends AnyVec2b
{

    override def r = x
    override def g = y

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }


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
    val True = new ConstVec2b(true, true)
    val False = new ConstVec2b(false, false)

    def apply(s: Boolean) = new Vec2b(s, s)
    def apply(x: Boolean, y: Boolean) = new Vec2b(x, y)
    def apply(u: AnyVec2b) = new Vec2b(u.x, u.y)
    def apply(u: AnyVec3b) = new Vec2b(u.x, u.y)
    def apply(u: AnyVec4b) = new Vec2b(u.x, u.y)

    implicit def constToMutable(u: ConstVec2b) = Vec2b(u)
}
