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
sealed abstract class AnyVec3b extends Read3[Boolean] {

    def x: Boolean
    def y: Boolean
    def z: Boolean

    def r = x
    def g = y
    def b = z

    def s = x
    def t = y
    def p = z


    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }

    def ==(u: AnyVec3b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z
    }

    def !=(u: AnyVec3b) :Boolean = !(this == u)


    override def toString = {
        this.getClass.getSimpleName + "(" + x + ", " + y + ", " + z + ")"
    }

}

final class ConstVec3b private (val x: Boolean, val y: Boolean, val z: Boolean)
extends AnyVec3b

object ConstVec3b {
    def apply(s: Boolean) = new ConstVec3b(s, s, s)
    def apply(x: Boolean, y: Boolean, z: Boolean) = new ConstVec3b(x, y, z)
    def apply(u: AnyVec3b) = new ConstVec3b(u.x, u.y, u.z)
    def apply(u: AnyVec4b) = new ConstVec3b(u.x, u.y, u.z)
    def apply(xy: AnyVec2b, z: Boolean) = new ConstVec3b(xy.x, xy.y, z)
    def apply(x: Boolean, yz: AnyVec2b) = new ConstVec3b(x, yz.x, yz.y)

    implicit def mutableToConst(u: Vec3b) = ConstVec3b(u)
    implicit def constVec3bToSwizzled(u: ConstVec3b) = new ConstVec3bSwizzled(u)
}

final class Vec3b private (var x: Boolean, var y: Boolean, var z: Boolean)
extends AnyVec3b
{
    override def r = x
    override def g = y
    override def b = z

    override def s = x
    override def t = y
    override def p = z

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }
    def b_=(b: Boolean) { z = b }

    def s_=(s: Boolean) { x = s }
    def t_=(t: Boolean) { y = t }
    def p_=(p: Boolean) { z = p }


    def :=(u: AnyVec3b) { x = u.x; y = u.y; z = u.z }
    def set(x: Boolean, y: Boolean, z: Boolean) {
        this.x = x; this.y = y; this.z = z
    }

    def update(i: Int, s: Boolean) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 2, got " + j)
        }
    }
}

object Vec3b {
    def apply(s: Boolean) = new Vec3b(s, s, s)
    def apply(x: Boolean, y: Boolean, z: Boolean) = new Vec3b(x, y, z)
    def apply(u: AnyVec3b) = new Vec3b(u.x, u.y, u.z)
    def apply(u: AnyVec4b) = new Vec3b(u.x, u.y, u.z)
    def apply(xy: AnyVec2b, z: Boolean) = new Vec3b(xy.x, xy.y, z)
    def apply(x: Boolean, yz: AnyVec2b) = new Vec3b(x, yz.x, yz.y)

    implicit def vec3bToSwizzled(u: Vec3b) = new Vec3bSwizzled(u)
}

private[math] class ConstVec3bSwizzled(u: AnyVec3b)
extends ConstVec2bSwizzled(null)
with Swizzle3Read[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    override def x = u.x
    override def y = u.y
    def z = u.z
}

private[math] class Vec3bSwizzled(u: Vec3b) extends ConstVec3bSwizzled(u)
with Swizzle3Write[Boolean, ConstVec2b, ConstVec3b, ConstVec4b]
{
    def x_=(x: Boolean) { u.x = x }
    def y_=(y: Boolean) { u.y = y }
    def z_=(z: Boolean) { u.z = z }
}
