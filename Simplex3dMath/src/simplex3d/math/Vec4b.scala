/*
 * Simplex3d, BaseMath module
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

package simplex3d.math

import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class AnyVec4b {

    def x: Boolean
    def y: Boolean
    def z: Boolean
    def w: Boolean

    def r = x
    def g = y
    def b = z
    def a = w


    def apply(i: Int) :Boolean = {
        i match {
            case 0 => x
            case 1 => y
            case 2 => z
            case 3 => w
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }

    def ==(u: AnyVec4b) :Boolean = {
        if (u eq null) false
        else x == u.x && y == u.y && z == u.z && w == u.w
    }

    def !=(u: AnyVec4b) :Boolean = !(this == u)

    override def equals(other: Any) :Boolean = {
        other match {
            case u: AnyVec4b => this == u
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
}

final class ConstVec4b private[math] (
    val x: Boolean, val y: Boolean, val z: Boolean, val w: Boolean)
extends AnyVec4b

object ConstVec4b {
    def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) = {
        new ConstVec4b(x, y, z, w)
    }
    def apply(u: AnyVec4b) = new ConstVec4b(u.x, u.y, u.z, u.w)

    implicit def mutableToConst(u: Vec4b) = new ConstVec4b(u.x, u.y, u.z, u.w)
}


final class Vec4b private[math] (
    var x: Boolean, var y: Boolean, var z: Boolean, var w: Boolean)
extends AnyVec4b
{
    override def r = x
    override def g = y
    override def b = z
    override def a = w

    def r_=(r: Boolean) { x = r }
    def g_=(g: Boolean) { y = g }
    def b_=(b: Boolean) { z = b }
    def a_=(a: Boolean) { w = a }


    def :=(u: AnyVec4b) { x = u.x; y = u.y; z = u.z; w = u.w }
    def set(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
        this.x = x; this.y = y; this.z = z; this.w = w
    }

    def update(i: Int, s: Boolean) {
        i match {
            case 0 => x = s
            case 1 => y = s
            case 2 => z = s
            case 3 => w = s
            case j => throw new IndexOutOfBoundsException(
                    "excpected from 0 to 3, got " + j)
        }
    }
}

object Vec4b {
    val True = new ConstVec4b(true, true, true, true)
    val False = new ConstVec4b(false, false, false, false)

    def apply(s: Boolean) =
        new Vec4b(s, s, s, s)

    def apply(x: Boolean, y: Boolean, z: Boolean, w: Boolean) =
        new Vec4b(x, y, z, w)

    def apply(u: AnyVec4b) =
        new Vec4b(u.x, u.y, u.z, u.w)

    def apply(xy: AnyVec2b, z: Boolean, w: Boolean) =
        new Vec4b(xy.x, xy.y, z, w)

    def apply(x: Boolean, yz: AnyVec2b, w: Boolean) =
        new Vec4b(x, yz.x, yz.y, w)

    def apply(x: Boolean, y: Boolean, zw: AnyVec2b) =
        new Vec4b(x, y, zw.x, zw.y)

    def apply(xy: AnyVec2b, zw: AnyVec2b) =
        new Vec4b(xy.x, xy.y, zw.x, zw.y)

    def apply(xyz: AnyVec3b, w: Boolean) =
        new Vec4b(xyz.x, xyz.y, xyz.z, w)

    def apply(x: Boolean, yzw: AnyVec3b) =
        new Vec4b(x, yzw.x, yzw.y, yzw.z)

    implicit def constToMutable(u: ConstVec4b) = Vec4b(u)
}
