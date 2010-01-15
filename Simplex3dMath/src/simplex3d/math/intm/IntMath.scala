/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object IntMath {

    // Int functions
    def abs(x: Int) :Int = if (x < 0) -x else x
    def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0
    def min(x: Int, y: Int) :Int = if (x < y) x else y
    def max(x: Int, y: Int) :Int = if (x > y) x else y
    def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
        if (x <= minVal) minVal
        else if (x >= maxVal) maxVal
        else x
    }

    // Vec2i functions
    def abs(u: AnyVec2i) :Vec2i = new Vec2i(abs(u.x), abs(u.y))
    def sign(u: AnyVec2i) :Vec2i = new Vec2i(sign(u.x), sign(u.y))
    def min(u: AnyVec2i, s: Int) :Vec2i = new Vec2i(min(u.x, s), min(u.y, s))
    def min(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        new Vec2i(min(u.x, v.x), min(u.y, v.y))
    }
    def max(u: AnyVec2i, s: Int) :Vec2i = new Vec2i(max(u.x, s), max(u.y, s))
    def max(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        new Vec2i(max(u.x, v.x), max(u.y, v.y))
    }
    def clamp(u: AnyVec2i, minVal: Int, maxVal: Int) :Vec2i = {
        new Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
    }
    def clamp(u: AnyVec2i, minVal: AnyVec2i, maxVal: AnyVec2i) :Vec2i = {
        new Vec2i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y)
        )
    }
    
    def lessThan(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x < v.x,
            u.y < v.y
        )
    }
    def lessThanEqual(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x <= v.x,
            u.y <= v.y
        )
    }
    def greaterThan(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x > v.x,
            u.y > v.y
        )
    }
    def greaterThanEqual(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x >= v.x,
            u.y >= v.y
        )
    }
    def equal(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x == v.x,
            u.y == v.y
        )
    }
    def notEqual(u: AnyVec2i, v: AnyVec2i) :Vec2b = {
        new Vec2b(
            u.x != v.x,
            u.y != v.y
        )
    }

    // Vec3i functions
    def abs(u: AnyVec3i) :Vec3i = new Vec3i(abs(u.x), abs(u.y), abs(u.z))
    def sign(u: AnyVec3i) :Vec3i = new Vec3i(sign(u.x), sign(u.y), sign(u.z))
    def min(u: AnyVec3i, s: Int) :Vec3i = {
        new Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
    }
    def min(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        new Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
    }
    def max(u: AnyVec3i, s: Int) :Vec3i = {
        new Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
    }
    def max(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        new Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
    }
    def clamp(u: AnyVec3i, minVal: Int, maxVal: Int) :Vec3i = {
        new Vec3i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec3i, minVal: AnyVec3i, maxVal: AnyVec3i) :Vec3i = {
        new Vec3i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z)
        )
    }

    def lessThan(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z
        )
    }
    def lessThanEqual(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z
        )
    }
    def greaterThan(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z
        )
    }
    def greaterThanEqual(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z
        )
    }
    def equal(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z
        )
    }
    def notEqual(u: AnyVec3i, v: AnyVec3i) :Vec3b = {
        new Vec3b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z
        )
    }

    // Vec4i functions
    def abs(u: AnyVec4i) :Vec4i = {
        new Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
    }
    def sign(u: AnyVec4i) :Vec4i = {
        new Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
    }
    def min(u: AnyVec4i, s: Int) :Vec4i = {
        new Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
    }
    def min(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        new Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
    }
    def max(u: AnyVec4i, s: Int) :Vec4i = {
        new Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
    }
    def max(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        new Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
    }
    def clamp(u: AnyVec4i, minVal: Int, maxVal: Int) :Vec4i = {
        new Vec4i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal),
            clamp(u.w, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec4i, minVal: AnyVec4i, maxVal: AnyVec4i) :Vec4i = {
        new Vec4i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z),
            clamp(u.w, minVal.w, maxVal.w)
        )
    }

    def lessThan(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x < v.x,
            u.y < v.y,
            u.z < v.z,
            u.w < v.w
        )
    }
    def lessThanEqual(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x <= v.x,
            u.y <= v.y,
            u.z <= v.z,
            u.w <= v.w
        )
    }
    def greaterThan(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z,
            u.w > v.w
        )
    }
    def greaterThanEqual(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x >= v.x,
            u.y >= v.y,
            u.z >= v.z,
            u.w >= v.w
        )
    }
    def equal(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x == v.x,
            u.y == v.y,
            u.z == v.z,
            u.w == v.w
        )
    }
    def notEqual(u: AnyVec4i, v: AnyVec4i) :Vec4b = {
        new Vec4b(
            u.x != v.x,
            u.y != v.y,
            u.z != v.z,
            u.w != v.w
        )
    }
}
