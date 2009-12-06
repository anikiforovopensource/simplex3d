/*
 * Simplex3D, IntMath module
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

package simplex3d.math.intm

import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object IntMath {
    //Implicints
    implicit def imintToExtInt(s: Int) = new ExtendedInt(s)

    // Random
    def nextVec2() :Vec2i = Vec2i(nextInt, nextInt)
    def nextVec3() :Vec3i = Vec3i(nextInt, nextInt, nextInt)
    def nextVec4() :Vec4i = Vec4i(nextInt, nextInt, nextInt, nextInt)

    def nextVec2i(n: Int) :Vec2i = nextVec2
    def nextVec3i(n: Int) :Vec3i = nextVec3
    def nextVec4i(n: Int) :Vec4i = nextVec4

    // Int functions
    def abs(x: Int) :Int = if (x < 0) -x else x
    def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0
    def min(x: Int, y: Int) :Int = if (x < y) x else y
    def max(x: Int, y: Int) :Int = if (x > y) x else y
    def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
        if (x < minVal) minVal
        else if (x > maxVal) maxVal
        else x
    }

    // Vec2i functions
    def abs(u: AnyVec2i) :Vec2i = Vec2i(abs(u.x), abs(u.y))
    def sign(u: AnyVec2i) :Vec2i = Vec2i(sign(u.x), sign(u.y))
    def min(u: AnyVec2i, s: Int) :Vec2i = Vec2i(min(u.x, s), min(u.y, s))
    def min(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        Vec2i(min(u.x, v.x), min(u.y, v.y))
    }
    def max(u: AnyVec2i, s: Int) :Vec2i = Vec2i(max(u.x, s), max(u.y, s))
    def max(u: AnyVec2i, v: AnyVec2i) :Vec2i = {
        Vec2i(max(u.x, v.x), max(u.y, v.y))
    }
    def clamp(u: AnyVec2i, minVal: Int, maxVal: Int) :Vec2i = {
        Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
    }
    def clamp(u: AnyVec2i, minVal: AnyVec2i, maxVal: AnyVec2i) :Vec2i = {
        Vec2i(clamp(u.x, minVal.x, maxVal.x), clamp(u.y, minVal.y, maxVal.y))
    }

    // Vec3i functions
    def abs(u: AnyVec3i) :Vec3i = Vec3i(abs(u.x), abs(u.y), abs(u.z))
    def sign(u: AnyVec3i) :Vec3i = Vec3i(sign(u.x), sign(u.y), sign(u.z))
    def min(u: AnyVec3i, s: Int) :Vec3i = {
        Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
    }
    def min(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
    }
    def max(u: AnyVec3i, s: Int) :Vec3i = {
        Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
    }
    def max(u: AnyVec3i, v: AnyVec3i) :Vec3i = {
        Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
    }
    def clamp(u: AnyVec3i, minVal: Int, maxVal: Int) :Vec3i = {
        Vec3i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec3i, minVal: AnyVec3i, maxVal: AnyVec3i) :Vec3i = {
        Vec3i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z)
        )
    }

    // Vec4i functions
    def abs(u: AnyVec4i) :Vec4i = Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
    def sign(u: AnyVec4i) :Vec4i = {
        Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
    }
    def min(u: AnyVec4i, s: Int) :Vec4i = {
        Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
    }
    def min(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
    }
    def max(u: AnyVec4i, s: Int) :Vec4i = {
        Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
    }
    def max(u: AnyVec4i, v: AnyVec4i) :Vec4i = {
        Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
    }
    def clamp(u: AnyVec4i, minVal: Int, maxVal: Int) :Vec4i = {
        Vec4i(
            clamp(u.x, minVal, maxVal),
            clamp(u.y, minVal, maxVal),
            clamp(u.z, minVal, maxVal),
            clamp(u.w, minVal, maxVal)
        )
    }
    def clamp(u: AnyVec4i, minVal: AnyVec4i, maxVal: AnyVec4i) :Vec4i = {
        Vec4i(
            clamp(u.x, minVal.x, maxVal.x),
            clamp(u.y, minVal.y, maxVal.y),
            clamp(u.z, minVal.z, maxVal.z),
            clamp(u.w, minVal.w, maxVal.w)
        )
    }
}
