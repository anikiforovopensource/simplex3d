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
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math


/**
 * @author Aleksey Nikiforov (lex)
 */
class Random private (seed: Long) extends scala.util.Random(seed) {

    def nextVec2() :Vec2 = Vec2(nextFloat(), nextFloat())
    def nextVec3() :Vec3 = Vec3(nextFloat(), nextFloat(), nextFloat())
    def nextVec4() :Vec4 = {
        Vec4(nextFloat(), nextFloat(), nextFloat(), nextFloat())
    }

    def nextVec2i() :Vec2i = Vec2i(nextInt(), nextInt())
    def nextVec3i() :Vec3i = Vec3i(nextInt(), nextInt(), nextInt())
    def nextVec4i() :Vec4i = {
        Vec4i(nextInt(), nextInt(), nextInt(), nextInt())
    }

    def nextVec2i(n: Int) :Vec2i = Vec2i(nextInt(n), nextInt(n))
    def nextVec3i(n: Int) :Vec3i = Vec3i(nextInt(n), nextInt(n), nextInt(n))
    def nextVec4i(n: Int) :Vec4i = {
        Vec4i(nextInt(n), nextInt(n), nextInt(n), nextInt(n))
    }

    def nextVec2b() :Vec2b = Vec2b(nextBoolean(), nextBoolean())
    def nextVec3b() :Vec3b = Vec3b(nextBoolean(), nextBoolean(), nextBoolean())
    def nextVec4b() :Vec4b = {
        Vec4b(nextBoolean(), nextBoolean(), nextBoolean(), nextBoolean())
    }
}

object Random {
    def apply(seed: Long) = new Random(seed)
    def apply() = new Random(System.nanoTime)
    def apply(seed: Int)  = new Random(seed: Long)
}
