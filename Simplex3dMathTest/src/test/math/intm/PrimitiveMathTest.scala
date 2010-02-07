/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.math.intm

import org.scalatest._

import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class PrimitiveMathTest extends FunSuite {

    test("intm Primitive Math") {
        {
            val u = ConstVec2i(10, 20)
            val b = ConstVec2i(0xF, 0xFF)

            assert(Vec2i(20, 40) == 2*u)
            assert(Vec2i(2, 1) == 20 / u)
            assert(Vec2i(2, 12) == 32 % u)

            assert(Vec2i(0xF, 0xF) == (0xF & b))
            assert(Vec2i(0xFF, 0xFF) == (0xFF | b))
            assert(Vec2i(0xF0, 0) == (0xFF ^ b))
        }

        {
            val u = ConstVec3i(10, 20, 30)
            val b = ConstVec3i(0xF, 0xFF, 0xFFF)

            assert(Vec3i(20, 40, 60) == 2*u)
            assert(Vec3i(3, 1, 1) == 30 / u)
            assert(Vec3i(2, 12, 2) == 32 % u)

            assert(Vec3i(0xF, 0xF, 0xF) == (0xF & b))
            assert(Vec3i(0xFF, 0xFF, 0xFFF) == (0xFF | b))
            assert(Vec3i(0xF0, 0, 0xF00) == (0xFF ^ b))
        }

        {
            val u = ConstVec4i(10, 20, 30, 40)
            val b = ConstVec4i(0xF, 0xFF, 0xFFF, 0xFFFF)

            assert(Vec4i(20, 40, 60, 80) == 2*u)
            assert(Vec4i(4, 2, 1, 1) == 40 / u)
            assert(Vec4i(3, 3, 13, 3) == 43 % u)

            assert(Vec4i(0xF, 0xF, 0xF, 0xF) == (0xF & b))
            assert(Vec4i(0xFF, 0xFF, 0xFFF, 0xFFFF) == (0xFF | b))
            assert(Vec4i(0xF0, 0, 0xF00, 0xFF00) == (0xFF ^ b))
        }
    }
}
