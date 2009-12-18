/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009 Simplex3d Team
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
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec3iTest extends FunSuite {

    test("Mutable factories") {
        pending
    }

    test("Const conversions") {
        pending
    }

    test("Equality methods") {
        pending
    }

    test("Indexed read") {
        pending
    }

    test("Indexed write") {
        pending
    }

    test("Setters") {
        pending
    }

    test("Const math") {
        val u = ConstVec3i(10, 20, 30)
        
        assert(Vec3i(-10, -20, -30) == -u)
        assert(Vec3i(~10, ~20, ~30) == ~u)
        
        assert(Vec3i(20, 40, 60) == u*2)
        assert(Vec3i(20, 40, 60) == 2*u)
        assert(Vec3i(5, 10, 15) == u / 2)
        assert(Vec3i(3, 1, 1) == 30 / u)
        assert(Vec3i(1, 2, 0) == u % 3)
        assert(Vec3i(2, 12, 2) == 32 % u)

        val v = ConstVec3i(2, 3, 4)

        assert(Vec3i(12, 23, 34) == u + v)
        assert(Vec3i(8, 17, 26) == u - v)
        assert(Vec3i(20, 60, 120) == u * v)
        assert(Vec3i(5, 6, 7) == u / v)
        assert(Vec3i(0, 2, 2) == u % v)

        val b = ConstVec3i(0xF, 0xFF, 0xFFF)

        assert(Vec3i(0, 0xF, 0xFF) == b >> 4)
        assert(Vec3i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4) == -b >>> 4)
        assert(Vec3i(0xF0, 0xFF0, 0xFFF0) == b << 4)

        assert(Vec3i(0xF, 0xF, 0xF) == (b & 0xF))
        assert(Vec3i(0xF, 0xF, 0xF) == (0xF & b))
        assert(Vec3i(0xFF, 0xFF, 0xFFF) == (b | 0xFF))
        assert(Vec3i(0xFF, 0xFF, 0xFFF) == (0xFF | b))
        assert(Vec3i(0xF0, 0, 0xF00) == (b ^ 0xFF))
        assert(Vec3i(0xF0, 0, 0xF00) == (0xFF ^ b))

        assert(Vec3i(0x3, 0xF, 0x3F) == (b >> Vec3i(2, 4, 6)))
        assert(Vec3i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6) == (-b >>> Vec3i(2, 4, 6)))
        assert(Vec3i(0xF0, 0xFF00, 0xFFF000) == (b << Vec3i(4, 8, 12)))

        assert(Vec3i(0xF, 0xFF, 0xF) == (b & Vec3i(0xFFF, 0xFF, 0xF)))
        assert(Vec3i(0xFFF, 0xFF, 0xFFF) == (b | Vec3i(0xFFF, 0xFF, 0xF)))
        assert(Vec3i(0xFF0, 0x00, 0xFF0) == (b ^ Vec3i(0xFFF, 0xFF, 0xF)))
    }

    test("Mutable math") {
        val i = ConstVec3i(10, 20, 30)
        val u = Vec3i(0)

        u := i; u *= 2; assert(Vec3i(20, 40, 60) == u)
        u := i; u /= 2; assert(Vec3i(5, 10, 15) == u)
        u := i; u %= 3; assert(Vec3i(1, 2, 0) == u)

        val v = ConstVec3i(2, 3, 4)

        u := i; u += v; assert(Vec3i(12, 23, 34) == u)
        u := i; u -= v; assert(Vec3i(8, 17, 26) == u)
        u := i; u *= v; assert(Vec3i(20, 60, 120) == u)
        u := i; u /= v; assert(Vec3i(5, 6, 7) == u)
        u := i; u %= v; assert(Vec3i(0, 2, 2) == u)

        val b = ConstVec3i(0xF, 0xFF, 0xFFF)

//        u := b; u >>= 4; assert(Vec3i(0, 0xF) == u)
//        u := -b; u >>>= 4; assert(Vec3i(-0xF >>> 4, -0xFF >>> 4) == u)
//        u := b; u <<= 4; assert(Vec3i(0xF0, 0xFF0) == u)
//        u := b; u &= 0xF; assert(Vec3i(0xF, 0xF) == u)
//        u := b; u |= 0xFF; assert(Vec3i(0xFF, 0xFF) == u)
//        u := b; u ^= 0xFF; assert(Vec3i(0xF0, 0) == u)
//
//        u := b; u >>= Vec3i(2, 4); assert(Vec3i(0x3, 0xF) == u)
//        u := -b; u >>>= Vec3i(2, 4); assert(Vec3i(-0xF >>> 2, -0xFF >>> 4) == u)
//        u := b; u <<= Vec3i(4, 8); assert(Vec3i(0xF0, 0xFF00) == u)
//
//        u := b; u &= Vec3i(0xFF, 0xF); assert(Vec3i(0xF, 0xF) == u)
//        u := b; u |= Vec3i(0xFF, 0xF); assert(Vec3i(0xFF, 0xFF) == u)
//        u := b; u ^= Vec3i(0xFF, 0xF); assert(Vec3i(0xF0, 0xF0) == u)
    }
}
