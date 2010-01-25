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
import test.math.BooleanCombinations

import simplex3d.math.BaseMath._
import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2iTest extends FunSuite {

    test("Factories") {
        val x = 3
        val y = 4
        val z = 5
        val w = 6

        var u = Vec2i(x)
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(x) { u.y }

        u = Vec2i(x, y)
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec2i(int(x), int(y)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec3i(int(x), int(y), int(z)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec4i(int(x), int(y), int(z), int(w)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec2f(float(x), float(y)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec3f(float(x), float(y), float(z)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec4f(float(x), float(y), float(z), float(w)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec2d(double(x), double(y)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec3d(double(x), double(y), double(z)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        u = Vec2i(Vec4d(double(x), double(y), double(z), double(w)))
        expect(classOf[Vec2i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }

        var c = ConstVec2i(x, y)
        expect(classOf[ConstVec2i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }

        c = ConstVec2i(Vec2i(int(x), int(y)))
        expect(classOf[ConstVec2i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }

        c = ConstVec2i(Vec2f(float(x), float(y)))
        expect(classOf[ConstVec2i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }

        c = ConstVec2i(Vec2d(double(x), double(y)))
        expect(classOf[ConstVec2i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }
    }

    test("Boolean factories") {
        BooleanCombinations.test { (x, y, z, w) =>
            var u = Vec2i(Vec2b(x, y))
            expect(classOf[Vec2i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }

            u = Vec2i(Vec3b(x, y, z))
            expect(classOf[Vec2i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }

            u = Vec2i(Vec4b(x, y, z, w))
            expect(classOf[Vec2i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }

            var c = ConstVec2i(Vec2b(x, y))
            expect(classOf[ConstVec2i]) { c.getClass }
            expect(int(x)) { c.x }
            expect(int(y)) { c.y }
        }
    }

    test("Const conversions") {
        val x = 1
        val y = 2

        val t: ConstVec2i = Vec2i(x, y)
        expect(classOf[ConstVec2i]) { t.getClass }
        assert(Vec2i(x, y) == t)

        var c: ConstVec2i = Vec2i(x, y); var v = Vec2i(3)
        expect(classOf[ConstVec2i]) { c.getClass }
        v = c; assert(Vec2i(x, y) == v)
        expect(classOf[Vec2i]) { v.getClass }

        c = Vec2i(5); v = Vec2i(x, y)
        expect(classOf[Vec2i]) { v.getClass }
        c = v; assert(Vec2i(x, y) == c)
        expect(classOf[ConstVec2i]) { c.getClass }
    }

    test("Equality methods") {
        val m = Vec2i(4, 7)
        val c = ConstVec2i(4, 7)

        assert(m == m)
        assert(m == c)
        assert(c == m)
        assert(c == c)

        assert(m.equals(c))
        assert(!m.equals(Nil))

        assert(Vec2i(1, 2) != Vec2i(9, 2))
        assert(Vec2i(1, 2) != Vec2i(1, 9))
    }

    test("Indexed read") {
        val u = ConstVec2i(3, 4)

        expect(3) { u(0) }
        expect(4) { u(1) }

        intercept[IndexOutOfBoundsException] {
            u(2)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Vec2i(3, 4)

        u(0) = 5
        assert(Vec2i(5, 4) == u)

        u(1) = 6
        assert(Vec2i(5, 6) == u)

        intercept[IndexOutOfBoundsException] {
            u(2) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Setters") {
        val u = Vec2i(0)

        u := Vec2i(1, 2)
        expect(1) { u.x }
        expect(2) { u.y }

        u.set(5, 6)
        expect(5) { u.x }
        expect(6) { u.y }
    }

    test("Const math") {
        val u = ConstVec2i(10, 20)
        
        assert(Vec2i(-10, -20) == -u)
        assert(Vec2i(~10, ~20) == ~u)
        
        assert(Vec2i(20, 40) == u*2)
        assert(Vec2i(20, 40) == 2*u)
        assert(Vec2i(5, 10) == u / 2)
        assert(Vec2i(2, 1) == 20 / u)
        assert(Vec2i(1, 2) == u % 3)
        assert(Vec2i(2, 12) == 32 % u)

        val v = ConstVec2i(2, 3)

        assert(Vec2i(12, 23) == u + v)
        assert(Vec2i(8, 17) == u - v)
        assert(Vec2i(20, 60) == u * v)
        assert(Vec2i(5, 6) == u / v)
        assert(Vec2i(0, 2) == u % v)

        val b = ConstVec2i(0xF, 0xFF)

        assert(Vec2i(0, 0xF) == b >> 4)
        assert(Vec2i(-0xF >>> 4, -0xFF >>> 4) == -b >>> 4)
        assert(Vec2i(0xF0, 0xFF0) == b << 4)

        assert(Vec2i(0xF, 0xF) == (b & 0xF))
        assert(Vec2i(0xF, 0xF) == (0xF & b))
        assert(Vec2i(0xFF, 0xFF) == (b | 0xFF))
        assert(Vec2i(0xFF, 0xFF) == (0xFF | b))
        assert(Vec2i(0xF0, 0) == (b ^ 0xFF))
        assert(Vec2i(0xF0, 0) == (0xFF ^ b))

        assert(Vec2i(0x3, 0xF) == (b >> Vec2i(2, 4)))
        assert(Vec2i(-0xF >>> 2, -0xFF >>> 4) == (-b >>> Vec2i(2, 4)))
        assert(Vec2i(0xF0, 0xFF00) == (b << Vec2i(4, 8)))

        assert(Vec2i(0xF, 0xF) == (b & Vec2i(0xFF, 0xF)))
        assert(Vec2i(0xFF, 0xFF) == (b | Vec2i(0xFF, 0xF)))
        assert(Vec2i(0xF0, 0xF0) == (b ^ Vec2i(0xFF, 0xF)))
    }

    test("Mutable math") {
        val i = ConstVec2i(10, 20)
        val u = Vec2i(0)

        u := i; u *= 2; assert(Vec2i(20, 40) == u)
        u := i; u /= 2; assert(Vec2i(5, 10) == u)
        u := i; u %= 3; assert(Vec2i(1, 2) == u)

        val v = ConstVec2i(2, 3)

        u := i; u += v; assert(Vec2i(12, 23) == u)
        u := i; u -= v; assert(Vec2i(8, 17) == u)
        u := i; u *= v; assert(Vec2i(20, 60) == u)
        u := i; u /= v; assert(Vec2i(5, 6) == u)
        u := i; u %= v; assert(Vec2i(0, 2) == u)

        val b = ConstVec2i(0xF, 0xFF)

        u := b; u >>= 4; assert(Vec2i(0, 0xF) == u)
        u := -b; u >>>= 4; assert(Vec2i(-0xF >>> 4, -0xFF >>> 4) == u)
        u := b; u <<= 4; assert(Vec2i(0xF0, 0xFF0) == u)
        
        u := b; u &= 0xF; assert(Vec2i(0xF, 0xF) == u)
        u := b; u |= 0xFF; assert(Vec2i(0xFF, 0xFF) == u)
        u := b; u ^= 0xFF; assert(Vec2i(0xF0, 0) == u)

        u := b; u >>= Vec2i(2, 4); assert(Vec2i(0x3, 0xF) == u)
        u := -b; u >>>= Vec2i(2, 4); assert(Vec2i(-0xF >>> 2, -0xFF >>> 4) == u)
        u := b; u <<= Vec2i(4, 8); assert(Vec2i(0xF0, 0xFF00) == u)

        u := b; u &= Vec2i(0xFF, 0xF); assert(Vec2i(0xF, 0xF) == u)
        u := b; u |= Vec2i(0xFF, 0xF); assert(Vec2i(0xFF, 0xFF) == u)
        u := b; u ^= Vec2i(0xFF, 0xF); assert(Vec2i(0xF0, 0xF0) == u)
    }
}
