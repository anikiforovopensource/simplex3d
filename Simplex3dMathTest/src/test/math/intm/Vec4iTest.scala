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
class Vec4iTest extends FunSuite {

    test("Factories") {
        val x = 3
        val y = 4
        val z = 5
        val w = 6

        var u: AnyVec4i = Vec4i(x)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(x) { u.y }
        expect(x) { u.z }
        expect(x) { u.w }

        u = Vec4i(Mat2f(x, y, z, w))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Mat2d(x, y, z, w))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, y, z, w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec4i(int(x), int(y), int(z), int(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, y, Vec2i(int(z), int(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec2i(int(y), int(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec2i(int(x), int(y)), z, w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec3i(int(y), int(z), int(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec3i(int(x), int(y), int(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec4f(float(x), float(y), float(z), float(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, y, Vec2f(float(z), float(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec2f(float(y), float(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec2f(float(x), float(y)), z, w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec3f(float(y), float(z), float(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec3f(float(x), float(y), float(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec4d(double(x), double(y), double(z), double(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, y, Vec2d(double(z), double(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec2d(double(y), double(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec2d(double(x), double(y)), z, w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(x, Vec3d(double(y), double(z), double(w)))
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        u = Vec4i(Vec3d(double(x), double(y), double(z)), w)
        expect(classOf[Vec4i]) { u.getClass }
        expect(x) { u.x }
        expect(y) { u.y }
        expect(z) { u.z }
        expect(w) { u.w }

        var c: AnyVec4i = ConstVec4i(x, y, z, w)
        expect(classOf[ConstVec4i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }
        expect(z) { c.z }
        expect(w) { c.w }

        c = ConstVec4i(Vec4i(int(x), int(y), int(z), int(w)))
        expect(classOf[ConstVec4i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }
        expect(z) { c.z }
        expect(w) { c.w }

        c = ConstVec4i(Vec4f(float(x), float(y), float(z), float(w)))
        expect(classOf[ConstVec4i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }
        expect(z) { c.z }
        expect(w) { c.w }

        c = ConstVec4i(Vec4d(double(x), double(y), double(z), double(w)))
        expect(classOf[ConstVec4i]) { c.getClass }
        expect(x) { c.x }
        expect(y) { c.y }
        expect(z) { c.z }
        expect(w) { c.w }
    }

    test("Boolean factories") {
        BooleanCombinations.test { (x, y, z, w) =>
            var u: AnyVec4i = Vec4i(Vec4b(x, y, z, w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            u = Vec4i(int(x), int(y), Vec2b(z, w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            u = Vec4i(int(x), Vec2b(y, z), int(w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            u = Vec4i(Vec2b(x, y), int(z), int(w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            u = Vec4i(int(x), Vec3b(y, z, w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            u = Vec4i(Vec3b(x, y, z), int(w))
            expect(classOf[Vec4i]) { u.getClass }
            expect(int(x)) { u.x }
            expect(int(y)) { u.y }
            expect(int(z)) { u.z }
            expect(int(w)) { u.w }

            var c: AnyVec4i = ConstVec4i(Vec4b(x, y, z, w))
            expect(classOf[ConstVec4i]) { c.getClass }
            expect(int(x)) { c.x }
            expect(int(y)) { c.y }
            expect(int(z)) { c.z }
            expect(int(w)) { c.w }
        }
    }

    test("Unapply") {
        val x = 1; val y = 2; val z = 3; val w = 4
        Vec4i(x, y, z, w) match {
            case Vec4i(ux, uy, uz, uw) =>
                if (ux != x || uy != y || uz != z || uw != w)
                    throw new AssertionError()
        }
        ConstVec4i(x, y, z, w) match {
            case Vec4i(ux, uy, uz, uw) =>
                if (ux != x || uy != y || uz != z || uw != w)
                    throw new AssertionError()
        }
    }

    test("Const conversions") {
        val x = 1
        val y = 2
        val z = 3
        val w = 4

        val t: ConstVec4i = Vec4i(x, y, z, w)
        expect(classOf[ConstVec4i]) { t.getClass }
        assert(Vec4i(x, y, z, w) == t)

        var c: ConstVec4i = Vec4i(x, y, z, w); var v = Vec4i(3)
        expect(classOf[ConstVec4i]) { c.getClass }
        v = c; assert(Vec4i(x, y, z, w) == v)
        expect(classOf[Vec4i]) { v.getClass }

        c = Vec4i(5); v = Vec4i(x, y, z, w)
        expect(classOf[Vec4i]) { v.getClass }
        c = v; assert(Vec4i(x, y, z, w) == c)
        expect(classOf[ConstVec4i]) { c.getClass }
    }

    test("Equality methods") {
        val m = Vec4i(4, 7, 9, 1)
        val c = ConstVec4i(4, 7, 9, 1)

        assert(m == m)
        assert(m == c)
        assert(c == m)
        assert(c == c)

        assert(m.equals(c))
        assert(!m.equals(Nil))

        assert(Vec4i(1, 2, 3, 4) != Vec4i(9, 2, 3, 4))
        assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 9, 3, 4))
        assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 2, 9, 4))
        assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 2, 3, 9))
    }

    test("Indexed read") {
        val u = ConstVec4i(3, 4, 5, 6)

        expect(3) { u(0) }
        expect(4) { u(1) }
        expect(5) { u(2) }
        expect(6) { u(3) }

        intercept[IndexOutOfBoundsException] {
            u(4)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Vec4i(3, 4, 5, 6)

        u(0) = 5
        assert(Vec4i(5, 4, 5, 6) == u)

        u(1) = 6
        assert(Vec4i(5, 6, 5, 6) == u)

        u(2) = 7
        assert(Vec4i(5, 6, 7, 6) == u)

        u(3) = 8
        assert(Vec4i(5, 6, 7, 8) == u)

        intercept[IndexOutOfBoundsException] {
            u(4) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Setters") {
        val u = Vec4i(0)

        u := Vec4i(1, 2, 3, 4)
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }
        expect(4) { u.w }

        u.set(5, 6, 7, 8)
        expect(5) { u.x }
        expect(6) { u.y }
        expect(7) { u.z }
        expect(8) { u.w }
    }

    test("Const math") {
        val u = ConstVec4i(10, 20, 30, 40)

        assert(+u eq u)
        
        assert(Vec4i(-10, -20, -30, -40) == -u)
        assert(Vec4i(~10, ~20, ~30, ~40) == ~u)
        
        assert(Vec4i(20, 40, 60, 80) == u*2)
        assert(Vec4i(5, 10, 15, 20) == u / 2)

        assert(Vec4i(12, 22, 32, 42) == u + 2)
        assert(Vec4i(8, 18, 28, 38) == u - 2)

        assert(Vec4i(1, 2, 0, 1) == u % 3)

        val v = ConstVec4i(2, 3, 4, 5)

        assert(Vec4i(12, 23, 34, 45) == u + v)
        assert(Vec4i(8, 17, 26, 35) == u - v)
        assert(Vec4i(20, 60, 120, 200) == u * v)
        assert(Vec4i(5, 6, 7, 8) == u / v)
        assert(Vec4i(0, 2, 2, 0) == u % v)

        val b = ConstVec4i(0xF, 0xFF, 0xFFF, 0xFFFF)

        assert(Vec4i(0, 0xF, 0xFF, 0xFFF) == b >> 4)
        assert(Vec4i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4, -0xFFFF >>> 4) == -b >>> 4)
        assert(Vec4i(0xF0, 0xFF0, 0xFFF0, 0xFFFF0) == b << 4)

        assert(Vec4i(0xF, 0xF, 0xF, 0xF) == (b & 0xF))
        assert(Vec4i(0xFF, 0xFF, 0xFFF, 0xFFFF) == (b | 0xFF))
        assert(Vec4i(0xF0, 0, 0xF00, 0xFF00) == (b ^ 0xFF))

        assert(Vec4i(0x3, 0xF, 0x3F, 0xFF) == (b >> Vec4i(2, 4, 6, 8)))
        assert(Vec4i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6, -0xFFFF >>> 8) == (-b >>> Vec4i(2, 4, 6, 8)))
        assert(Vec4i(0xF0, 0xFF00, 0xFFF000, 0xFFFF0000) == (b << Vec4i(4, 8, 12, 16)))

        assert(Vec4i(0xF, 0xFF, 0xFF, 0xF) == (b & Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
        assert(Vec4i(0xFFFF, 0xFFF, 0xFFF, 0xFFFF) == (b | Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
        assert(Vec4i(0xFFF0, 0xF00, 0xF00, 0xFFF0) == (b ^ Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
    }

    test("Mutable math") {
        val i = ConstVec4i(10, 20, 30, 40)
        val u = Vec4i(0)

        u := i; u *= 2; assert(Vec4i(20, 40, 60, 80) == u)
        u := i; u /= 2; assert(Vec4i(5, 10, 15, 20) == u)

        u := i; u += 2; assert(Vec4i(12, 22, 32, 42) == u)
        u := i; u -= 2; assert(Vec4i(8, 18, 28, 38) == u)

        u := i; u %= 3; assert(Vec4i(1, 2, 0, 1) == u)

        val v = ConstVec4i(2, 3, 4, 5)

        u := i; u += v; assert(Vec4i(12, 23, 34, 45) == u)
        u := i; u -= v; assert(Vec4i(8, 17, 26, 35) == u)
        u := i; u *= v; assert(Vec4i(20, 60, 120, 200) == u)
        u := i; u /= v; assert(Vec4i(5, 6, 7, 8) == u)
        u := i; u %= v; assert(Vec4i(0, 2, 2, 0) == u)

        val b = ConstVec4i(0xF, 0xFF, 0xFFF, 0xFFFF)

        u := b; u >>= 4; assert(Vec4i(0, 0xF, 0xFF, 0xFFF) == u)
        u := -b; u >>>= 4; assert(Vec4i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4, -0xFFFF >>> 4) == u)
        u := b; u <<= 4; assert(Vec4i(0xF0, 0xFF0, 0xFFF0, 0xFFFF0) == u)

        u := b; u &= 0xF; assert(Vec4i(0xF, 0xF, 0xF, 0xF) == u)
        u := b; u |= 0xFF; assert(Vec4i(0xFF, 0xFF, 0xFFF, 0xFFFF) == u)
        u := b; u ^= 0xFF; assert(Vec4i(0xF0, 0, 0xF00, 0xFF00) == u)

        u := b; u >>= Vec4i(2, 4, 6, 8); assert(Vec4i(0x3, 0xF, 0x3F, 0xFF) == u)
        u := -b; u >>>= Vec4i(2, 4, 6, 8); assert(Vec4i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6, -0xFFFF >>> 8) == u)
        u := b; u <<= Vec4i(4, 8, 12, 16); assert(Vec4i(0xF0, 0xFF00, 0xFFF000, 0xFFFF0000) == u)

        u := b; u &= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xF, 0xFF, 0xFF, 0xF) == u)
        u := b; u |= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xFFFF, 0xFFF, 0xFFF, 0xFFFF) == u)
        u := b; u ^= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xFFF0, 0xF00, 0xF00, 0xFFF0) == u)
    }

    test("Collection") {
        def test(u: AnyVec4i) = {
            assert(u.head == u.x)
            assert(u.last == u.w)
            assert(u.size == 4)

            val iterator = u.iterator
            assert(iterator.hasNext)
            assert(iterator.next == u.x)
            assert(iterator.hasNext)
            assert(iterator.next == u.y)
            assert(iterator.hasNext)
            assert(iterator.next == u.z)
            assert(iterator.hasNext)
            assert(iterator.next == u.w)
            assert(!iterator.hasNext)
            intercept[NoSuchElementException] {
                iterator.next
            }

            var i = 0
            u.foreach { element =>
                assert(element == u(i))
                i += 1
            }
        }

        val x = 1
        val y = 2
        val z = 3
        val w = 4

        test(Vec4i(x, y, z, w))
        test(ConstVec4i(x, y, z, w))
    }
}
