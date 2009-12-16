/*
 * Simplex3D, Math tests
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
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
class Vec2iTest extends FunSuite {

    test("Mutable factories") {
        var u = Vec2i(5)
        expect(classOf[Vec2i]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }

        u = Vec2i(2, 3)
        expect(classOf[Vec2i]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = Vec2i(Vec2i(4, 5))
        expect(classOf[Vec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2i(Vec3i(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4i(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = Vec2i(Vec2f(4, 5))
        expect(classOf[Vec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2i(Vec3f(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4f(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = Vec2i(Vec2d(4, 5))
        expect(classOf[Vec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = Vec2i(Vec3d(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4d(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Const conversions") {
        val x = 1
        val y = 2

        val a = ConstVec2i(x, y)
        expect(x) { a.x }
        expect(y) { a.y }

        val b = ConstVec2i(Vec2i(x, y))
        expect(x) { b.x }
        expect(y) { b.y }

        val t: ConstVec2i = Vec2i(x, y)
        assert(Vec2i(x, y) == t)

        var c: ConstVec2i = Vec2i(x, y); var v = Vec2i(3)
        v = c; assert(Vec2i(x, y) == v)

        c = Vec2i(5); v = Vec2i(x, y)
        c = v; assert(Vec2i(x, y) == c)
    }

    test("Equality methods") {
        assert(Vec2i(4, 7) == ConstVec2i(4, 7))
        assert(ConstVec2i(4, 7) == Vec2i(4, 7))

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
        pending
    }

    test("Mutable math") {
        pending
    }
}
