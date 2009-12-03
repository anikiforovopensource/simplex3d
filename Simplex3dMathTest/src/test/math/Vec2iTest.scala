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

package test.math

import org.scalatest._

import simplex3d.math.intvec._
import simplex3d.math.floatvec._
import simplex3d.math.intvec.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2iTest extends FunSuite {

    test("Const factories") {
        var u = ConstVec2i(5)
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        
        u = ConstVec2i(2, 3)
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = ConstVec2i(Vec2(4, 5))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2i(Vec3(6, 7, 8))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2i(Vec4(1, 2, 3, 4))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = ConstVec2i(Vec2i(4, 5))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2i(Vec3i(6, 7, 8))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2i(Vec4i(1, 2, 3, 4))
        expect(classOf[ConstVec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

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

        u = Vec2i(Vec3(6, 7, 8))
        expect(classOf[Vec2i]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = Vec2i(Vec4(1, 2, 3, 4))
        expect(classOf[Vec2i]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

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

    test("Const math") {
        pending
    }

    test("Mutable math") {
        pending
    }
}
