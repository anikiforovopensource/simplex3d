/*
 * Simplex3D, MathTest package
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

package simplex3d.math

import org.scalatest._


class Vec2Test extends FunSuite {

    test ("Example") {
        expect (1) { 3 - 2 }

        info("example info")

        intercept[IllegalArgumentException] {
            throw new IllegalArgumentException()
        }
    }

    test("Const factories") {
        var u = ConstVec2(5)
        expect(classOf[ConstVec2]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        
        u = ConstVec2(2, 3)
        expect(classOf[ConstVec2]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }

        u = ConstVec2(Vec2(4, 5))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2(Vec3(6, 7, 8))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2(Vec4(1, 2, 9, 0))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }

        u = ConstVec2(Vec2i(4, 5))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }

        u = ConstVec2(Vec3i(6, 7, 8))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }

        u = ConstVec2(Vec4i(1, 2, 9, 0))
        expect(classOf[ConstVec2]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
    }

    test("Const swizzle") {
        pending
    }

    test("Equality methods") {
        pending
    }

    test("isValid") {
        pending
    }

    test("Shared math") {
        pending
    }

    test("Mutable factories") {
        pending
    }

    test("Mutable swizzle") {
        pending
    }

    test("Mutable math") {
        pending
    }
}
