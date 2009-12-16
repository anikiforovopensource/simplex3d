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

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec2bTest extends FunSuite {

    val combinations = {
        (false, false, false, false) ::
        (false, false, false, true ) ::
        (false, false, true,  false) ::
        (false, false, true,  true ) ::
        (false, true,  false, false) ::
        (false, true,  false, true ) ::
        (false, true,  true,  false) ::
        (false, true,  true,  true ) ::
        (true,  false, false, false) ::
        (true,  false, false, true ) ::
        (true,  false, true,  false) ::
        (true,  false, true,  true ) ::
        (true,  true,  false, false) ::
        (true,  true,  false, true ) ::
        (true,  true,  true,  false) ::
        (true,  true,  true,  true ) ::
        Nil
    }

    test("Mutable factories") {
        var u = Vec2b(true)
        expect(true) { u.x }
        expect(true) { u.y }

        u = Vec2b(false)
        expect(false) { u.x }
        expect(false) { u.y }

        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            var u = Vec2b(x, y)
            expect(x) { u.x }
            expect(y) { u.y }

            u = Vec2b(Vec2b(x, y))
            expect(x) { u.x }
            expect(y) { u.y }

            u = Vec2b(Vec3b(x, y, z))
            expect(x) { u.x }
            expect(y) { u.y }

            u = Vec2b(Vec4b(x, y, z, w))
            expect(x) { u.x }
            expect(y) { u.y }
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Const conversions") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            val a = ConstVec2b(x, y)
            expect(x) { a.x }
            expect(y) { a.y }

            val b = ConstVec2b(Vec2b(x, y))
            expect(x) { b.x }
            expect(y) { b.y }

            val t: ConstVec2b = Vec2b(x, y)
            assert(Vec2b(x, y) == t)

            var c: ConstVec2b = Vec2b(x, y); var v = Vec2b(false)
            v = c; assert(Vec2b(x, y) == v)

            c = Vec2b(true); v = Vec2b(x, y)
            c = v; assert(Vec2b(x, y) == c)
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Equality methods") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            assert(Vec2b(x, y) == ConstVec2b(x, y))
            assert(ConstVec2b(x, y) == Vec2b(x, y))

            assert(Vec2b(x, y) != Vec2b(!x, y))
            assert(Vec2b(x, y) != Vec2b(x, !y))
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Indexed read") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            val u = ConstVec2b(x, y)

            expect(x) { u(0) }
            expect(y) { u(1) }

            intercept[IndexOutOfBoundsException] {
                u(2)
            }
            intercept[IndexOutOfBoundsException] {
                u(-1)
            }
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Indexed write") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            val u = Vec2b(x, y)

            u(0) = !x
            assert(Vec2b(!x, y) == u)

            u(1) = !y
            assert(Vec2b(!x, !y) == u)

            intercept[IndexOutOfBoundsException] {
                u(2) = true
            }
            intercept[IndexOutOfBoundsException] {
                u(-1) = true
            }
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Setters") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            val u = Vec2b(!x)

            u := Vec2b(x, y)
            expect(x) { u.x }
            expect(y) { u.y }

            u.set(!x, !y)
            expect(!x) { u.x }
            expect(!y) { u.y }
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }

    test("Getters") {
        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
            val u = Vec2b(x, y)
            expect(x) { u.r }
            expect(y) { u.g }
        }

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }
}
