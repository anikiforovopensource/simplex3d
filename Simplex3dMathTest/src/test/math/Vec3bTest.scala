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

package test.math

import org.scalatest._

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec3bTest extends FunSuite {

    test("Factories") {
        var u = Vec3b(true)
        expect(classOf[Vec3b]) { u.getClass }
        expect(true) { u.x }
        expect(true) { u.y }
        expect(true) { u.z }

        u = Vec3b(false)
        expect(classOf[Vec3b]) { u.getClass }
        expect(false) { u.x }
        expect(false) { u.y }
        expect(false) { u.z }

        BooleanCombinations.test { (x, y, z, w) =>
            var u = Vec3b(x, y, z)
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec3b(x, y, z))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(x, Vec2b(y, z))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec2b(x, y), z)
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec4b(x, y, z, w))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec3i(int(x), int(y), int(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(x, Vec2i(int(y), int(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec2i(int(x), int(y)), z)
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec4i(int(x), int(y), int(z), int(w)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec3f(float(x), float(y), float(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(x, Vec2f(float(y), float(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec2f(float(x), float(y)), z)
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec4f(float(x), float(y), float(z), float(w)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec3d(double(x), double(y), double(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(x, Vec2d(double(y), double(z)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec2d(double(x), double(y)), z)
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u = Vec3b(Vec4d(double(x), double(y), double(z), double(w)))
            expect(classOf[Vec3b]) { u.getClass }
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            var c = ConstVec3b(x, y, z)
            expect(classOf[ConstVec3b]) { c.getClass }
            expect(x) { c.x }
            expect(y) { c.y }
            expect(z) { c.z }

            c = ConstVec3b(Vec3i(int(x), int(y), int(z)))
            expect(classOf[ConstVec3b]) { c.getClass }
            expect(x) { c.x }
            expect(y) { c.y }
            expect(z) { c.z }

            c = ConstVec3b(Vec3f(float(x), float(y), float(z)))
            expect(classOf[ConstVec3b]) { c.getClass }
            expect(x) { c.x }
            expect(y) { c.y }
            expect(z) { c.z }

            c = ConstVec3b(Vec3d(double(x), double(y), double(z)))
            expect(classOf[ConstVec3b]) { c.getClass }
            expect(x) { c.x }
            expect(y) { c.y }
            expect(z) { c.z }
        }
    }

    test("Const conversions") {
        BooleanCombinations.test { (x, y, z, w) =>
            val t: ConstVec3b = Vec3b(x, y, z)
            expect(classOf[ConstVec3b]) { t.getClass }
            assert(Vec3b(x, y, z) == t)

            var c: ConstVec3b = Vec3b(x, y, z); var v = Vec3b(false)
            expect(classOf[ConstVec3b]) { c.getClass }
            v = c; assert(Vec3b(x, y, z) == v)
            expect(classOf[Vec3b]) { v.getClass }

            c = Vec3b(true); v = Vec3b(x, y, z)
            expect(classOf[Vec3b]) { v.getClass }
            c = v; assert(Vec3b(x, y, z) == c)
            expect(classOf[ConstVec3b]) { c.getClass }
        }
    }

    test("Equality methods") {
        BooleanCombinations.test { (x, y, z, w) =>
            val m = Vec3b(x, y, z)
            val c = ConstVec3b(x, y, z)

            assert(m == m)
            assert(m == c)
            assert(c == m)
            assert(c == c)

            assert(m.equals(c))
            assert(!m.equals(Nil))

            assert(Vec3b(x, y, z) != Vec3b(!x, y, z))
            assert(Vec3b(x, y, z) != Vec3b(x, !y, z))
            assert(Vec3b(x, y, z) != Vec3b(x, y, !z))
        }
    }

    test("Indexed read") {
        BooleanCombinations.test { (x, y, z, w) =>
            val u = ConstVec3b(x, y, z)

            expect(x) { u(0) }
            expect(y) { u(1) }
            expect(z) { u(2) }

            intercept[IndexOutOfBoundsException] {
                u(3)
            }
            intercept[IndexOutOfBoundsException] {
                u(-1)
            }
        }
    }

    test("Indexed write") {
        BooleanCombinations.test { (x, y, z, w) =>
            val u = Vec3b(x, y, z)

            u(0) = !x
            assert(Vec3b(!x, y, z) == u)

            u(1) = !y
            assert(Vec3b(!x, !y, z) == u)

            u(2) = !z
            assert(Vec3b(!x, !y, !z) == u)

            intercept[IndexOutOfBoundsException] {
                u(3) = true
            }
            intercept[IndexOutOfBoundsException] {
                u(-1) = true
            }
        }
    }

    test("Setters") {
        BooleanCombinations.test { (x, y, z, w) =>
            val u = Vec3b(!x)

            u := Vec3b(x, y, z)
            expect(x) { u.x }
            expect(y) { u.y }
            expect(z) { u.z }

            u.set(!x, !y, !z)
            expect(!x) { u.x }
            expect(!y) { u.y }
            expect(!z) { u.z }
        }
    }

    test("Getters") {
        BooleanCombinations.test { (x, y, z, w) =>
            val u = Vec3b(x, y, z)
            expect(x) { u.r }
            expect(y) { u.g }
            expect(z) { u.b }
        }
    }
}
