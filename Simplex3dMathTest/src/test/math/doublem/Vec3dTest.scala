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

package test.math.doublem

import org.scalatest._

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec3dTest extends FunSuite {

    test("Mutable factories") {
        var u = Vec3(5)
        expect(classOf[Vec3]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        expect(5) { u.z }

        u = Vec3(2, 3, 4)
        expect(classOf[Vec3]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }
        expect(4) { u.z }

        u = Vec3(Vec3(4, 5, 6))
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(6, Vec2(7, 8))
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec2(6, 7), 8)
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec4(1, 2, 3, 4))
        expect(classOf[Vec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }

        u = Vec3(Vec2i(4, 5), 6)
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(4, Vec2i(5, 6))
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(Vec3i(6, 7, 8))
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec4i(1, 2, 3, 4))
        expect(classOf[Vec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }

        u = Vec3(Vec2f(4, 5), 6)
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(4, Vec2f(5, 6))
        expect(classOf[Vec3]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }

        u = Vec3(Vec3f(6, 7, 8))
        expect(classOf[Vec3]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }

        u = Vec3(Vec4f(1, 2, 3, 4))
        expect(classOf[Vec3]) { u.getClass }
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }
    }

    test("Const conversions") {
        val x = 1d
        val y = 2d
        val z = 3d

        val a = ConstVec3(x, y, z)
        expect(x) { a.x }
        expect(y) { a.y }
        expect(z) { a.z }

        val b = ConstVec3(Vec3(x, y, z))
        expect(x) { b.x }
        expect(y) { b.y }
        expect(z) { b.z }

        val t: ConstVec3 = Vec3(x, y, z)
        assert(Vec3(x, y, z) == t)

        var c: ConstVec3 = Vec3(x, y, z); var v = Vec3(3)
        v = c; assert(Vec3(x, y, z) == v)

        c = Vec3(5); v = Vec3(x, y, z)
        c = v; assert(Vec3(x, y, z) == c)
    }

    test("Equality methods") {
        assert(Vec3(4, 7, 9) == ConstVec3(4, 7, 9))
        assert(ConstVec3(4, 7, 9) == Vec3(4, 7, 9))

        assert(Vec3(1, 2, 3) != Vec3(9, 2, 3))
        assert(Vec3(1, 2, 3) != Vec3(1, 9, 3))
        assert(Vec3(1, 2, 3) != Vec3(1, 2, 9))
    }

    test("Indexed read") {
        val u = ConstVec3(3, 4, 5)

        expect(3) { u(0) }
        expect(4) { u(1) }
        expect(5) { u(2) }

        intercept[IndexOutOfBoundsException] {
            u(3)
        }
        intercept[IndexOutOfBoundsException] {
            u(-1)
        }
    }

    test("Indexed write") {
        val u = Vec3(3, 4, 5)

        u(0) = 5
        assert(Vec3(5, 4, 5) == u)

        u(1) = 6
        assert(Vec3(5, 6, 5) == u)

        u(2) = 7
        assert(Vec3(5, 6, 7) == u)

        intercept[IndexOutOfBoundsException] {
            u(3) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Setters") {
        val u = Vec3(0)

        u := Vec3(1, 2, 3)
        expect(1) { u.x }
        expect(2) { u.y }
        expect(3) { u.z }

        u.set(5, 6, 7)
        expect(5) { u.x }
        expect(6) { u.y }
        expect(7) { u.z }
    }

    test("Const math") {
        val u = ConstVec3(7, 8, 9)

        assert(Vec3(-7, -8, -9) == -u)

        assert(Vec3(14, 16, 18) == u*2)
        assert(Vec3(14, 16, 18) == 2*u)
        assert(Vec3(14, 16, 18) == 2f*u)
        assert(Vec3(14, 16, 18) == 2d*u)
        assert(Vec3(3.5, 4, 4.5) == u/2)
        assert(Vec3(1, 7/8d, 7/9d) == 7/u)
        assert(Vec3(1, 7/8d, 7/9d) == 7f/u)
        assert(Vec3(1, 7/8d, 7/9d) == 7d/u)

        val v = ConstVec3(2, 4, 3)

        assert(Vec3(9, 12, 12) == u + v)
        assert(Vec3(5, 4, 6) == u - v)
        assert(Vec3(14, 32, 27) == u*v)
        assert(Vec3(3.5, 2, 3) == u/v)

        val t = ConstVec3(2, 3, 4)

        val m3x2 = ConstMat3x2(
            2, 5, 4,
            3, 4, 8
        )
        assert(Vec2(35, 50) == t*m3x2)

        val m3 = ConstMat3(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2
        )
        assert(Vec3(35, 50, 34) == t*m3)

        val m3x4 = ConstMat3x4(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2,
            5, 9, 2
        )
        assert(Vec4(35, 50, 34, 45) == t*m3x4)
    }

    test("Mutable math") {
        var u = Vec3(2, 3, 4)

        u = Vec3(2, 3, 4); u *= 2; assert(Vec3(4, 6, 8) == u)
        u = Vec3(2, 3, 4); u /= 2; assert(Vec3(1, 1.5, 2) == u)

        u = Vec3(2, 3, 4); u += Vec3(3, 4, 5); assert(Vec3(5, 7, 9) == u)
        u = Vec3(2, 3, 4); u += u; assert(Vec3(4, 6, 8) == u)
        u = Vec3(2, 3, 4); u -= Vec3(2, 3, 4); assert(Vec3(0, 0, 0) == u)
        u = Vec3(2, 3, 4); u -= u; assert(Vec3(0, 0, 0) == u)

        u = Vec3(2, 3, 4); u *= Vec3(5, 10, 15); assert(Vec3(10, 30, 60) == u)
        u = Vec3(2, 3, 4); u *= u; assert(Vec3(4, 9, 16) == u)
        u = Vec3(2, 3, 4); u /= Vec3(2, 6, 2); assert(Vec3(1, 0.5, 2) == u)
        u = Vec3(2, 3, 4); u /= u; assert(Vec3(1, 1, 1) == u)

        u = Vec3(2, 3, 4); u := Vec3(11, 12, 13); assert(Vec3(11, 12, 13) == u)
        u = Vec3(2, 3, 4); u.set(22, 33, 44); assert(Vec3(22, 33, 44) == u)

        u = Vec3(2, 3, 4)
        val m3 = ConstMat3(
            2, 5, 4,
            3, 4, 8,
            7, 4, 2
        )
        u *= m3
        assert(Vec3(35, 50, 34) == u)
    }
}
