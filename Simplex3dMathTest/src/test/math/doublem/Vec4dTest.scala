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

package test.math.doublem

import org.scalatest._

import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4dTest extends FunSuite {

    test("Mutable factories") {
        var u = Vec4(5)
        expect(classOf[Vec4]) { u.getClass }
        expect(5) { u.x }
        expect(5) { u.y }
        expect(5) { u.z }
        expect(5) { u.w }

        u = Vec4(2, 3, 4, 5)
        expect(classOf[Vec4]) { u.getClass }
        expect(2) { u.x }
        expect(3) { u.y }
        expect(4) { u.z }
        expect(5) { u.w }

        u = Vec4(6, 7, Vec2(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec2(7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2(6, 7), 8, 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2(6, 7), Vec2(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec3(7, 8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec3(6, 7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec4(4, 5, 6, 7))
        expect(classOf[Vec4]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }
        expect(7) { u.w }

        u = Vec4(6, 7, Vec2i(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec2i(7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2i(6, 7), 8, 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2i(6, 7), Vec2i(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec3i(7, 8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec3i(6, 7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec4i(4, 5, 6, 7))
        expect(classOf[Vec4]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }
        expect(7) { u.w }

        u = Vec4(6, 7, Vec2f(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec2f(7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2f(6, 7), 8, 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec2f(6, 7), Vec2f(8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(6, Vec3f(7, 8, 9))
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec3f(6, 7, 8), 9)
        expect(classOf[Vec4]) { u.getClass }
        expect(6) { u.x }
        expect(7) { u.y }
        expect(8) { u.z }
        expect(9) { u.w }

        u = Vec4(Vec4f(4, 5, 6, 7))
        expect(classOf[Vec4]) { u.getClass }
        expect(4) { u.x }
        expect(5) { u.y }
        expect(6) { u.z }
        expect(7) { u.w }
    }

    test("Const conversions") {
        val x = 1d
        val y = 2d
        val z = 3d
        val w = 4d

        val a = ConstVec4(x, y, z, w)
        expect(x) { a.x }
        expect(y) { a.y }
        expect(z) { a.z }
        expect(w) { a.w }

        val b = ConstVec4(Vec4(x, y, z, w))
        expect(x) { b.x }
        expect(y) { b.y }
        expect(z) { b.z }
        expect(w) { b.w }

        val t: ConstVec4 = Vec4(x, y, z, w)
        assert(Vec4(x, y, z, w) == t)

        var c: ConstVec4 = Vec4(x, y, z, w); var v = Vec4(3)
        v = c; assert(Vec4(x, y, z, w) == v)

        c = Vec4(5); v = Vec4(x, y, z, w)
        c = v; assert(Vec4(x, y, z, w) == c)
    }

    test("Equality methods") {
        assert(Vec4(4, 7, 9, 1) == ConstVec4(4, 7, 9, 1))
        assert(ConstVec4(4, 7, 9, 1) == Vec4(4, 7, 9, 1))

        assert(Vec4(1, 2, 3, 4) != Vec4(9, 2, 3, 4))
        assert(Vec4(1, 2, 3, 4) != Vec4(1, 9, 3, 4))
        assert(Vec4(1, 2, 3, 4) != Vec4(1, 2, 9, 4))
        assert(Vec4(1, 2, 3, 4) != Vec4(1, 2, 3, 9))
    }

    test("Indexed read") {
        val u = ConstVec4(3, 4, 5, 6)

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
        val u = Vec4(3, 4, 5, 6)

        u(0) = 5
        assert(Vec4(5, 4, 5, 6) == u)

        u(1) = 6
        assert(Vec4(5, 6, 5, 6) == u)

        u(2) = 7
        assert(Vec4(5, 6, 7, 6) == u)

        u(3) = 8
        assert(Vec4(5, 6, 7, 8) == u)

        intercept[IndexOutOfBoundsException] {
            u(4) = 1
        }
        intercept[IndexOutOfBoundsException] {
            u(-1) = 1
        }
    }

    test("Setters") {
        val u = Vec4(0)

        u := Vec4(1, 2, 3, 4)
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
        val u = ConstVec4(6, 7, 8, 9)

        assert(Vec4(-6, -7, -8, -9) == -u)

        assert(Vec4(12, 14, 16, 18) == u*2)
        assert(Vec4(12, 14, 16, 18) == 2*u)
        assert(Vec4(12, 14, 16, 18) == 2f*u)
        assert(Vec4(12, 14, 16, 18) == 2d*u)
        assert(Vec4(3, 3.5, 4, 4.5) == u/2)
        assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7/u)
        assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7f/u)
        assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7d/u)

        val v = ConstVec4(2, 3, 4, 5)

        assert(Vec4(8, 10, 12, 14) == u + v)
        assert(Vec4(4) == u - v)
        assert(Vec4(12, 21, 32, 45) == u*v)
        assert(Vec4(3, 7/3d, 2, 9/5d) == u/v)

        val m4x2 = ConstMat4x2(
            2, 5, 4, 6,
            3, 4, 8, 2
        )
        assert(Vec2(65, 60) == v*m4x2)

        val m4x3 = ConstMat4x3(
            2, 5, 4, 6,
            3, 4, 8, 2,
            7, 4, 2, 5
        )
        assert(Vec3(65, 60, 59) == v*m4x3)

        val m4 = ConstMat4(
            2, 5, 4, 6,
            3, 4, 8, 2,
            7, 4, 2, 5,
            5, 9, 2, 3
        )
        assert(Vec4(65, 60, 59, 60) == v*m4)
    }

    test("Mutable math") {
        val u = Vec4(0)
        val i = ConstVec4(2, 3, 4, 5)

        u := i; u *= 2; assert(Vec4(4, 6, 8, 10) == u)
        u := i; u /= 2; assert(Vec4(1, 1.5, 2, 2.5) == u)

        u := i; u += Vec4(3, 4, 5, 6); assert(Vec4(5, 7, 9, 11) == u)
        u := i; u += u; assert(Vec4(4, 6, 8, 10) == u)
        u := i; u -= Vec4(2, 3, 4, 5); assert(Vec4(0) == u)
        u := i; u -= u; assert(Vec4(0) == u)

        u := i; u *= Vec4(5, 10, 15, 2); assert(Vec4(10, 30, 60, 10) == u)
        u := i; u *= u; assert(Vec4(4, 9, 16, 25) == u)
        u := i; u /= Vec4(2, 6, 2, 2); assert(Vec4(1, 0.5, 2, 2.5) == u)
        u := i; u /= u; assert(Vec4(1) == u)

        u := i
        val m4 = ConstMat4(
            2, 5, 4, 6,
            3, 4, 8, 2,
            7, 4, 2, 5,
            5, 9, 2, 3
        )
        u *= m4
        assert(Vec4(65, 60, 59, 60) == u)
    }
}
