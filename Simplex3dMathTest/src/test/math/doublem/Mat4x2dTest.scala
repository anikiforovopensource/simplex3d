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

package test.math.doublem

import org.scalatest._

import simplex3d.math.doublem.renamed._
import simplex3d.math.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat4x2dTest extends FunSuite {
    val (m00, m10, m20, m30) = (1d, 2d, 3d, 4d)
    val (m01, m11, m21, m31) = (5d, 6d, 7d, 8d)
    val (m02, m12, m22, m32) = (9d, 10d, 11d, 12d)
    val (m03, m13, m23, m33) = (13d, 14d, 15d, 16d)

    val M = Mat4(m00, m10, m20, m30,
                 m01, m11, m21, m31,
                 m02, m12, m22, m32,
                 m03, m13, m23, m33)

    test("Mutable factories") {
        var m = Mat4x2(2)
        expect(classOf[Mat4x2]) { m.getClass }
        expect((2, 0, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((0, 2, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Vec4f(Vec4(m00, m10, m20, m30)),
                   Vec4f(Vec4(m01, m11, m21, m31)))
        expect(classOf[Mat4x2]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Vec4(m00, m10, m20, m30),
                   Vec4(m01, m11, m21, m31))
        expect(classOf[Mat4x2]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(m00, m10, m20, m30,
                   m01, m11, m21, m31)
        expect(classOf[Mat4x2]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat2x2(m00, m10,
                          m01, m11))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat2x3(m00, m10,
                          m01, m11,
                          m02, m12))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat2x4(m00, m10,
                          m01, m11,
                          m02, m12,
                          m03, m13))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat3x2(m00, m10, m20,
                          m01, m11, m21))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat3x3(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat3x4(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22,
                          m03, m13, m23))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat4x2(m00, m10, m20, m30,
                          m01, m11, m21, m31))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat4x3(m00, m10, m20, m30,
                         m01, m11, m21, m31,
                         m02, m12, m22, m32))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(Mat4x4(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32,
                          m03, m13, m23, m33))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
    }

    test("Const conversions") {
        val mat1 = ConstMat4x2(m00, m10, m20, m30,
                               m01, m11, m21, m31)
        expect(classOf[ConstMat4x2]) { mat1.getClass }
        expect((m00, m10, m20, m30)) { (mat1.m00, mat1.m10, mat1.m20, mat1.m30)}
        expect((m01, m11, m21, m31)) { (mat1.m01, mat1.m11, mat1.m21, mat1.m31)}

        val mat2 = ConstMat4x2(Vec4(m00, m10, m20, m30),
                               Vec4(m01, m11, m21, m31))
        expect(classOf[ConstMat4x2]) { mat2.getClass }
        expect((m00, m10, m20, m30)) { (mat2.m00, mat2.m10, mat2.m20, mat2.m30)}
        expect((m01, m11, m21, m31)) { (mat2.m01, mat2.m11, mat2.m21, mat2.m31)}

        val mat3 = ConstMat4x2(Mat4x2(m00, m10, m20, m30,
                                      m01, m11, m21, m31))
        expect(classOf[ConstMat4x2]) { mat3.getClass }
        expect((m00, m10, m20, m30)) { (mat3.m00, mat3.m10, mat3.m20, mat3.m30)}
        expect((m01, m11, m21, m31)) { (mat3.m01, mat3.m11, mat3.m21, mat3.m31)}

        val i = Mat4x2(m00, m10, m20, m30,
                       m01, m11, m21, m31)

        val t: ConstMat4x2 = i
        expect(classOf[ConstMat4x2]) { t.getClass }
        assert(i == t)

        var c: ConstMat4x2 = i; var v = Mat4x2(2)
        expect(classOf[ConstMat4x2]) { c.getClass }
        v = c; assert(i == v)
        expect(classOf[Mat4x2]) { v.getClass }

        c = Mat4x2(2); v = i
        expect(classOf[Mat4x2]) { v.getClass }
        c = v; assert(i == c)
        expect(classOf[ConstMat4x2]) { c.getClass }
    }

    test("Equality methods") {
        val m = Mat4x2(m00, m10, m20, m30,
                       m01, m11, m21, m31)
        val n = ConstMat4x2(m00, m10, m20, m30,
                            m01, m11, m21, m31)
        assert(m == m)
        assert(m == n)
        assert(n == m)
        assert(n == n)

        assert(m.equals(n))
        assert(!m.equals(Nil))

        for (r <- 0 until 4; c <- 0 until 2) {
            val t = Mat4x2(n)
            t(c, r) = -1
            assert(t != n)
        }
    }

    test("Indexed read") {
        val m = ConstMat4x2(m00, m10, m20, m30,
                            m01, m11, m21, m31)

        var count = 0
        for (c <- 0 until 2; r <- 0 until 4) {
            count += 1
            expect(count) { m(c, r) }
        }

        intercept[IndexOutOfBoundsException] {
            m(2, 1)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1, 1)
        }

        intercept[IndexOutOfBoundsException] {
            m(1, 4)
        }
        intercept[IndexOutOfBoundsException] {
            m(1, -1)
        }

        expect(Vec4(m00, m10, m20, m30)) { m(0) }
        expect(Vec4(m01, m11, m21, m31)) { m(1) }

        expect(classOf[ConstVec4]) { m(0).getClass }
        expect(classOf[ConstVec4]) { m(1).getClass }

        intercept[IndexOutOfBoundsException] {
            m(2)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1)
        }
    }

    test("Indexed write") {
        var m = Mat4x2(m00, m10, m20, m30,
                       m01, m11, m21, m31)

        var count = 0
        for (c <- 0 until 2; r <- 0 until 4) {
            count += 1
            m(c, r) = count + 1
            expect(count + 1) { m(c, r) }
        }

        intercept[IndexOutOfBoundsException] {
            m(2, 1) = 1
        }
        intercept[IndexOutOfBoundsException] {
            m(-1, 1) = 1
        }

        intercept[IndexOutOfBoundsException] {
            m(1, 4) = 1
        }
        intercept[IndexOutOfBoundsException] {
            m(1, -1) = 1
        }

        m = Mat4x2(0)

        m(0) = Vec4(m00, m10, m20, m30)
        m(1) = Vec4(m01, m11, m21, m31)

        expect(Vec4(m00, m10, m20, m30)) { m(0) }
        expect(Vec4(m01, m11, m21, m31)) { m(1) }

        m = Mat4x2(0)

        m(0) = Vec3(m00, m10, m20)
        m(1) = Vec3(m01, m11, m21)

        expect(Vec4(m00, m10, m20, 0)) { m(0) }
        expect(Vec4(m01, m11, m21, 0)) { m(1) }

        m = Mat4x2(0)

        m(0) = Vec2(m00, m10)
        m(1) = Vec2(m01, m11)

        expect(Vec4(m00, m10, 0, 0)) { m(0) }
        expect(Vec4(m01, m11, 0, 0)) { m(1) }

        intercept[IndexOutOfBoundsException] {
            m(2) = Vec4(1)
            m(2) = Vec3(1)
            m(2) = Vec2(1)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1) = Vec4(1)
            m(-1) = Vec3(1)
            m(-1) = Vec2(1)
        }
    }

    test("Setters") {
        var m = Mat4x2(0)
        val i = ConstMat4x2(m00, m10, m20, m30,
                            m01, m11, m21, m31)

        m = Mat4x2(0)
        m := i
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }

        m = Mat4x2(0)
        m.set(m00, m10, m20, m30,
              m01, m11, m21, m31)
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
    }

    test("Const math") {
        val m = ConstMat4x2(m00, m10, m20, m30,
                            m01, m11, m21, m31)

        var t = Mat4x2(-m00, -m10, -m20, -m30,
                       -m01, -m11, -m21, -m31)
        assert(-m == t)

        t = Mat4x2(2*m00, 2*m10, 2*m20, 2*m30,
                   2*m01, 2*m11, 2*m21, 2*m31)
        assert(m*2 == t)
        assert(2*m == t)
        assert(2f*m == t)
        assert(2d*m == t)

        t = Mat4x2(m00/2, m10/2, m20/2, m30/2,
                   m01/2, m11/2, m21/2, m31/2)
        assert(m/2 == t)
        
        t = Mat4x2(2/m00, 2/m10, 2/m20, 2/m30,
                   2/m01, 2/m11, 2/m21, 2/m31)
        assert(2/m == t)
        assert(2f/m == t)
        assert(2d/m == t)

        val n: ConstMat4x2 = m*3
        t = Mat4x2(4*m00, 4*m10, 4*m20, 4*m30,
                   4*m01, 4*m11, 4*m21, 4*m31)
        assert(n + m == t)

        t = Mat4x2(2*m00, 2*m10, 2*m20, 2*m30,
                   2*m01, 2*m11, 2*m21, 2*m31)
        assert(n - m == t)

        t = Mat4x2(3, 3, 3, 3,
                   3, 3, 3, 3)
        assert(n / m == t)


        val mul22 = Mat4x2(11, 14, 17, 20,
                           35, 46, 57, 68)
        assert(m*Mat2x2(M) == mul22)

        val mul23 = Mat4x3(11, 14, 17, 20,
                           35, 46, 57, 68,
                           59, 78, 97, 116)
        assert(m*Mat2x3(M) == mul23)

        val mul24 = Mat4x4(11, 14, 17, 20,
                           35, 46, 57, 68,
                           59, 78, 97, 116,
                           83, 110, 137, 164)
        assert(m*Mat2x4(M) == mul24)

        assert(m*Vec2(1, 2) == Vec4(11, 14, 17, 20))
    }

    test("Mutable math") {
        val m = Mat4x2(0)
        val i = ConstMat4x2(m00, m10, m20, m30,
                            m01, m11, m21, m31)

        var t = Mat4x2(2*m00, 2*m10, 2*m20, 2*m30,
                       2*m01, 2*m11, 2*m21, 2*m31)
        m := i; m *= 2; assert(m == t)

        t = Mat4x2(m00/2, m10/2, m20/2, m30/2,
                   m01/2, m11/2, m21/2, m31/2)
        m := i; m /= 2; assert(m == t)

        val n: ConstMat4x2 = i*3

        t = Mat4x2(4*m00, 4*m10, 4*m20, 4*m30,
                   4*m01, 4*m11, 4*m21, 4*m31)
        m := i; m += n; assert(m == t)

        t = Mat4x2(-2*m00, -2*m10, -2*m20, -2*m30,
                   -2*m01, -2*m11, -2*m21, -2*m31)
        m := i; m -= n; assert(m == t)

        t = Mat4x2(11, 14, 17, 20,
                   35, 46, 57, 68)
        m := i; m *= Mat2(M); assert(m == t)
    }
}
