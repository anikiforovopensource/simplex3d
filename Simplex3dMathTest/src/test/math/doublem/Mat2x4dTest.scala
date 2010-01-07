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
class Mat2x4dTest extends FunSuite {
    val (m00, m10, m20, m30) = (1d, 2d, 3d, 4d)
    val (m01, m11, m21, m31) = (5d, 6d, 7d, 8d)
    val (m02, m12, m22, m32) = (9d, 10d, 11d, 12d)
    val (m03, m13, m23, m33) = (13d, 14d, 15d, 16d)

    val M = Mat4(m00, m10, m20, m30,
                 m01, m11, m21, m31,
                 m02, m12, m22, m32,
                 m03, m13, m23, m33)

    test("Mutable factories") {
        var m = Mat2x4(2)
        expect(classOf[Mat2x4]) { m.getClass }
        expect((2, 0)) { (m.m00, m.m10) }
        expect((0, 2)) { (m.m01, m.m11) }
        expect((0, 0)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Vec2f(Vec2(m00, m10)),
                   Vec2f(Vec2(m01, m11)),
                   Vec2f(Vec2(m02, m12)),
                   Vec2f(Vec2(m03, m13)))
        expect(classOf[Mat2x4]) { m.getClass }
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(Vec2(m00, m10),
                   Vec2(m01, m11),
                   Vec2(m02, m12),
                   Vec2(m03, m13))
        expect(classOf[Mat2x4]) { m.getClass }
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(m00, m10,
                   m01, m11,
                   m02, m12,
                   m03, m13)
        expect(classOf[Mat2x4]) { m.getClass }
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(Mat2x2(m00, m10,
                          m01, m11))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((0, 0)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat2x3(m00, m10,
                          m01, m11,
                          m02, m12))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat2x4(m00, m10,
                          m01, m11,
                          m02, m12,
                          m03, m13))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(Mat3x2(m00, m10, m20,
                          m01, m11, m21))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((0, 0)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat3x3(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat3x4(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22,
                          m03, m13, m23))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(Mat4x2(m00, m10, m20, m30,
                          m01, m11, m21, m31))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((0, 0)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat4x3(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((0, 0)) { (m.m03, m.m13) }

        m = Mat2x4(Mat4x4(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32,
                          m03, m13, m23, m33))
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }
    }

    test("Const conversions") {
        val mat1 = ConstMat2x4(m00, m10,
                               m01, m11,
                               m02, m12,
                               m03, m13)
        expect(classOf[ConstMat2x4]) { mat1.getClass }
        expect((m00, m10)) { (mat1.m00, mat1.m10) }
        expect((m01, m11)) { (mat1.m01, mat1.m11) }
        expect((m02, m12)) { (mat1.m02, mat1.m12) }
        expect((m03, m13)) { (mat1.m03, mat1.m13) }

        val mat2 = ConstMat2x4(Vec2(m00, m10),
                               Vec2(m01, m11),
                               Vec2(m02, m12),
                               Vec2(m03, m13))
        expect(classOf[ConstMat2x4]) { mat2.getClass }
        expect((m00, m10)) { (mat2.m00, mat2.m10) }
        expect((m01, m11)) { (mat2.m01, mat2.m11) }
        expect((m02, m12)) { (mat2.m02, mat2.m12) }
        expect((m03, m13)) { (mat2.m03, mat2.m13) }

        val mat3 = ConstMat2x4(Mat2x4(m00, m10,
                                      m01, m11,
                                      m02, m12,
                                      m03, m13))
        expect(classOf[ConstMat2x4]) { mat3.getClass }
        expect((m00, m10)) { (mat3.m00, mat3.m10) }
        expect((m01, m11)) { (mat3.m01, mat3.m11) }
        expect((m02, m12)) { (mat3.m02, mat3.m12) }
        expect((m03, m13)) { (mat3.m03, mat3.m13) }

        val i = Mat2x4(m00, m10,
                       m01, m11,
                       m02, m12,
                       m03, m13)

        val t: ConstMat2x4 = i
        expect(classOf[ConstMat2x4]) { t.getClass }
        assert(i == t)

        var c: ConstMat2x4 = i; var v = Mat2x4(2)
        expect(classOf[ConstMat2x4]) { c.getClass }
        v = c; assert(i == v)
        expect(classOf[Mat2x4]) { v.getClass }

        c = Mat2x4(2); v = i
        expect(classOf[Mat2x4]) { v.getClass }
        c = v; assert(i == c)
        expect(classOf[ConstMat2x4]) { c.getClass }
    }

    test("Equality methods") {
        val m = Mat2x4(m00, m10,
                       m01, m11,
                       m02, m12,
                       m03, m13)
        val n = ConstMat2x4(m00, m10,
                            m01, m11,
                            m02, m12,
                            m03, m13)
        assert(m == m)
        assert(m == n)
        assert(n == m)
        assert(n == n)

        assert(m.equals(n))
        assert(!m.equals(Nil))

        for (r <- 0 until 2; c <- 0 until 4) {
            val t = Mat2x4(n)
            t(c, r) = -1
            assert(t != n)
        }
    }

    test("Indexed read") {
        {
            val m = ConstMat2x4(1, 2,
                                3, 4,
                                5, 6,
                                7, 8)

            var count = 0
            for (c <- 0 until 4; r <- 0 until 2) {
                count += 1
                expect(count) { m(c, r) }
            }

            intercept[IndexOutOfBoundsException] {
                m(4, 1)
            }
            intercept[IndexOutOfBoundsException] {
                m(-1, 1)
            }

            intercept[IndexOutOfBoundsException] {
                m(1, 2)
            }
            intercept[IndexOutOfBoundsException] {
                m(1, -1)
            }
        }

        val m = ConstMat2x4(m00, m10,
                            m01, m11,
                            m02, m12,
                            m03, m13)

        expect(Vec2(m00, m10)) { m(0) }
        expect(Vec2(m01, m11)) { m(1) }
        expect(Vec2(m02, m12)) { m(2) }
        expect(Vec2(m03, m13)) { m(3) }

        expect(classOf[ConstVec2]) { m(0).getClass }
        expect(classOf[ConstVec2]) { m(1).getClass }
        expect(classOf[ConstVec2]) { m(2).getClass }
        expect(classOf[ConstVec2]) { m(3).getClass }

        intercept[IndexOutOfBoundsException] {
            m(4)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1)
        }
    }

    test("Indexed write") {
        var m = Mat2x4(m00, m10,
                       m01, m11,
                       m02, m12,
                       m03, m13)

        var count = 0
        for (c <- 0 until 4; r <- 0 until 2) {
            count += 1
            m(c, r) = count + 1
            expect(count + 1) { m(c, r) }
        }

        intercept[IndexOutOfBoundsException] {
            m(4, 1) = 1
        }
        intercept[IndexOutOfBoundsException] {
            m(-1, 1) = 1
        }

        intercept[IndexOutOfBoundsException] {
            m(1, 2) = 1
        }
        intercept[IndexOutOfBoundsException] {
            m(1, -1) = 1
        }

        m = Mat2x4(0)

        m(0) = Vec2(m00, m10)
        m(1) = Vec2(m01, m11)
        m(2) = Vec2(m02, m12)
        m(3) = Vec2(m03, m13)

        expect(Vec2(m00, m10)) { m(0) }
        expect(Vec2(m01, m11)) { m(1) }
        expect(Vec2(m02, m12)) { m(2) }
        expect(Vec2(m03, m13)) { m(3) }

        intercept[IndexOutOfBoundsException] {
            m(4) = Vec2(1)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1) = Vec2(1)
        }
    }

    test("Setters") {
        var m = Mat2x4(0)
        val i = ConstMat2x4(m00, m10,
                            m01, m11,
                            m02, m12,
                            m03, m13)

        m = Mat2x4(0)
        m := i
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }

        m = Mat2x4(0)
        m.set(m00, m10,
              m01, m11,
              m02, m12,
              m03, m13)
        expect((m00, m10)) { (m.m00, m.m10) }
        expect((m01, m11)) { (m.m01, m.m11) }
        expect((m02, m12)) { (m.m02, m.m12) }
        expect((m03, m13)) { (m.m03, m.m13) }
    }

    test("Const math") {
        val m = ConstMat2x4(m00, m10,
                            m01, m11,
                            m02, m12,
                            m03, m13)

        var t = Mat2x4(-m00, -m10,
                       -m01, -m11,
                       -m02, -m12,
                       -m03, -m13)
        assert(-m == t)

        t = Mat2x4(2*m00, 2*m10,
                   2*m01, 2*m11,
                   2*m02, 2*m12,
                   2*m03, 2*m13)
        assert(m*2 == t)
        assert(2*m == t)
        assert(2f*m == t)
        assert(2d*m == t)

        t = Mat2x4(m00/2, m10/2,
                   m01/2, m11/2,
                   m02/2, m12/2,
                   m03/2, m13/2)
        assert(m/2 == t)
        
        t = Mat2x4(2/m00, 2/m10,
                   2/m01, 2/m11,
                   2/m02, 2/m12,
                   2/m03, 2/m13)
        assert(2/m == t)
        assert(2f/m == t)
        assert(2d/m == t)

        val n: ConstMat2x4 = m*3
        t = Mat2x4(4*m00, 4*m10,
                   4*m01, 4*m11,
                   4*m02, 4*m12,
                   4*m03, 4*m13)
        assert(n + m == t)

        t = Mat2x4(2*m00, 2*m10,
                   2*m01, 2*m11,
                   2*m02, 2*m12,
                   2*m03, 2*m13)
        assert(n - m == t)

        t = Mat2x4(3, 3,
                   3, 3,
                   3, 3,
                   3, 3)
        assert(n / m == t)

        
        val mul42 = Mat2x2(90, 100,
                           202, 228)
        assert(m*Mat4x2(M) == mul42)

        val mul43 = Mat2x3(90, 100,
                           202, 228,
                           314, 356)
        assert(m*Mat4x3(M) == mul43)

        val mul44 = Mat2x4(90, 100,
                           202, 228,
                           314, 356,
                           426, 484)
        assert(m*M == mul44)

        assert(m*Vec4(1, 2, 3, 4) == Vec2(90, 100))
    }

    test("Mutable math") {
        val m = Mat2x4(0)
        val i = ConstMat2x4(m00, m10,
                            m01, m11,
                            m02, m12,
                            m03, m13)

        var t = Mat2x4(2*m00, 2*m10,
                       2*m01, 2*m11,
                       2*m02, 2*m12,
                       2*m03, 2*m13)
        m := i; m *= 2; assert(m == t)

        t = Mat2x4(m00/2, m10/2,
                   m01/2, m11/2,
                   m02/2, m12/2,
                   m03/2, m13/2)
        m := i; m /= 2; assert(m == t)

        val n: ConstMat2x4 = i*3

        t = Mat2x4(4*m00, 4*m10,
                   4*m01, 4*m11,
                   4*m02, 4*m12,
                   4*m03, 4*m13)
        m := i; m += n; assert(m == t)

        t = Mat2x4(-2*m00, -2*m10,
                   -2*m01, -2*m11,
                   -2*m02, -2*m12,
                   -2*m03, -2*m13)
        m := i; m -= n; assert(m == t)

        t = Mat2x4(90, 100,
                   202, 228,
                   314, 356,
                   426, 484)
        m := i; m *= M; assert(m == t)
    }
}
