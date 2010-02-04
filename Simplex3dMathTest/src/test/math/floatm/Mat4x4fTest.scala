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

package test.math.floatm

import org.scalatest._

import simplex3d.math.BaseMath._
import simplex3d.math.floatm.renamed._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat4x4fTest extends FunSuite {
    val (m00, m10, m20, m30) = (1f, 2f, 3f, 4f)
    val (m01, m11, m21, m31) = (5f, 6f, 7f, 8f)
    val (m02, m12, m22, m32) = (9f, 10f, 11f, 12f)
    val (m03, m13, m23, m33) = (13f, 14f, 15f, 16f)

    val (f00, f10, f20, f30) = (1f+1e-5f, 2f+1e-5f, 3f+1e-5f, 4f+1e-5f)
    val (f01, f11, f21, f31) = (5f+1e-5f, 6f+1e-5f, 7f+1e-5f, 8f+1e-5f)
    val (f02, f12, f22, f32) = (9f+1e-5f, 10f+1e-5f, 11f+1e-5f, 12f+1e-5f)
    val (f03, f13, f23, f33) = (13f, 14f, 15f, 16f)

    val (d00, d10, d20, d30) = (1+1e-5, 2+1e-5, 3+1e-5, 4+1e-5)
    val (d01, d11, d21, d31) = (5+1e-5, 6+1e-5, 7+1e-5, 8+1e-5)
    val (d02, d12, d22, d32) = (9+1e-5, 10+1e-5, 11+1e-5, 12+1e-5)
    val (d03, d13, d23, d33) = (13+1e-5, 14+1e-5, 15+1e-5, 16+1e-5)

    val M = Mat4(m00, m10, m20, m30,
                 m01, m11, m21, m31,
                 m02, m12, m22, m32,
                 m03, m13, m23, m33)

    test("Mutable factories") {
        var m = Mat4x4(2)
        expect(classOf[Mat4]) { m.getClass }
        expect((2, 0, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((0, 2, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 2, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 2)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Vec4d(m00, m10, m20, m30),
                   Vec4d(m01, m11, m21, m31),
                   Vec4d(m02, m12, m22, m32),
                   Vec4d(m03, m13, m23, m33))
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Vec4(m00, m10, m20, m30),
                   Vec4(m01, m11, m21, m31),
                   Vec4(m02, m12, m22, m32),
                   Vec4(m03, m13, m23, m33))
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(m00, m10, m20, m30,
                   m01, m11, m21, m31,
                   m02, m12, m22, m32,
                   m03, m13, m23, m33)
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat2x2(m00, m10,
                          m01, m11))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat2x3(m00, m10,
                          m01, m11,
                          m02, m12))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat2x4(m00, m10,
                          m01, m11,
                          m02, m12,
                          m03, m13))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat3x2(m00, m10, m20,
                          m01, m11, m21))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat3x3(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat3x4(m00, m10, m20,
                          m01, m11, m21,
                          m02, m12, m22,
                          m03, m13, m23))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat4x2(m00, m10, m20, m30,
                          m01, m11, m21, m31))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat4x3(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(Mat4x4(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32,
                          m03, m13, m23, m33))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }
    }

    test("Const conversions") {
        val mat1 = ConstMat4x4(m00, m10, m20, m30,
                               m01, m11, m21, m31,
                               m02, m12, m22, m32,
                               m03, m13, m23, m33)
        expect(classOf[ConstMat4]) { mat1.getClass }
        expect((m00, m10, m20, m30)) { (mat1.m00, mat1.m10, mat1.m20, mat1.m30)}
        expect((m01, m11, m21, m31)) { (mat1.m01, mat1.m11, mat1.m21, mat1.m31)}
        expect((m02, m12, m22, m32)) { (mat1.m02, mat1.m12, mat1.m22, mat1.m32)}
        expect((m03, m13, m23, m33)) { (mat1.m03, mat1.m13, mat1.m23, mat1.m33)}

        val mat2 = ConstMat4x4(Vec4(m00, m10, m20, m30),
                               Vec4(m01, m11, m21, m31),
                               Vec4(m02, m12, m22, m32),
                               Vec4(m03, m13, m23, m33))
        expect(classOf[ConstMat4]) { mat2.getClass }
        expect((m00, m10, m20, m30)) { (mat2.m00, mat2.m10, mat2.m20, mat2.m30)}
        expect((m01, m11, m21, m31)) { (mat2.m01, mat2.m11, mat2.m21, mat2.m31)}
        expect((m02, m12, m22, m32)) { (mat2.m02, mat2.m12, mat2.m22, mat2.m32)}
        expect((m03, m13, m23, m33)) { (mat2.m03, mat2.m13, mat2.m23, mat2.m33)}

        val mat3 = ConstMat4x4(Mat4x4(m00, m10, m20, m30,
                                      m01, m11, m21, m31,
                                      m02, m12, m22, m32,
                                      m03, m13, m23, m33))
        expect(classOf[ConstMat4]) { mat3.getClass }
        expect((m00, m10, m20, m30)) { (mat3.m00, mat3.m10, mat3.m20, mat3.m30)}
        expect((m01, m11, m21, m31)) { (mat3.m01, mat3.m11, mat3.m21, mat3.m31)}
        expect((m02, m12, m22, m32)) { (mat3.m02, mat3.m12, mat3.m22, mat3.m32)}
        expect((m03, m13, m23, m33)) { (mat3.m03, mat3.m13, mat3.m23, mat3.m33)}

        val i = Mat4x4(m00, m10, m20, m30,
                       m01, m11, m21, m31,
                       m02, m12, m22, m32,
                       m03, m13, m23, m33)

        val t: ConstMat4x4 = i
        expect(classOf[ConstMat4]) { t.getClass }
        assert(i == t)

        var c: ConstMat4x4 = i; var v = Mat4x4(2)
        expect(classOf[ConstMat4]) { c.getClass }
        v = c; assert(i == v)
        expect(classOf[Mat4]) { v.getClass }

        c = Mat4x4(2); v = i
        expect(classOf[Mat4]) { v.getClass }
        c = v; assert(i == c)
        expect(classOf[ConstMat4]) { c.getClass }
    }

    test("Equality methods") {
        val m = Mat4x4(m00, m10, m20, m30,
                       m01, m11, m21, m31,
                       m02, m12, m22, m32,
                       m03, m13, m23, m33)
        val n = ConstMat4x4(m00, m10, m20, m30,
                            m01, m11, m21, m31,
                            m02, m12, m22, m32,
                            m03, m13, m23, m33)
        assert(m == m)
        assert(m == n)
        assert(n == m)
        assert(n == n)

        assert(m.equals(n))
        assert(!m.equals(Nil))

        for (r <- 0 until 4; c <- 0 until 4) {
            val t = Mat4x4(n)
            t(c, r) = -1
            assert(t != n)
        }
    }

    test("Indexed read") {
        val m = ConstMat4x4(m00, m10, m20, m30,
                            m01, m11, m21, m31,
                            m02, m12, m22, m32,
                            m03, m13, m23, m33)

        var count = 0
        for (c <- 0 until 4; r <- 0 until 4) {
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
            m(1, 4)
        }
        intercept[IndexOutOfBoundsException] {
            m(1, -1)
        }

        expect(Vec4(m00, m10, m20, m30)) { m(0) }
        expect(Vec4(m01, m11, m21, m31)) { m(1) }
        expect(Vec4(m02, m12, m22, m32)) { m(2) }
        expect(Vec4(m03, m13, m23, m33)) { m(3) }

        expect(classOf[ConstVec4]) { m(0).getClass }
        expect(classOf[ConstVec4]) { m(1).getClass }
        expect(classOf[ConstVec4]) { m(2).getClass }
        expect(classOf[ConstVec4]) { m(3).getClass }

        intercept[IndexOutOfBoundsException] {
            m(4)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1)
        }
    }

    test("Indexed write") {
        var m = Mat4x4(m00, m10, m20, m30,
                       m01, m11, m21, m31,
                       m02, m12, m22, m32,
                       m03, m13, m23, m33)

        var count = 0
        for (c <- 0 until 4; r <- 0 until 4) {
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
            m(1, 4) = 1
        }
        intercept[IndexOutOfBoundsException] {
            m(1, -1) = 1
        }

        m = Mat4x4(0)

        m(0) = Vec4(m00, m10, m20, m30)
        m(1) = Vec4(m01, m11, m21, m31)
        m(2) = Vec4(m02, m12, m22, m32)
        m(3) = Vec4(m03, m13, m23, m33)

        expect(Vec4(m00, m10, m20, m30)) { m(0) }
        expect(Vec4(m01, m11, m21, m31)) { m(1) }
        expect(Vec4(m02, m12, m22, m32)) { m(2) }
        expect(Vec4(m03, m13, m23, m33)) { m(3) }

        m = Mat4x4(0)

        m(0) = Vec3(m00, m10, m20)
        m(1) = Vec3(m01, m11, m21)
        m(2) = Vec3(m02, m12, m22)
        m(3) = Vec3(m03, m13, m23)

        expect(Vec4(m00, m10, m20, 0)) { m(0) }
        expect(Vec4(m01, m11, m21, 0)) { m(1) }
        expect(Vec4(m02, m12, m22, 0)) { m(2) }
        expect(Vec4(m03, m13, m23, 0)) { m(3) }

        m = Mat4x4(0)

        m(0) = Vec2(m00, m10)
        m(1) = Vec2(m01, m11)
        m(2) = Vec2(m02, m12)
        m(3) = Vec2(m03, m13)

        expect(Vec4(m00, m10, 0, 0)) { m(0) }
        expect(Vec4(m01, m11, 0, 0)) { m(1) }
        expect(Vec4(m02, m12, 0, 0)) { m(2) }
        expect(Vec4(m03, m13, 0, 0)) { m(3) }

        intercept[IndexOutOfBoundsException] {
            m(4) = Vec4(1)
            m(4) = Vec3(1)
            m(4) = Vec2(1)
        }
        intercept[IndexOutOfBoundsException] {
            m(-1) = Vec4(1)
            m(-1) = Vec3(1)
            m(-1) = Vec2(1)
        }
    }

    test("Setters") {
        var m = Mat4x4(0)
        val i = ConstMat4x4(m00, m10, m20, m30,
                            m01, m11, m21, m31,
                            m02, m12, m22, m32,
                            m03, m13, m23, m33)

        m = Mat4x4(0)
        m := i
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4x4(0)
        m.set(m00, m10, m20, m30,
              m01, m11, m21, m31,
              m02, m12, m22, m32,
              m03, m13, m23, m33)
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }
    }

    test("Const math") {
        val m = ConstMat4x4(m00, m10, m20, m30,
                            m01, m11, m21, m31,
                            m02, m12, m22, m32,
                            m03, m13, m23, m33)

        var t = Mat4x4(-m00, -m10, -m20, -m30,
                       -m01, -m11, -m21, -m31,
                       -m02, -m12, -m22, -m32,
                       -m03, -m13, -m23, -m33)
        assert(-m == t)

        t = Mat4x4(2*m00, 2*m10, 2*m20, 2*m30,
                   2*m01, 2*m11, 2*m21, 2*m31,
                   2*m02, 2*m12, 2*m22, 2*m32,
                   2*m03, 2*m13, 2*m23, 2*m33)
        assert(m*2 == t)
        assert(2*m == t)
        assert(2f*m == t)

        t = Mat4x4(m00/2, m10/2, m20/2, m30/2,
                   m01/2, m11/2, m21/2, m31/2,
                   m02/2, m12/2, m22/2, m32/2,
                   m03/2, m13/2, m23/2, m33/2)
        assert(m/2 == t)
        
        t = Mat4x4(2/m00, 2/m10, 2/m20, 2/m30,
                   2/m01, 2/m11, 2/m21, 2/m31,
                   2/m02, 2/m12, 2/m22, 2/m32,
                   2/m03, 2/m13, 2/m23, 2/m33)
        assert(2/m == t)
        assert(2f/m == t)

        val n: ConstMat4x4 = m*3
        t = Mat4x4(4*m00, 4*m10, 4*m20, 4*m30,
                   4*m01, 4*m11, 4*m21, 4*m31,
                   4*m02, 4*m12, 4*m22, 4*m32,
                   4*m03, 4*m13, 4*m23, 4*m33)
        assert(n + m == t)

        t = Mat4x4(2*m00, 2*m10, 2*m20, 2*m30,
                   2*m01, 2*m11, 2*m21, 2*m31,
                   2*m02, 2*m12, 2*m22, 2*m32,
                   2*m03, 2*m13, 2*m23, 2*m33)
        assert(n - m == t)

        t = Mat4x4(3, 3, 3, 3,
                   3, 3, 3, 3,
                   3, 3, 3, 3,
                   3, 3, 3, 3)
        assert(n / m == t)

        
        val mul42 = Mat4x2(90, 100, 110, 120,
                           202, 228, 254, 280)
        assert(m*Mat4x2(M) == mul42)

        val mul43 = Mat4x3(90, 100, 110, 120,
                           202, 228, 254, 280,
                           314, 356, 398, 440)
        assert(m*Mat4x3(M) == mul43)

        val mul44 = Mat4x4(90, 100, 110, 120,
                           202, 228, 254, 280,
                           314, 356, 398, 440,
                           426, 484, 542, 600)
        assert(m*M == mul44)

        assert(m*Vec4(1, 2, 3, 4) == Vec4(90, 100, 110, 120))
    }

    test("Mutable math") {
        val m = Mat4x4(0)
        val i = ConstMat4x4(m00, m10, m20, m30,
                            m01, m11, m21, m31,
                            m02, m12, m22, m32,
                            m03, m13, m23, m33)

        var t = Mat4x4(2*m00, 2*m10, 2*m20, 2*m30,
                       2*m01, 2*m11, 2*m21, 2*m31,
                       2*m02, 2*m12, 2*m22, 2*m32,
                       2*m03, 2*m13, 2*m23, 2*m33)
        m := i; m *= 2; assert(m == t)

        t = Mat4x4(m00/2, m10/2, m20/2, m30/2,
                   m01/2, m11/2, m21/2, m31/2,
                   m02/2, m12/2, m22/2, m32/2,
                   m03/2, m13/2, m23/2, m33/2)
        m := i; m /= 2; assert(m == t)

        val n: ConstMat4x4 = i*3

        t = Mat4x4(4*m00, 4*m10, 4*m20, 4*m30,
                   4*m01, 4*m11, 4*m21, 4*m31,
                   4*m02, 4*m12, 4*m22, 4*m32,
                   4*m03, 4*m13, 4*m23, 4*m33)
        m := i; m += n; assert(m == t)

        t = Mat4x4(-2*m00, -2*m10, -2*m20, -2*m30,
                   -2*m01, -2*m11, -2*m21, -2*m31,
                   -2*m02, -2*m12, -2*m22, -2*m32,
                   -2*m03, -2*m13, -2*m23, -2*m33)
        m := i; m -= n; assert(m == t)

        t = Mat4x4(90, 100, 110, 120,
                   202, 228, 254, 280,
                   314, 356, 398, 440,
                   426, 484, 542, 600)
        m := i; m *= m; assert(m == t)
    }
}
