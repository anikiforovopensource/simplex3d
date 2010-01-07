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

package test.math.floatm

import org.scalatest._

import simplex3d.math.floatm.renamed._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat4fTest extends FunSuite {
    val (m00, m10, m20, m30) = (1, 2, 3, 4)
    val (m01, m11, m21, m31) = (5, 6, 7, 8)
    val (m02, m12, m22, m32) = (9, 10, 11, 12)
    val (m03, m13, m23, m33) = (13, 14, 15, 16)

    test("Mutable factories") {
        var m = Mat4(2)
        expect(classOf[Mat4]) { m.getClass }
        expect((2, 0, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((0, 2, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 2, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 2)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Vec4d(m00, m10, m20, m30),
                 Vec4d(m01, m11, m21, m31),
                 Vec4d(m02, m12, m22, m32),
                 Vec4d(m03, m13, m23, m33))
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Vec4(m00, m10, m20, m30),
                 Vec4(m01, m11, m21, m31),
                 Vec4(m02, m12, m22, m32),
                 Vec4(m03, m13, m23, m33))
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(m00, m10, m20, m30,
                 m01, m11, m21, m31,
                 m02, m12, m22, m32,
                 m03, m13, m23, m33)
        expect(classOf[Mat4]) { m.getClass }
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat2x2(m00, m10,
                        m01, m11))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat2x3(m00, m10,
                        m01, m11,
                        m02, m12))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat2x4(m00, m10,
                        m01, m11,
                        m02, m12,
                        m03, m13))
        expect((m00, m10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat3x2(m00, m10, m20,
                        m01, m11, m21))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat3x3(m00, m10, m20,
                        m01, m11, m21,
                        m02, m12, m22))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat3x4(m00, m10, m20,
                        m01, m11, m21,
                        m02, m12, m22,
                        m03, m13, m23))
        expect((m00, m10, m20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat4x2(m00, m10, m20, m30,
                        m01, m11, m21, m31))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat4x3(m00, m10, m20, m30,
                        m01, m11, m21, m31,
                        m02, m12, m22, m32))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

        m = Mat4(Mat4x4(m00, m10, m20, m30,
                        m01, m11, m21, m31,
                        m02, m12, m22, m32,
                        m03, m13, m23, m33))
        expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
        expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
        expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
        expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }
    }

    test("Const conversions") {
        val mat1 = ConstMat4(m00, m10, m20, m30,
                             m01, m11, m21, m31,
                             m02, m12, m22, m32,
                             m03, m13, m23, m33)
        expect(classOf[ConstMat4]) { mat1.getClass }
        expect((m00, m10, m20, m30)) { (mat1.m00, mat1.m10, mat1.m20, mat1.m30)}
        expect((m01, m11, m21, m31)) { (mat1.m01, mat1.m11, mat1.m21, mat1.m31)}
        expect((m02, m12, m22, m32)) { (mat1.m02, mat1.m12, mat1.m22, mat1.m32)}
        expect((m03, m13, m23, m33)) { (mat1.m03, mat1.m13, mat1.m23, mat1.m33)}

        val mat2 = ConstMat4(Vec4(m00, m10, m20, m30),
                             Vec4(m01, m11, m21, m31),
                             Vec4(m02, m12, m22, m32),
                             Vec4(m03, m13, m23, m33))
        expect(classOf[ConstMat4]) { mat2.getClass }
        expect((m00, m10, m20, m30)) { (mat2.m00, mat2.m10, mat2.m20, mat2.m30)}
        expect((m01, m11, m21, m31)) { (mat2.m01, mat2.m11, mat2.m21, mat2.m31)}
        expect((m02, m12, m22, m32)) { (mat2.m02, mat2.m12, mat2.m22, mat2.m32)}
        expect((m03, m13, m23, m33)) { (mat2.m03, mat2.m13, mat2.m23, mat2.m33)}

        val mat3 = ConstMat4(Mat4(m00, m10, m20, m30,
                                  m01, m11, m21, m31,
                                  m02, m12, m22, m32,
                                  m03, m13, m23, m33))
        expect(classOf[ConstMat4]) { mat3.getClass }
        expect((m00, m10, m20, m30)) { (mat3.m00, mat3.m10, mat3.m20, mat3.m30)}
        expect((m01, m11, m21, m31)) { (mat3.m01, mat3.m11, mat3.m21, mat3.m31)}
        expect((m02, m12, m22, m32)) { (mat3.m02, mat3.m12, mat3.m22, mat3.m32)}
        expect((m03, m13, m23, m33)) { (mat3.m03, mat3.m13, mat3.m23, mat3.m33)}

        val i = Mat4(m00, m10, m20, m30,
                     m01, m11, m21, m31,
                     m02, m12, m22, m32,
                     m03, m13, m23, m33)

        val t: ConstMat4 = i
        expect(classOf[ConstMat4]) { t.getClass }
        assert(i == t)

        var c: ConstMat4 = i; var v = Mat4(2)
        expect(classOf[ConstMat4]) { c.getClass }
        v = c; assert(i == v)
        expect(classOf[Mat4]) { v.getClass }

        c = Mat4(2); v = i
        expect(classOf[Mat4]) { v.getClass }
        c = v; assert(i == c)
        expect(classOf[ConstMat4]) { c.getClass }
    }

    test("Equality methods") {
        val m = Mat4(m00, m10, m20, m30,
                     m01, m11, m21, m31,
                     m02, m12, m22, m32,
                     m03, m13, m23, m33)
        val c = ConstMat4(m00, m10, m20, m30,
                          m01, m11, m21, m31,
                          m02, m12, m22, m32,
                          m03, m13, m23, m33)
        assert(m == m)
        assert(m == c)
        assert(c == m)
        assert(c == c)

        assert(m.equals(c))
        assert(!m.equals(Nil))

        for (r <- 0 until 4; c <- 0 until 4) {
            val t = Mat4(c)
            t(c, r) = -1
            assert(t != c)
        }
    }

    test("Indexed read") {
        val m = ConstMat4(m00, m10, m20, m30,
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

    }

    test("Setters") {

    }

    test("Const math") {

    }

    test("Mutable math") {

    }
}
