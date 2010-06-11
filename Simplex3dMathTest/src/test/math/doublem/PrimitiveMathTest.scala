/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
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
class PrimitiveMathTest extends FunSuite {

  val M = Mat4x4f(
    1, 2, 3, 4, 5, 6, 7, 8,
    9, 10, 11, 12, 13, 14, 15, 16
  )
  val C = 7 + 1e-9

  test("doublem Primitive Math") {
    {
      val u = ConstVec2(7, 8)

      assert(Vec2(14, 16) == 2*u)
      assert(Vec2(14, 16) == 2f*u)
      assert(Vec2(14, 16) == 2d*u)
      assert(Vec2(1, 7/8d) == 7/u)
      assert(Vec2(1, 7/8d) == 7f/u)
      assert(Vec2(1, 7/8d) == 7d/u)

      assert(Vec2(9, 10) == 2 + u)
      assert(Vec2(9, 10) == 2f + u)
      assert(Vec2(9, 10) == 2d + u)
      assert(Vec2(-5, -6) == 2 - u)
      assert(Vec2(-5, -6) == 2f - u)
      assert(Vec2(-5, -6) == 2d - u)
    }

    {
      val u = ConstVec3(7, 8, 9)

      assert(Vec3(14, 16, 18) == 2*u)
      assert(Vec3(14, 16, 18) == 2f*u)
      assert(Vec3(14, 16, 18) == 2d*u)
      assert(Vec3(1, 7/8d, 7/9d) == 7/u)
      assert(Vec3(1, 7/8d, 7/9d) == 7f/u)
      assert(Vec3(1, 7/8d, 7/9d) == 7d/u)

      assert(Vec3(9, 10, 11) == 2 + u)
      assert(Vec3(9, 10, 11) == 2f + u)
      assert(Vec3(9, 10, 11) == 2d + u)
      assert(Vec3(-5, -6, -7) == 2 - u)
      assert(Vec3(-5, -6, -7) == 2f - u)
      assert(Vec3(-5, -6, -7) == 2d - u)
    }

    {
      val u = ConstVec4(6, 7, 8, 9)

      assert(Vec4(12, 14, 16, 18) == 2*u)
      assert(Vec4(12, 14, 16, 18) == 2f*u)
      assert(Vec4(12, 14, 16, 18) == 2d*u)
      assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7/u)
      assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7f/u)
      assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7d/u)

      assert(Vec4(8, 9, 10, 11) == 2 + u)
      assert(Vec4(8, 9, 10, 11) == 2f + u)
      assert(Vec4(8, 9, 10, 11) == 2d + u)
      assert(Vec4(-4, -5, -6, -7) == 2 - u)
      assert(Vec4(-4, -5, -6, -7) == 2f - u)
      assert(Vec4(-4, -5, -6, -7) == 2d - u)
    }

    {
      val q = ConstQuat4(6, 7, 8, 9)

      assert(Quat4(12, 14, 16, 18) == 2*q)
      assert(Quat4(12, 14, 16, 18) == 2f*q)
      assert(Quat4(12, 14, 16, 18) == 2d*q)
      assert(Quat4(7/6d, 1, 7/8d, 7/9d) == 7/q)
      assert(Quat4(7/6d, 1, 7/8d, 7/9d) == 7f/q)
      assert(Quat4(7/6d, 1, 7/8d, 7/9d) == 7d/q)

      assert(Quat4(8, 9, 10, 11) == 2 + q)
      assert(Quat4(8, 9, 10, 11) == 2f + q)
      assert(Quat4(8, 9, 10, 11) == 2d + q)
      assert(Quat4(-4, -5, -6, -7) == 2 - q)
      assert(Quat4(-4, -5, -6, -7) == 2f - q)
      assert(Quat4(-4, -5, -6, -7) == 2d - q)
    }


    val (m00, m10, m20, m30) = (1d, 2d, 3d, 4d)
    val (m01, m11, m21, m31) = (5d, 6d, 7d, 8d)
    val (m02, m12, m22, m32) = (9d, 10d, 11d, 12d)
    val (m03, m13, m23, m33) = (13d, 14d, 15d, 16d)

    {
      val m = ConstMat2x2(
        m00, m10,
        m01, m11
      )

      var t = Mat2x2(
        2*m00, 2*m10,
        2*m01, 2*m11
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat2x2(
        2/m00, 2/m10,
        2/m01, 2/m11
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat2x2(
        2+m00, 2+m10,
        2+m01, 2+m11
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat2x2(
        2-m00, 2-m10,
        2-m01, 2-m11
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat2x3(
        m00, m10,
        m01, m11,
        m02, m12
      )

      var t = Mat2x3(
        2*m00, 2*m10,
        2*m01, 2*m11,
        2*m02, 2*m12
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat2x3(
        2/m00, 2/m10,
        2/m01, 2/m11,
        2/m02, 2/m12
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat2x3(
        2+m00, 2+m10,
        2+m01, 2+m11,
        2+m02, 2+m12
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat2x3(
        2-m00, 2-m10,
        2-m01, 2-m11,
        2-m02, 2-m12
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat2x4(
        m00, m10,
        m01, m11,
        m02, m12,
        m03, m13
      )

      var t = Mat2x4(
        2*m00, 2*m10,
        2*m01, 2*m11,
        2*m02, 2*m12,
        2*m03, 2*m13
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat2x4(
        2/m00, 2/m10,
        2/m01, 2/m11,
        2/m02, 2/m12,
        2/m03, 2/m13
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat2x4(
        2+m00, 2+m10,
        2+m01, 2+m11,
        2+m02, 2+m12,
        2+m03, 2+m13
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat2x4(
        2-m00, 2-m10,
        2-m01, 2-m11,
        2-m02, 2-m12,
        2-m03, 2-m13
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat3x2(
        m00, m10, m20,
        m01, m11, m21
      )

      var t = Mat3x2(
        2*m00, 2*m10, 2*m20,
        2*m01, 2*m11, 2*m21
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat3x2(
        2/m00, 2/m10, 2/m20,
        2/m01, 2/m11, 2/m21
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat3x2(
        2+m00, 2+m10, 2+m20,
        2+m01, 2+m11, 2+m21
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat3x2(
        2-m00, 2-m10, 2-m20,
        2-m01, 2-m11, 2-m21
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat3x3(
        m00, m10, m20,
        m01, m11, m21,
        m02, m12, m22
      )

      var t = Mat3x3(
        2*m00, 2*m10, 2*m20,
        2*m01, 2*m11, 2*m21,
        2*m02, 2*m12, 2*m22
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat3x3(
        2/m00, 2/m10, 2/m20,
        2/m01, 2/m11, 2/m21,
        2/m02, 2/m12, 2/m22
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat3x3(
        2+m00, 2+m10, 2+m20,
        2+m01, 2+m11, 2+m21,
        2+m02, 2+m12, 2+m22
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat3x3(
        2-m00, 2-m10, 2-m20,
        2-m01, 2-m11, 2-m21,
        2-m02, 2-m12, 2-m22
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat3x4(
        m00, m10, m20,
        m01, m11, m21,
        m02, m12, m22,
        m03, m13, m23
      )

      var t = Mat3x4(
        2*m00, 2*m10, 2*m20,
        2*m01, 2*m11, 2*m21,
        2*m02, 2*m12, 2*m22,
        2*m03, 2*m13, 2*m23
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat3x4(
        2/m00, 2/m10, 2/m20,
        2/m01, 2/m11, 2/m21,
        2/m02, 2/m12, 2/m22,
        2/m03, 2/m13, 2/m23
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat3x4(
        2+m00, 2+m10, 2+m20,
        2+m01, 2+m11, 2+m21,
        2+m02, 2+m12, 2+m22,
        2+m03, 2+m13, 2+m23
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat3x4(
        2-m00, 2-m10, 2-m20,
        2-m01, 2-m11, 2-m21,
        2-m02, 2-m12, 2-m22,
        2-m03, 2-m13, 2-m23
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat4x2(
        m00, m10, m20, m30,
        m01, m11, m21, m31
      )

      var t = Mat4x2(
        2*m00, 2*m10, 2*m20, 2*m30,
        2*m01, 2*m11, 2*m21, 2*m31
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat4x2(
        2/m00, 2/m10, 2/m20, 2/m30,
        2/m01, 2/m11, 2/m21, 2/m31
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat4x2(
        2+m00, 2+m10, 2+m20, 2+m30,
        2+m01, 2+m11, 2+m21, 2+m31
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat4x2(
        2-m00, 2-m10, 2-m20, 2-m30,
        2-m01, 2-m11, 2-m21, 2-m31
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat4x3(
        m00, m10, m20, m30,
        m01, m11, m21, m31,
        m02, m12, m22, m32
      )

      var t = Mat4x3(
        2*m00, 2*m10, 2*m20, 2*m30,
        2*m01, 2*m11, 2*m21, 2*m31,
        2*m02, 2*m12, 2*m22, 2*m32
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat4x3(
        2/m00, 2/m10, 2/m20, 2/m30,
        2/m01, 2/m11, 2/m21, 2/m31,
        2/m02, 2/m12, 2/m22, 2/m32
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat4x3(
        2+m00, 2+m10, 2+m20, 2+m30,
        2+m01, 2+m11, 2+m21, 2+m31,
        2+m02, 2+m12, 2+m22, 2+m32
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat4x3(
        2-m00, 2-m10, 2-m20, 2-m30,
        2-m01, 2-m11, 2-m21, 2-m31,
        2-m02, 2-m12, 2-m22, 2-m32
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    {
      val m = ConstMat4x4(
        m00, m10, m20, m30,
        m01, m11, m21, m31,
        m02, m12, m22, m32,
        m03, m13, m23, m33
      )

      var t = Mat4x4(
        2*m00, 2*m10, 2*m20, 2*m30,
        2*m01, 2*m11, 2*m21, 2*m31,
        2*m02, 2*m12, 2*m22, 2*m32,
        2*m03, 2*m13, 2*m23, 2*m33
      )
      assert(2*m == t)
      assert(2f*m == t)
      assert(2d*m == t)

      t = Mat4x4(
        2/m00, 2/m10, 2/m20, 2/m30,
        2/m01, 2/m11, 2/m21, 2/m31,
        2/m02, 2/m12, 2/m22, 2/m32,
        2/m03, 2/m13, 2/m23, 2/m33
      )
      assert(2/m == t)
      assert(2f/m == t)
      assert(2d/m == t)

      t = Mat4x4(
        2+m00, 2+m10, 2+m20, 2+m30,
        2+m01, 2+m11, 2+m21, 2+m31,
        2+m02, 2+m12, 2+m22, 2+m32,
        2+m03, 2+m13, 2+m23, 2+m33
      )
      assert(2 + m == t)
      assert(2f + m == t)
      assert(2d + m == t)

      t = Mat4x4(
        2-m00, 2-m10, 2-m20, 2-m30,
        2-m01, 2-m11, 2-m21, 2-m31,
        2-m02, 2-m12, 2-m22, 2-m32,
        2-m03, 2-m13, 2-m23, 2-m33
      )
      assert(2 - m == t)
      assert(2f - m == t)
      assert(2d - m == t)
    }

    // Promotions
    {
      val u = ConstVec2i(7, 8)

      assert(Vec2(14, 16) == 2d*u)
      assert(Vec2(1, 7/8d) == 7d/u)

      assert(Vec2(9, 10) == 2d + u)
      assert(Vec2(-5, -6) == 2d - u)
    }

    {
      val u = ConstVec3i(7, 8, 9)

      assert(Vec3(14, 16, 18) == 2d*u)
      assert(Vec3(1, 7/8d, 7/9d) == 7d/u)

      assert(Vec3(9, 10, 11) == 2d + u)
      assert(Vec3(-5, -6, -7) == 2d - u)
    }

    {
      val u = ConstVec4i(6, 7, 8, 9)

      assert(Vec4(12, 14, 16, 18) == 2d*u)
      assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7d/u)

      assert(Vec4(8, 9, 10, 11) == 2d + u)
      assert(Vec4(-4, -5, -6, -7) == 2d - u)
    }

    {
      val u = ConstVec2f(7, 8)

      assert(Vec2(14, 16) == 2d*u)
      assert(Vec2(1, 7/8d) == 7d/u)

      assert(Vec2(9, 10) == 2d + u)
      assert(Vec2(-5, -6) == 2d - u)
    }

    {
      val u = ConstVec3f(7, 8, 9)

      assert(Vec3(14, 16, 18) == 2d*u)
      assert(Vec3(1, 7/8d, 7/9d) == 7d/u)

      assert(Vec3(9, 10, 11) == 2d + u)
      assert(Vec3(-5, -6, -7) == 2d - u)
    }

    {
      val u = ConstVec4f(6, 7, 8, 9)

      assert(Vec4(12, 14, 16, 18) == 2d*u)
      assert(Vec4(7/6d, 1, 7/8d, 7/9d) == 7d/u)

      assert(Vec4(8, 9, 10, 11) == 2d + u)
      assert(Vec4(-4, -5, -6, -7) == 2d - u)
    }

    {
      val q = ConstQuat4f(6, 7, 8, 9)

      assert(C*Quat4(6, 7, 8, 9) == C*q)
      assert(C/Quat4(6, 7, 8, 9) == C/q)

      assert(C + Quat4(6, 7, 8, 9) == C + q)
      assert(C - Quat4(6, 7, 8, 9) == C - q)
    }

    {
      val m = ConstMat2x2f(M)

      assert(C*Mat2x2(M) == C*m)
      assert(C/Mat2x2(M) == C/m)

      assert(C + Mat2x2(M) == C + m)
      assert(C - Mat2x2(M) == C - m)
    }

    {
      val m = ConstMat2x3f(M)

      assert(C*Mat2x3(M) == C*m)
      assert(C/Mat2x3(M) == C/m)

      assert(C + Mat2x3(M) == C + m)
      assert(C - Mat2x3(M) == C - m)
    }

    {
      val m = ConstMat2x4f(M)

      assert(C*Mat2x4(M) == C*m)
      assert(C/Mat2x4(M) == C/m)

      assert(C + Mat2x4(M) == C + m)
      assert(C - Mat2x4(M) == C - m)
    }

    {
      val m = ConstMat3x2f(M)

      assert(C*Mat3x2(M) == C*m)
      assert(C/Mat3x2(M) == C/m)

      assert(C + Mat3x2(M) == C + m)
      assert(C - Mat3x2(M) == C - m)
    }

    {
      val m = ConstMat3x3f(M)

      assert(C*Mat3x3(M) == C*m)
      assert(C/Mat3x3(M) == C/m)

      assert(C + Mat3x3(M) == C + m)
      assert(C - Mat3x3(M) == C - m)
    }

    {
      val m = ConstMat3x4f(M)

      assert(C*Mat3x4(M) == C*m)
      assert(C/Mat3x4(M) == C/m)

      assert(C + Mat3x4(M) == C + m)
      assert(C - Mat3x4(M) == C - m)
    }

    {
      val m = ConstMat4x2f(M)

      assert(C*Mat4x2(M) == C*m)
      assert(C/Mat4x2(M) == C/m)

      assert(C + Mat4x2(M) == C + m)
      assert(C - Mat4x2(M) == C - m)
    }

    {
      val m = ConstMat4x3f(M)

      assert(C*Mat4x3(M) == C*m)
      assert(C/Mat4x3(M) == C/m)

      assert(C + Mat4x3(M) == C + m)
      assert(C - Mat4x3(M) == C - m)
    }

    {
      val m = ConstMat4x4f(M)

      assert(C*Mat4x4(M) == C*m)
      assert(C/Mat4x4(M) == C/m)

      assert(C + Mat4x4(M) == C + m)
      assert(C - Mat4x4(M) == C - m)
    }
  }
}
