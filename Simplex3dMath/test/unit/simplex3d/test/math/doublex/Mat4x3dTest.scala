/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d.test.math.doublex

import org.scalatest._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.floatx._
import simplex3d.math.doublex.functions._
import MatConstants._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat4x3dTest extends FunSuite {
  
  test("Clone") {
    var t: ReadMat4x3 = Mat4x3(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat4x3(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat4x3 = Mat4x3(1)

    m = Mat4x3(d00)
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, 0, 0)) { (m.m00, m.m01, m.m02) }
    expect((0, d00, 0)) { (m.m10, m.m11, m.m12) }
    expect((0, 0, d00)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(
      Vec3(d00, d01, d02),
      Vec3(d10, d11, d12),
      Vec3(d20, d21, d22),
      Vec3(d30, d31, d32)
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x2(
      d00, d01,
      d10, d11
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, 0)) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x2(
      d00, d01,
      d10, d11,
      d20, d21
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, 0)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, 0)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, 1)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23,
      d30, d31, d32, d33
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(
      Vec3f(f00, f01, f02),
      Vec3f(f10, f11, f12),
      Vec3f(f20, f21, f22),
      Vec3f(f30, f31, f32)
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), toDouble(f32))) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x2f(
      f00, f01,
      f10, f11
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x2f(
      f00, f01,
      f10, f11,
      f20, f21
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x2f(
      f00, f01,
      f10, f11,
      f20, f21,
      f30, f31
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), 1)) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x3f(
      f00, f01, f02,
      f10, f11, f12
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22,
      f30, f31, f32
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), toDouble(f32))) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat2x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((0, 0, 1)) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat3x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((0, 0, 0)) { (m.m30, m.m31, m.m32) }

    m = Mat4x3(Mat4x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23,
      f30, f31, f32, f33
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), toDouble(f32))) { (m.m30, m.m31, m.m32) }


    m = ConstMat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = ConstMat4x3(
      Vec3(d00, d01, d02),
      Vec3(d10, d11, d12),
      Vec3(d20, d21, d22),
      Vec3(d30, d31, d32)
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = ConstMat4x3(Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ))
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expect((d10, d11, d12)) { (m.m10, m.m11, m.m12) }
    expect((d20, d21, d22)) { (m.m20, m.m21, m.m22) }
    expect((d30, d31, d32)) { (m.m30, m.m31, m.m32) }

    m = ConstMat4x3(
      Vec3f(f00, f01, f02),
      Vec3f(f10, f11, f12),
      Vec3f(f20, f21, f22),
      Vec3f(f30, f31, f32)
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), toDouble(f32))) { (m.m30, m.m31, m.m32) }

    m = ConstMat4x3(Mat4x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22,
      f30, f31, f32
    ))
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expect((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
    expect((toDouble(f20), toDouble(f21), toDouble(f22))) { (m.m20, m.m21, m.m22) }
    expect((toDouble(f30), toDouble(f31), toDouble(f32))) { (m.m30, m.m31, m.m32) }
  }

  test("Unapply") {
    Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ) match {
      case Mat4x3(c1, c2, c3, c4) =>
        if (
          c1 != Vec3(d00, d01, d02) ||
          c2 != Vec3(d10, d11, d12) ||
          c3 != Vec3(d20, d21, d22) ||
          c4 != Vec3(d30, d31, d32)
        ) throw new AssertionError()
    }
    ConstMat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ) match {
      case Mat4x3(c1, c2, c3, c4) =>
        if (
          c1 != Vec3(d00, d01, d02) ||
          c2 != Vec3(d10, d11, d12) ||
          c3 != Vec3(d20, d21, d22) ||
          c4 != Vec3(d30, d31, d32)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )

    val t: ConstMat4x3 = i
    expect(classOf[ConstMat4x3]) { t.getClass }
    assert(i == t)

    var c = ConstMat4x3(2); val v = i
    expect(classOf[Mat4x3]) { v.getClass }
    c = v; assert(i == c)
    expect(classOf[ConstMat4x3]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )
    val n = ConstMat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 3; c <- 0 until 4) {
      val t = Mat4x3(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat4x3f(M))
    for (r <- 0 until 3; c <- 0 until 4) {
      val t = Mat4x3f(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat4x3(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9,
        10, 11, 12
      )

      var count = 0
      for (c <- 0 until 4; r <- 0 until 3) {
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
        m(1, 3)
      }
      intercept[IndexOutOfBoundsException] {
        m(1, -1)
      }
    }

    val m = ConstMat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )

    expect(Vec3(m00, m01, m02)) { m(0) }
    expect(Vec3(m10, m11, m12)) { m(1) }
    expect(Vec3(m20, m21, m22)) { m(2) }
    expect(Vec3(m30, m31, m32)) { m(3) }

    expect(classOf[ConstVec3]) { m(0).getClass }
    expect(classOf[ConstVec3]) { m(1).getClass }
    expect(classOf[ConstVec3]) { m(2).getClass }
    expect(classOf[ConstVec3]) { m(3).getClass }

    intercept[IndexOutOfBoundsException] {
      m(4)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )

    var count = 0
    for (c <- 0 until 4; r <- 0 until 3) {
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
      m(1, 3) = 1
    }
    intercept[IndexOutOfBoundsException] {
      m(1, -1) = 1
    }

    m = Mat4x3(0)

    m(0) = Vec3(m00, m01, m02)
    m(1) = Vec3(m10, m11, m12)
    m(2) = Vec3(m20, m21, m22)
    m(3) = Vec3(m30, m31, m32)

    expect(Vec3(m00, m01, m02)) { m(0) }
    expect(Vec3(m10, m11, m12)) { m(1) }
    expect(Vec3(m20, m21, m22)) { m(2) }
    expect(Vec3(m30, m31, m32)) { m(3) }

    m = Mat4x3(0)

    m(0) = Vec2(m00, m01)
    m(1) = Vec2(m10, m11)
    m(2) = Vec2(m20, m21)
    m(3) = Vec2(m30, m31)

    expect(Vec3(m00, m01, 0)) { m(0) }
    expect(Vec3(m10, m11, 0)) { m(1) }
    expect(Vec3(m20, m21, 0)) { m(2) }
    expect(Vec3(m30, m31, 0)) { m(3) }

    intercept[IndexOutOfBoundsException] {
      m(4) = Vec3(1)
      m(4) = Vec2(1)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1) = Vec3(1)
      m(-1) = Vec2(1)
    }
  }

  test("Setters") {
    var m = Mat4x3(0)
    val i = ConstMat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )

    m = Mat4x3(0)
    m := i
    expect((m00, m01, m02)) { (m.m00, m.m01, m.m02) }
    expect((m10, m11, m12)) { (m.m10, m.m11, m.m12) }
    expect((m20, m21, m22)) { (m.m20, m.m21, m.m22) }
    expect((m30, m31, m32)) { (m.m30, m.m31, m.m32) }
  }

  test("Const math") {
    val m = ConstMat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )
    assert(+m eq m)

    var t = Mat4x3(
      -m00, -m01, -m02,
      -m10, -m11, -m12,
      -m20, -m21, -m22,
      -m30, -m31, -m32
    )
    assert(-m == t)

    t = Mat4x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12,
      2*m20, 2*m21, 2*m22,
      2*m30, 2*m31, 2*m32
    )
    assert(m*2 == t)

    t = Mat4x3(
      m00/2, m01/2, m02/2,
      m10/2, m11/2, m12/2,
      m20/2, m21/2, m22/2,
      m30/2, m31/2, m32/2
    )
    assert(m/2 == t)

    t = Mat4x3(
      m00+2, m01+2, m02+2,
      m10+2, m11+2, m12+2,
      m20+2, m21+2, m22+2,
      m30+2, m31+2, m32+2
    )
    assert(m + 2 == t)

    t = Mat4x3(
      m00-2, m01-2, m02-2,
      m10-2, m11-2, m12-2,
      m20-2, m21-2, m22-2,
      m30-2, m31-2, m32-2
    )
    assert(m - 2 == t)

    val n: ConstMat4x3 = m*3

    t = Mat4x3(
      4*m00, 4*m01, 4*m02,
      4*m10, 4*m11, 4*m12,
      4*m20, 4*m21, 4*m22,
      4*m30, 4*m31, 4*m32
    )
    assert(n + m == t)

    t = Mat4x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12,
      2*m20, 2*m21, 2*m22,
      2*m30, 2*m31, 2*m32
    )
    assert(n - m == t)

    t = Mat4x3(
      3, 3, 3,
      3, 3, 3,
      3, 3, 3,
      3, 3, 3
    )
    assert(n / m == t)


    val mul24 = Mat2x3(
      90, 100, 110,
      202, 228, 254
    )
    assert(m*Mat2x4(M) == mul24)

    val mul34 = Mat3x3(
      90, 100, 110,
      202, 228, 254,
      314, 356, 398
    )
    assert(m*Mat3x4(M) == mul34)

    val mul44 = Mat4x3(
      90, 100, 110,
      202, 228, 254,
      314, 356, 398,
      426, 484, 542
    )
    assert(m*M == mul44)

    assert(m*Vec4(1, 2, 3, 4) == Vec3(90, 100, 110))
  }

  test("Mutable math") {
    val m = Mat4x3(0)
    val i = ConstMat4x3(
      m00, m01, m02,
      m10, m11, m12,
      m20, m21, m22,
      m30, m31, m32
    )

    var t = Mat4x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12,
      2*m20, 2*m21, 2*m22,
      2*m30, 2*m31, 2*m32
    )
    m := i; m *= 2; assert(m == t)

    t = Mat4x3(
      m00/2, m01/2, m02/2,
      m10/2, m11/2, m12/2,
      m20/2, m21/2, m22/2,
      m30/2, m31/2, m32/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat4x3(
      m00+2, m01+2, m02+2,
      m10+2, m11+2, m12+2,
      m20+2, m21+2, m22+2,
      m30+2, m31+2, m32+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat4x3(
      m00-2, m01-2, m02-2,
      m10-2, m11-2, m12-2,
      m20-2, m21-2, m22-2,
      m30-2, m31-2, m32-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat4x3 = i*3

    t = Mat4x3(
      4*m00, 4*m01, 4*m02,
      4*m10, 4*m11, 4*m12,
      4*m20, 4*m21, 4*m22,
      4*m30, 4*m31, 4*m32
    )
    m := i; m += n; assert(m == t)

    t = Mat4x3(
      -2*m00, -2*m01, -2*m02,
      -2*m10, -2*m11, -2*m12,
      -2*m20, -2*m21, -2*m22,
      -2*m30, -2*m31, -2*m32
    )
    m := i; m -= n; assert(m == t)

    t = Mat4x3(
      90, 100, 110,
      202, 228, 254,
      314, 356, 398,
      426, 484, 542
    )
    m := i; m *= M; assert(m == t)

    t = Mat4x3(
      1, 1, 1,
      1, 1, 1,
      1, 1, 1,
      1, 1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
