/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

import simplex3d.math._
import simplex3d.math.doublem.renamed._
import simplex3d.math.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat4x3dTest extends FunSuite {
  val (m00, m10, m20, m30) = (1d, 2d, 3d, 4d)
  val (m01, m11, m21, m31) = (5d, 6d, 7d, 8d)
  val (m02, m12, m22, m32) = (9d, 10d, 11d, 12d)
  val (m03, m13, m23, m33) = (13d, 14d, 15d, 16d)

  val (f00, f10, f20, f30) = (1f+1e-5f, 2f+1e-5f, 3f+1e-5f, 4f+1e-5f)
  val (f01, f11, f21, f31) = (5f+1e-5f, 6f+1e-5f, 7f+1e-5f, 8f+1e-5f)
  val (f02, f12, f22, f32) = (9f+1e-5f, 10f+1e-5f, 11f+1e-5f, 12f+1e-5f)
  val (f03, f13, f23, f33) = (13f+1e-5f, 14f+1e-5f, 15f+1e-5f, 16f+1e-5f)

  val (d00, d10, d20, d30) = (1+1e-14, 2+1e-14, 3+1e-14, 4+1e-14)
  val (d01, d11, d21, d31) = (5+1e-14, 6+1e-14, 7+1e-14, 8+1e-14)
  val (d02, d12, d22, d32) = (9+1e-14, 10+1e-14, 11+1e-14, 12+1e-14)
  val (d03, d13, d23, d33) = (13+1e-14, 14+1e-14, 15+1e-14, 16+1e-14)

  val M = Mat4(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33
  )

  test("Factories") {
    var m: ReadMat4x3 = Mat4x3(1)

    m = Mat4x3(d00)
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, 0, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((0, d00, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, d00, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(
      Vec4(d00, d10, d20, d30),
      Vec4(d01, d11, d21, d31),
      Vec4(d02, d12, d22, d32)
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x2(
      d00, d10,
      d01, d11
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x3(
      d00, d10,
      d01, d11,
      d02, d12
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x2(
      d00, d10, d20,
      d01, d11, d21
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x3(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x4(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22,
      d03, d13, d23
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x2(
      d00, d10, d20, d30,
      d01, d11, d21, d31
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x4(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32,
      d03, d13, d23, d33
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(
      Vec4f(f00, f10, f20, f30),
      Vec4f(f01, f11, f21, f31),
      Vec4f(f02, f12, f22, f32)
    )
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), double(f32))) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x2f(
      f00, f10,
      f01, f11
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x3f(
      f00, f10,
      f01, f11,
      f02, f12
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat2x4f(
      f00, f10,
      f01, f11,
      f02, f12,
      f03, f13
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x2f(
      f00, f10, f20,
      f01, f11, f21
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x3f(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat3x4f(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22,
      f03, f13, f23
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x2f(
      f00, f10, f20, f30,
      f01, f11, f21, f31
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x3f(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), double(f32))) { (m.m02, m.m12, m.m22, m.m32) }

    m = Mat4x3(Mat4x4f(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ))
    expect(classOf[Mat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), double(f32))) { (m.m02, m.m12, m.m22, m.m32) }


    m = ConstMat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = ConstMat4x3(
      Vec4(d00, d10, d20, d30),
      Vec4(d01, d11, d21, d31),
      Vec4(d02, d12, d22, d32)
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = ConstMat4x3(Mat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ))
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((d00, d10, d20, d30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((d01, d11, d21, d31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((d02, d12, d22, d32)) { (m.m02, m.m12, m.m22, m.m32) }

    m = ConstMat4x3(
      Vec4f(f00, f10, f20, f30),
      Vec4f(f01, f11, f21, f31),
      Vec4f(f02, f12, f22, f32)
    )
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), double(f32))) { (m.m02, m.m12, m.m22, m.m32) }

    m = ConstMat4x3(Mat4x3f(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32
    ))
    expect(classOf[ConstMat4x3]) { m.getClass }
    expect((double(f00), double(f10), double(f20), double(f30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((double(f01), double(f11), double(f21), double(f31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((double(f02), double(f12), double(f22), double(f32))) { (m.m02, m.m12, m.m22, m.m32) }
  }

  test("Unapply") {
    Mat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ) match {
      case Mat4x3(c1, c2, c3) =>
        if (
          c1 != Vec4(d00, d10, d20, d30) ||
          c2 != Vec4(d01, d11, d21, d31) ||
          c3 != Vec4(d02, d12, d22, d32)
        ) throw new AssertionError()
    }
    ConstMat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ) match {
      case Mat4x3(c1, c2, c3) =>
        if (
          c1 != Vec4(d00, d10, d20, d30) ||
          c2 != Vec4(d01, d11, d21, d31) ||
          c3 != Vec4(d02, d12, d22, d32)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )

    val t: ConstMat4x3 = i
    expect(classOf[ConstMat4x3]) { t.getClass }
    assert(i == t)

    var c: ConstMat4x3 = i; var v = Mat4x3(2)
    expect(classOf[ConstMat4x3]) { c.getClass }
    v = c; assert(i == v)
    expect(classOf[Mat4x3]) { v.getClass }

    c = Mat4x3(2); v = i
    expect(classOf[Mat4x3]) { v.getClass }
    c = v; assert(i == c)
    expect(classOf[ConstMat4x3]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )
    val n = ConstMat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 4; c <- 0 until 3) {
      val t = Mat4x3(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat4x3f(M))
    for (r <- 0 until 4; c <- 0 until 3) {
      val t = Mat4x3f(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    val m = ConstMat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )

    var count = 0
    for (c <- 0 until 3; r <- 0 until 4) {
      count += 1
      expect(count) { m(c, r) }
    }

    intercept[IndexOutOfBoundsException] {
      m(3, 1)
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

    expect(classOf[ConstVec4]) { m(0).getClass }
    expect(classOf[ConstVec4]) { m(1).getClass }
    expect(classOf[ConstVec4]) { m(2).getClass }

    intercept[IndexOutOfBoundsException] {
      m(3)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )

    var count = 0
    for (c <- 0 until 3; r <- 0 until 4) {
      count += 1
      m(c, r) = count + 1
      expect(count + 1) { m(c, r) }
    }

    intercept[IndexOutOfBoundsException] {
      m(3, 1) = 1
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

    m = Mat4x3(0)

    m(0) = Vec4(m00, m10, m20, m30)
    m(1) = Vec4(m01, m11, m21, m31)
    m(2) = Vec4(m02, m12, m22, m32)

    expect(Vec4(m00, m10, m20, m30)) { m(0) }
    expect(Vec4(m01, m11, m21, m31)) { m(1) }
    expect(Vec4(m02, m12, m22, m32)) { m(2) }

    m = Mat4x3(0)

    m(0) = Vec3(m00, m10, m20)
    m(1) = Vec3(m01, m11, m21)
    m(2) = Vec3(m02, m12, m22)

    expect(Vec4(m00, m10, m20, 0)) { m(0) }
    expect(Vec4(m01, m11, m21, 0)) { m(1) }
    expect(Vec4(m02, m12, m22, 0)) { m(2) }

    m = Mat4x3(0)

    m(0) = Vec2(m00, m10)
    m(1) = Vec2(m01, m11)
    m(2) = Vec2(m02, m12)

    expect(Vec4(m00, m10, 0, 0)) { m(0) }
    expect(Vec4(m01, m11, 0, 0)) { m(1) }
    expect(Vec4(m02, m12, 0, 0)) { m(2) }

    intercept[IndexOutOfBoundsException] {
      m(3) = Vec4(1)
      m(3) = Vec3(1)
      m(3) = Vec2(1)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1) = Vec4(1)
      m(-1) = Vec3(1)
      m(-1) = Vec2(1)
    }
  }

  test("Setters") {
    var m = Mat4x3(0)
    val i = ConstMat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )

    m = Mat4x3(0)
    m := i
    expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
  }

  test("Const math") {
    val m = ConstMat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )
    assert(+m eq m)

    var t = Mat4x3(
      -m00, -m10, -m20, -m30,
      -m01, -m11, -m21, -m31,
      -m02, -m12, -m22, -m32
    )
    assert(-m == t)

    t = Mat4x3(
      2*m00, 2*m10, 2*m20, 2*m30,
      2*m01, 2*m11, 2*m21, 2*m31,
      2*m02, 2*m12, 2*m22, 2*m32
    )
    assert(m*2 == t)

    t = Mat4x3(
      m00/2, m10/2, m20/2, m30/2,
      m01/2, m11/2, m21/2, m31/2,
      m02/2, m12/2, m22/2, m32/2
    )
    assert(m/2 == t)

    t = Mat4x3(
      m00+2, m10+2, m20+2, m30+2,
      m01+2, m11+2, m21+2, m31+2,
      m02+2, m12+2, m22+2, m32+2
    )
    assert(m + 2 == t)

    t = Mat4x3(
      m00-2, m10-2, m20-2, m30-2,
      m01-2, m11-2, m21-2, m31-2,
      m02-2, m12-2, m22-2, m32-2
    )
    assert(m - 2 == t)

    val n: ConstMat4x3 = m*3

    t = Mat4x3(
      4*m00, 4*m10, 4*m20, 4*m30,
      4*m01, 4*m11, 4*m21, 4*m31,
      4*m02, 4*m12, 4*m22, 4*m32
    )
    assert(n + m == t)

    t = Mat4x3(
      2*m00, 2*m10, 2*m20, 2*m30,
      2*m01, 2*m11, 2*m21, 2*m31,
      2*m02, 2*m12, 2*m22, 2*m32
    )
    assert(n - m == t)

    t = Mat4x3(
      3, 3, 3, 3,
      3, 3, 3, 3,
      3, 3, 3, 3
    )
    assert(n / m == t)


    val mul32 = Mat4x2(
      38, 44, 50, 56,
      98, 116, 134, 152
    )
    assert(m*Mat3x2(M) == mul32)

    val mul33 = Mat4x3(
      38, 44, 50, 56,
      98, 116, 134, 152,
      158, 188, 218, 248
    )
    assert(m*Mat3x3(M) == mul33)

    val mul34 = Mat4x4(
      38, 44, 50, 56,
      98, 116, 134, 152,
      158, 188, 218, 248,
      218, 260, 302, 344
    )
    assert(m*Mat3x4(M) == mul34)

    assert(m*Vec3(1, 2, 3) == Vec4(38, 44, 50, 56))
  }

  test("Mutable math") {
    val m = Mat4x3(0)
    val i = ConstMat4x3(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32
    )

    var t = Mat4x3(
      2*m00, 2*m10, 2*m20, 2*m30,
      2*m01, 2*m11, 2*m21, 2*m31,
      2*m02, 2*m12, 2*m22, 2*m32
    )
    m := i; m *= 2; assert(m == t)

    t = Mat4x3(
      m00/2, m10/2, m20/2, m30/2,
      m01/2, m11/2, m21/2, m31/2,
      m02/2, m12/2, m22/2, m32/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat4x3(
      m00+2, m10+2, m20+2, m30+2,
      m01+2, m11+2, m21+2, m31+2,
      m02+2, m12+2, m22+2, m32+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat4x3(
      m00-2, m10-2, m20-2, m30-2,
      m01-2, m11-2, m21-2, m31-2,
      m02-2, m12-2, m22-2, m32-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat4x3 = i*3

    t = Mat4x3(
      4*m00, 4*m10, 4*m20, 4*m30,
      4*m01, 4*m11, 4*m21, 4*m31,
      4*m02, 4*m12, 4*m22, 4*m32
    )
    m := i; m += n; assert(m == t)

    t = Mat4x3(
      -2*m00, -2*m10, -2*m20, -2*m30,
      -2*m01, -2*m11, -2*m21, -2*m31,
      -2*m02, -2*m12, -2*m22, -2*m32
    )
    m := i; m -= n; assert(m == t)

    t = Mat4x3(
      38, 44, 50, 56,
      98, 116, 134, 152,
      158, 188, 218, 248
    )
    m := i; m *= Mat3(M); assert(m == t)

    t = Mat4x3(
      1, 1, 1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
