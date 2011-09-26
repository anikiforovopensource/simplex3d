/*
 * Simplex3d, MathTest package
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

package test.math.floatx

import org.scalatest._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.doublex._
import simplex3d.math.floatx.functions._


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
  val (f03, f13, f23, f33) = (13f+1e-5f, 14f+1e-5f, 15f+1e-5f, 16f+1e-5f)

  val (d00, d10, d20, d30) = (1+1e-5, 2+1e-5, 3+1e-5, 4+1e-5)
  val (d01, d11, d21, d31) = (5+1e-5, 6+1e-5, 7+1e-5, 8+1e-5)
  val (d02, d12, d22, d32) = (9+1e-5, 10+1e-5, 11+1e-5, 12+1e-5)
  val (d03, d13, d23, d33) = (13+1e-5, 14+1e-5, 15+1e-5, 16+1e-5)

  val M = Mat4(
    m00, m10, m20, m30,
    m01, m11, m21, m31,
    m02, m12, m22, m32,
    m03, m13, m23, m33
  )

  test("Clone") {
    var t: ReadMat4x4 = Mat4x4(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat4x4(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat4x4 = Mat4x4(1)

    m = Mat4x4(f00)
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, 0, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((0, f00, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, f00, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, f00)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    )
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(
      Vec4(f00, f10, f20, f30),
      Vec4(f01, f11, f21, f31),
      Vec4(f02, f12, f22, f32),
      Vec4(f03, f13, f23, f33)
    )
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x2(
      f00, f10,
      f01, f11
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x3(
      f00, f10,
      f01, f11,
      f02, f12
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x4(
      f00, f10,
      f01, f11,
      f02, f12,
      f03, f13
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x2(
      f00, f10, f20,
      f01, f11, f21
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x3(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x4(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22,
      f03, f13, f23
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x2(
      f00, f10, f20, f30,
      f01, f11, f21, f31
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x3(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(
      Vec4d(d00, d10, d20, d30),
      Vec4d(d01, d11, d21, d31),
      Vec4d(d02, d12, d22, d32),
      Vec4d(d03, d13, d23, d33)
    )
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), toFloat(d32))) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), toFloat(d23), toFloat(d33))) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x2d(
      d00, d10,
      d01, d11
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x3d(
      d00, d10,
      d01, d11,
      d02, d12
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat2x4d(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0, 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), 0, 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x2d(
      d00, d10, d20,
      d01, d11, d21
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x3d(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat3x4d(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22,
      d03, d13, d23
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), 0)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), 0)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), toFloat(d23), 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x2d(
      d00, d10, d20, d30,
      d01, d11, d21, d31
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((0, 0, 1, 0)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x3d(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), toFloat(d32))) { (m.m02, m.m12, m.m22, m.m32) }
    expect((0, 0, 0, 1)) { (m.m03, m.m13, m.m23, m.m33) }

    m = Mat4x4(Mat4x4d(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32,
      d03, d13, d23, d33
    ))
    expect(classOf[Mat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), toFloat(d32))) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), toFloat(d23), toFloat(d33))) { (m.m03, m.m13, m.m23, m.m33) }


    m = ConstMat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    )
    expect(classOf[ConstMat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = ConstMat4x4(
      Vec4(f00, f10, f20, f30),
      Vec4(f01, f11, f21, f31),
      Vec4(f02, f12, f22, f32),
      Vec4(f03, f13, f23, f33)
    )
    expect(classOf[ConstMat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = ConstMat4x4(Mat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ))
    expect(classOf[ConstMat4x4]) { m.getClass }
    expect((f00, f10, f20, f30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((f01, f11, f21, f31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((f02, f12, f22, f32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((f03, f13, f23, f33)) { (m.m03, m.m13, m.m23, m.m33) }

    m = ConstMat4x4(
      Vec4d(d00, d10, d20, d30),
      Vec4d(d01, d11, d21, d31),
      Vec4d(d02, d12, d22, d32),
      Vec4d(d03, d13, d23, d33)
    )
    expect(classOf[ConstMat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), toFloat(d32))) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), toFloat(d23), toFloat(d33))) { (m.m03, m.m13, m.m23, m.m33) }

    m = ConstMat4x4(Mat4x4d(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32,
      d03, d13, d23, d33
    ))
    expect(classOf[ConstMat4x4]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20), toFloat(d30))) { (m.m00, m.m10, m.m20, m.m30) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21), toFloat(d31))) { (m.m01, m.m11, m.m21, m.m31) }
    expect((toFloat(d02), toFloat(d12), toFloat(d22), toFloat(d32))) { (m.m02, m.m12, m.m22, m.m32) }
    expect((toFloat(d03), toFloat(d13), toFloat(d23), toFloat(d33))) { (m.m03, m.m13, m.m23, m.m33) }
  }

  test("Unapply") {
    Mat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ) match {
      case Mat4x4(c1, c2, c3, c4) =>
        if (
          c1 != Vec4(f00, f10, f20, f30) ||
          c2 != Vec4(f01, f11, f21, f31) ||
          c3 != Vec4(f02, f12, f22, f32) ||
          c4 != Vec4(f03, f13, f23, f33)
        ) throw new AssertionError()
    }
    ConstMat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ) match {
      case Mat4x4(c1, c2, c3, c4) =>
        if (
          c1 != Vec4(f00, f10, f20, f30) ||
          c2 != Vec4(f01, f11, f21, f31) ||
          c3 != Vec4(f02, f12, f22, f32) ||
          c4 != Vec4(f03, f13, f23, f33)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )

    val t: ConstMat4x4 = i
    expect(classOf[ConstMat4]) { t.getClass }
    assert(i == t)

    var c = ConstMat4x4(2); val v = i
    expect(classOf[Mat4]) { v.getClass }
    c = v; assert(i == c)
    expect(classOf[ConstMat4]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )
    val n = ConstMat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )
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

    assert(m == Mat4x4d(M))
    for (r <- 0 until 4; c <- 0 until 4) {
      val t = Mat4x4d(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    val m = ConstMat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )

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
    var m = Mat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )

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
    val i = ConstMat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )

    m = Mat4x4(0)
    m := i
    expect((m00, m10, m20, m30)) { (m.m00, m.m10, m.m20, m.m30) }
    expect((m01, m11, m21, m31)) { (m.m01, m.m11, m.m21, m.m31) }
    expect((m02, m12, m22, m32)) { (m.m02, m.m12, m.m22, m.m32) }
    expect((m03, m13, m23, m33)) { (m.m03, m.m13, m.m23, m.m33) }
  }

  test("Const math") {
    val m = ConstMat4x4(
      m00, m10, m20, m30,
      m01, m11, m21, m31,
      m02, m12, m22, m32,
      m03, m13, m23, m33
    )
    assert(+m eq m)

    var t = Mat4x4(
      -m00, -m10, -m20, -m30,
      -m01, -m11, -m21, -m31,
      -m02, -m12, -m22, -m32,
      -m03, -m13, -m23, -m33
    )
    assert(-m == t)

    t = Mat4x4(
      2*m00, 2*m10, 2*m20, 2*m30,
      2*m01, 2*m11, 2*m21, 2*m31,
      2*m02, 2*m12, 2*m22, 2*m32,
      2*m03, 2*m13, 2*m23, 2*m33
    )
    assert(m*2 == t)

    t = Mat4x4(
      m00/2, m10/2, m20/2, m30/2,
      m01/2, m11/2, m21/2, m31/2,
      m02/2, m12/2, m22/2, m32/2,
      m03/2, m13/2, m23/2, m33/2
    )
    assert(m/2 == t)

    t = Mat4x4(
      m00+2, m10+2, m20+2, m30+2,
      m01+2, m11+2, m21+2, m31+2,
      m02+2, m12+2, m22+2, m32+2,
      m03+2, m13+2, m23+2, m33+2
    )
    assert(m + 2 == t)

    t = Mat4x4(
      m00-2, m10-2, m20-2, m30-2,
      m01-2, m11-2, m21-2, m31-2,
      m02-2, m12-2, m22-2, m32-2,
      m03-2, m13-2, m23-2, m33-2
    )
    assert(m - 2 == t)

    val n: ConstMat4x4 = m*3

    t = Mat4x4(
      4*m00, 4*m10, 4*m20, 4*m30,
      4*m01, 4*m11, 4*m21, 4*m31,
      4*m02, 4*m12, 4*m22, 4*m32,
      4*m03, 4*m13, 4*m23, 4*m33
    )
    assert(n + m == t)

    t = Mat4x4(
      2*m00, 2*m10, 2*m20, 2*m30,
      2*m01, 2*m11, 2*m21, 2*m31,
      2*m02, 2*m12, 2*m22, 2*m32,
      2*m03, 2*m13, 2*m23, 2*m33
    )
    assert(n - m == t)

    t = Mat4x4(
      3, 3, 3, 3,
      3, 3, 3, 3,
      3, 3, 3, 3,
      3, 3, 3, 3
    )
    assert(n / m == t)

    
    val mul42 = Mat4x2(
      90, 100, 110, 120,
      202, 228, 254, 280
    )
    assert(m*Mat4x2(M) == mul42)

    val mul43 = Mat4x3(
      90, 100, 110, 120,
      202, 228, 254, 280,
      314, 356, 398, 440
    )
    assert(m*Mat4x3(M) == mul43)

    val mul44 = Mat4x4(
      90, 100, 110, 120,
      202, 228, 254, 280,
      314, 356, 398, 440,
      426, 484, 542, 600
    )
    assert(m*M == mul44)

    assert(m*Vec4(1, 2, 3, 4) == Vec4(90, 100, 110, 120))
  }

  test("Mutable math") {
    val m = Mat4x4(0)
    val i = ConstMat4x4(
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
    m := i; m *= 2; assert(m == t)

    t = Mat4x4(
      m00/2, m10/2, m20/2, m30/2,
      m01/2, m11/2, m21/2, m31/2,
      m02/2, m12/2, m22/2, m32/2,
      m03/2, m13/2, m23/2, m33/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat4x4(
      m00+2, m10+2, m20+2, m30+2,
      m01+2, m11+2, m21+2, m31+2,
      m02+2, m12+2, m22+2, m32+2,
      m03+2, m13+2, m23+2, m33+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat4x4(
      m00-2, m10-2, m20-2, m30-2,
      m01-2, m11-2, m21-2, m31-2,
      m02-2, m12-2, m22-2, m32-2,
      m03-2, m13-2, m23-2, m33-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat4x4 = i*3

    t = Mat4x4(
      4*m00, 4*m10, 4*m20, 4*m30,
      4*m01, 4*m11, 4*m21, 4*m31,
      4*m02, 4*m12, 4*m22, 4*m32,
      4*m03, 4*m13, 4*m23, 4*m33
    )
    m := i; m += n; assert(m == t)

    t = Mat4x4(
      -2*m00, -2*m10, -2*m20, -2*m30,
      -2*m01, -2*m11, -2*m21, -2*m31,
      -2*m02, -2*m12, -2*m22, -2*m32,
      -2*m03, -2*m13, -2*m23, -2*m33
    )
    m := i; m -= n; assert(m == t)

    t = Mat4x4(
      90, 100, 110, 120,
      202, 228, 254, 280,
      314, 356, 398, 440,
      426, 484, 542, 600
    )
    m := i; m *= m; assert(m == t)

    t = Mat4x4(
      1, 1, 1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1,
      1, 1, 1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
