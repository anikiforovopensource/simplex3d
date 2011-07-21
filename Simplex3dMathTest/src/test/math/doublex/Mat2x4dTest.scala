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

package test.math.doublex

import org.scalatest._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.floatx._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat2x4dTest extends FunSuite {
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

  test("Clone") {
    var t: ReadMat2x4 = Mat2x4(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat2x4(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat2x4 = Mat2x4(1)

    m = Mat2x4(d00)
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, 0)) { (m.m00, m.m10) }
    expect((0, d00)) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    )
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = Mat2x4(
      Vec2(d00, d10),
      Vec2(d01, d11),
      Vec2(d02, d12),
      Vec2(d03, d13)
    )
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x2(
      d00, d10,
      d01, d11
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x3(
      d00, d10,
      d01, d11,
      d02, d12
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x2(
      d00, d10, d20,
      d01, d11, d21
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x3(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x4(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22,
      d03, d13, d23
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x2(
      d00, d10, d20, d30,
      d01, d11, d21, d31
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x3(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x4(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32,
      d03, d13, d23, d33
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = Mat2x4(
      Vec2f(f00, f10),
      Vec2f(f01, f11),
      Vec2f(f02, f12),
      Vec2f(f03, f13)
    )
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x2f(
      f00, f10,
      f01, f11
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x3f(
      f00, f10,
      f01, f11,
      f02, f12
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat2x4f(
      f00, f10,
      f01, f11,
      f02, f12,
      f03, f13
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x2f(
      f00, f10, f20,
      f01, f11, f21
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x3f(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat3x4f(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22,
      f03, f13, f23
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x2f(
      f00, f10, f20, f30,
      f01, f11, f21, f31
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((0, 0)) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x3f(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((0, 0)) { (m.m03, m.m13) }

    m = Mat2x4(Mat4x4f(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ))
    expect(classOf[Mat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }


    m = ConstMat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    )
    expect(classOf[ConstMat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = ConstMat2x4(
      Vec2(d00, d10),
      Vec2(d01, d11),
      Vec2(d02, d12),
      Vec2(d03, d13)
    )
    expect(classOf[ConstMat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = ConstMat2x4(Mat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ))
    expect(classOf[ConstMat2x4]) { m.getClass }
    expect((d00, d10)) { (m.m00, m.m10) }
    expect((d01, d11)) { (m.m01, m.m11) }
    expect((d02, d12)) { (m.m02, m.m12) }
    expect((d03, d13)) { (m.m03, m.m13) }

    m = ConstMat2x4(
      Vec2f(f00, f10),
      Vec2f(f01, f11),
      Vec2f(f02, f12),
      Vec2f(f03, f13)
    )
    expect(classOf[ConstMat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }

    m = ConstMat2x4(Mat2x4f(
      f00, f10,
      f01, f11,
      f02, f12,
      f03, f13
    ))
    expect(classOf[ConstMat2x4]) { m.getClass }
    expect((Double(f00), Double(f10))) { (m.m00, m.m10) }
    expect((Double(f01), Double(f11))) { (m.m01, m.m11) }
    expect((Double(f02), Double(f12))) { (m.m02, m.m12) }
    expect((Double(f03), Double(f13))) { (m.m03, m.m13) }
  }

  test("Unapply") {
    Mat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ) match {
      case Mat2x4(c1, c2, c3, c4) =>
        if (
          c1 != Vec2(d00, d10) ||
          c2 != Vec2(d01, d11) ||
          c3 != Vec2(d02, d12) ||
          c4 != Vec2(d03, d13)
        ) throw new AssertionError()
    }
    ConstMat2x4(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ) match {
      case Mat2x4(c1, c2, c3, c4) =>
        if (
          c1 != Vec2(d00, d10) ||
          c2 != Vec2(d01, d11) ||
          c3 != Vec2(d02, d12) ||
          c4 != Vec2(d03, d13)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )

    val t: ConstMat2x4 = i
    expect(classOf[ConstMat2x4]) { t.getClass }
    assert(i == t)

    var c = ConstMat2x4(2); val v = i
    expect(classOf[Mat2x4]) { v.getClass }
    c = v; assert(i == c)
    expect(classOf[ConstMat2x4]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )
    val n = ConstMat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )
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

    assert(m == Mat2x4f(M))
    for (r <- 0 until 2; c <- 0 until 4) {
      val t = Mat2x4f(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat2x4(
        1, 2,
        3, 4,
        5, 6,
        7, 8
      )

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

    val m = ConstMat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )

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
    var m = Mat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )

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
    val i = ConstMat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )

    m = Mat2x4(0)
    m := i
    expect((m00, m10)) { (m.m00, m.m10) }
    expect((m01, m11)) { (m.m01, m.m11) }
    expect((m02, m12)) { (m.m02, m.m12) }
    expect((m03, m13)) { (m.m03, m.m13) }
  }

  test("Const math") {
    val m = ConstMat2x4(
      m00, m10,
      m01, m11,
      m02, m12,
      m03, m13
    )
    assert(+m eq m)

    var t = Mat2x4(
      -m00, -m10,
      -m01, -m11,
      -m02, -m12,
      -m03, -m13
    )
    assert(-m == t)

    t = Mat2x4(
      2*m00, 2*m10,
      2*m01, 2*m11,
      2*m02, 2*m12,
      2*m03, 2*m13
    )
    assert(m*2 == t)

    t = Mat2x4(
      m00/2, m10/2,
      m01/2, m11/2,
      m02/2, m12/2,
      m03/2, m13/2
    )
    assert(m/2 == t)

    t = Mat2x4(
      m00+2, m10+2,
      m01+2, m11+2,
      m02+2, m12+2,
      m03+2, m13+2
    )
    assert(m + 2 == t)

    t = Mat2x4(
      m00-2, m10-2,
      m01-2, m11-2,
      m02-2, m12-2,
      m03-2, m13-2
    )
    assert(m - 2 == t)

    val n: ConstMat2x4 = m*3

    t = Mat2x4(
      4*m00, 4*m10,
      4*m01, 4*m11,
      4*m02, 4*m12,
      4*m03, 4*m13
    )
    assert(n + m == t)

    t = Mat2x4(
      2*m00, 2*m10,
      2*m01, 2*m11,
      2*m02, 2*m12,
      2*m03, 2*m13
    )
    assert(n - m == t)

    t = Mat2x4(
      3, 3,
      3, 3,
      3, 3,
      3, 3
    )
    assert(n / m == t)


    val mul42 = Mat2x2(
      90, 100,
      202, 228
    )
    assert(m*Mat4x2(M) == mul42)

    val mul43 = Mat2x3(
      90, 100,
      202, 228,
      314, 356
    )
    assert(m*Mat4x3(M) == mul43)

    val mul44 = Mat2x4(
      90, 100,
      202, 228,
      314, 356,
      426, 484
    )
    assert(m*M == mul44)

    assert(m*Vec4(1, 2, 3, 4) == Vec2(90, 100))
  }

  test("Mutable math") {
    val m = Mat2x4(0)
    val i = ConstMat2x4(
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
    m := i; m *= 2; assert(m == t)

    t = Mat2x4(
      m00/2, m10/2,
      m01/2, m11/2,
      m02/2, m12/2,
      m03/2, m13/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat2x4(
      m00+2, m10+2,
      m01+2, m11+2,
      m02+2, m12+2,
      m03+2, m13+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat2x4(
      m00-2, m10-2,
      m01-2, m11-2,
      m02-2, m12-2,
      m03-2, m13-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat2x4 = i*3

    t = Mat2x4(
      4*m00, 4*m10,
      4*m01, 4*m11,
      4*m02, 4*m12,
      4*m03, 4*m13
    )
    m := i; m += n; assert(m == t)

    t = Mat2x4(
      -2*m00, -2*m10,
      -2*m01, -2*m11,
      -2*m02, -2*m12,
      -2*m03, -2*m13
    )
    m := i; m -= n; assert(m == t)

    t = Mat2x4(
      90, 100,
      202, 228,
      314, 356,
      426, 484
    )
    m := i; m *= M; assert(m == t)

    t = Mat2x4(
      1, 1,
      1, 1,
      1, 1,
      1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
