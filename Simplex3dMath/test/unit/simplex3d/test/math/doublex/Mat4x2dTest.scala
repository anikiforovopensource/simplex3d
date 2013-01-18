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
class Mat4x2dTest extends FunSuite {
  
  val M = Mat4(
    m00, m01, m02, m03,
    m10, m11, m12, m13,
    m20, m21, m22, m23,
    m30, m31, m32, m33
  )

  test("Clone") {
    var t: ReadMat4x2 = Mat4x2(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat4x2(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat4x2 = Mat4x2(1)

    m = Mat4x2(d00)
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, 0)) { (m.m00, m.m01) }
    expectResult((0, d00)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    )
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = Mat4x2(
      Vec2(d00, d01),
      Vec2(d10, d11),
      Vec2(d20, d21),
      Vec2(d30, d31)
    )
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x2(
      d00, d01,
      d10, d11
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x2(
      d00, d01,
      d10, d11,
      d20, d21
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23,
      d30, d31, d32, d33
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = Mat4x2(
      Vec2f(f00, f01),
      Vec2f(f10, f11),
      Vec2f(f20, f21),
      Vec2f(f30, f31)
    )
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x2f(
      f00, f01,
      f10, f11
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x2f(
      f00, f01,
      f10, f11,
      f20, f21
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x2f(
      f00, f01,
      f10, f11,
      f20, f21,
      f30, f31
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x3f(
      f00, f01, f02,
      f10, f11, f12
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22,
      f30, f31, f32
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }

    m = Mat4x2(Mat2x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat3x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((0, 0)) { (m.m30, m.m31) }

    m = Mat4x2(Mat4x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23,
      f30, f31, f32, f33
    ))
    expectResult(classOf[Mat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }


    m = ConstMat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    )
    expectResult(classOf[ConstMat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = ConstMat4x2(
      Vec2(d00, d01),
      Vec2(d10, d11),
      Vec2(d20, d21),
      Vec2(d30, d31)
    )
    expectResult(classOf[ConstMat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = ConstMat4x2(Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ))
    expectResult(classOf[ConstMat4x2]) { m.getClass }
    expectResult((d00, d01)) { (m.m00, m.m01) }
    expectResult((d10, d11)) { (m.m10, m.m11) }
    expectResult((d20, d21)) { (m.m20, m.m21) }
    expectResult((d30, d31)) { (m.m30, m.m31) }

    m = ConstMat4x2(
      Vec2f(f00, f01),
      Vec2f(f10, f11),
      Vec2f(f20, f21),
      Vec2f(f30, f31)
    )
    expectResult(classOf[ConstMat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }

    m = ConstMat4x2(Mat4x2f(
      f00, f01,
      f10, f11,
      f20, f21,
      f30, f31
    ))
    expectResult(classOf[ConstMat4x2]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01))) { (m.m00, m.m01) }
    expectResult((toDouble(f10), toDouble(f11))) { (m.m10, m.m11) }
    expectResult((toDouble(f20), toDouble(f21))) { (m.m20, m.m21) }
    expectResult((toDouble(f30), toDouble(f31))) { (m.m30, m.m31) }
  }

  test("Unapply") {
    Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ) match {
      case Mat4x2(c1, c2, c3, c4) =>
        if (
          c1 != Vec2(d00, d01) ||
          c2 != Vec2(d10, d11) ||
          c3 != Vec2(d20, d21) ||
          c4 != Vec2(d30, d31)
        ) throw new AssertionError()
    }
    ConstMat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ) match {
      case Mat4x2(c1, c2, c3, c4) =>
        if (
          c1 != Vec2(d00, d01) ||
          c2 != Vec2(d10, d11) ||
          c3 != Vec2(d20, d21) ||
          c4 != Vec2(d30, d31)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )

    val t: ConstMat4x2 = i
    expectResult(classOf[ConstMat4x2]) { t.getClass }
    assert(i == t)

    var c = ConstMat4x2(2); val v = i
    expectResult(classOf[Mat4x2]) { v.getClass }
    c = v; assert(i == c)
    expectResult(classOf[ConstMat4x2]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )
    val n = ConstMat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 2; c <- 0 until 4) {
      val t = Mat4x2(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat4x2f(M))
    for (r <- 0 until 2; c <- 0 until 4) {
      val t = Mat4x2f(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat4x2(
        1, 2,
        3, 4,
        5, 6,
        7, 8
      )

      var count = 0
      for (c <- 0 until 4; r <- 0 until 2) {
        count += 1
        expectResult(count) { m(c, r) }
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

    val m = ConstMat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )

    expectResult(Vec2(m00, m01)) { m(0) }
    expectResult(Vec2(m10, m11)) { m(1) }
    expectResult(Vec2(m20, m21)) { m(2) }
    expectResult(Vec2(m30, m31)) { m(3) }

    expectResult(classOf[ConstVec2]) { m(0).getClass }
    expectResult(classOf[ConstVec2]) { m(1).getClass }
    expectResult(classOf[ConstVec2]) { m(2).getClass }
    expectResult(classOf[ConstVec2]) { m(3).getClass }

    intercept[IndexOutOfBoundsException] {
      m(4)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )

    var count = 0
    for (c <- 0 until 4; r <- 0 until 2) {
      count += 1
      m(c, r) = count + 1
      expectResult(count + 1) { m(c, r) }
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

    m = Mat4x2(0)

    m(0) = Vec2(m00, m01)
    m(1) = Vec2(m10, m11)
    m(2) = Vec2(m20, m21)
    m(3) = Vec2(m30, m31)

    expectResult(Vec2(m00, m01)) { m(0) }
    expectResult(Vec2(m10, m11)) { m(1) }
    expectResult(Vec2(m20, m21)) { m(2) }
    expectResult(Vec2(m30, m31)) { m(3) }

    intercept[IndexOutOfBoundsException] {
      m(4) = Vec2(1)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1) = Vec2(1)
    }
  }

  test("Setters") {
    var m = Mat4x2(0)
    val i = ConstMat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )

    m = Mat4x2(0)
    m := i
    expectResult((m00, m01)) { (m.m00, m.m01) }
    expectResult((m10, m11)) { (m.m10, m.m11) }
    expectResult((m20, m21)) { (m.m20, m.m21) }
    expectResult((m30, m31)) { (m.m30, m.m31) }
  }

  test("Const math") {
    val m = ConstMat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )
    assert(+m eq m)

    var t = Mat4x2(
      -m00, -m01,
      -m10, -m11,
      -m20, -m21,
      -m30, -m31
    )
    assert(-m == t)

    t = Mat4x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21,
      2*m30, 2*m31
    )
    assert(m*2 == t)

    t = Mat4x2(
      m00/2, m01/2,
      m10/2, m11/2,
      m20/2, m21/2,
      m30/2, m31/2
    )
    assert(m/2 == t)

    t = Mat4x2(
      m00+2, m01+2,
      m10+2, m11+2,
      m20+2, m21+2,
      m30+2, m31+2
    )
    assert(m + 2 == t)

    t = Mat4x2(
      m00-2, m01-2,
      m10-2, m11-2,
      m20-2, m21-2,
      m30-2, m31-2
    )
    assert(m - 2 == t)

    val n: ConstMat4x2 = m*3

    t = Mat4x2(
      4*m00, 4*m01,
      4*m10, 4*m11,
      4*m20, 4*m21,
      4*m30, 4*m31
    )
    assert(n + m == t)

    t = Mat4x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21,
      2*m30, 2*m31
    )
    assert(n - m == t)

    t = Mat4x2(
      3, 3,
      3, 3,
      3, 3,
      3, 3
    )
    assert(n / m == t)


    val mul24 = Mat2x2(
      90, 100,
      202, 228
    )
    assert(m*Mat2x4(M) == mul24)

    val mul34 = Mat3x2(
      90, 100,
      202, 228,
      314, 356
    )
    assert(m*Mat3x4(M) == mul34)

    val mul44 = Mat4x2(
      90, 100,
      202, 228,
      314, 356,
      426, 484
    )
    assert(m*M == mul44)

    assert(m*Vec4(1, 2, 3, 4) == Vec2(90, 100))
  }

  test("Mutable math") {
    val m = Mat4x2(0)
    val i = ConstMat4x2(
      m00, m01,
      m10, m11,
      m20, m21,
      m30, m31
    )

    var t = Mat4x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21,
      2*m30, 2*m31
    )
    m := i; m *= 2; assert(m == t)

    t = Mat4x2(
      m00/2, m01/2,
      m10/2, m11/2,
      m20/2, m21/2,
      m30/2, m31/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat4x2(
      m00+2, m01+2,
      m10+2, m11+2,
      m20+2, m21+2,
      m30+2, m31+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat4x2(
      m00-2, m01-2,
      m10-2, m11-2,
      m20-2, m21-2,
      m30-2, m31-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat4x2 = i*3

    t = Mat4x2(
      4*m00, 4*m01,
      4*m10, 4*m11,
      4*m20, 4*m21,
      4*m30, 4*m31
    )
    m := i; m += n; assert(m == t)

    t = Mat4x2(
      -2*m00, -2*m01,
      -2*m10, -2*m11,
      -2*m20, -2*m21,
      -2*m30, -2*m31
    )
    m := i; m -= n; assert(m == t)

    t = Mat4x2(
      90, 100,
      202, 228,
      314, 356,
      426, 484
    )
    m := i; m *= M; assert(m == t)

    t = Mat4x2(
      1, 1,
      1, 1,
      1, 1,
      1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
