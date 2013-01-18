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

package simplex3d.test.math.floatx

import org.scalatest._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.doublex._
import simplex3d.math.floatx.functions._
import MatConstants._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Mat3x2fTest extends FunSuite {
  
  test("Clone") {
    var t: ReadMat3x2 = Mat3x2(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat3x2(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat3x2 = Mat3x2(1)

    m = Mat3x2(f00)
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, 0)) { (m.m00, m.m01) }
    expectResult((0, f00)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    )
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(
      Vec2(f00, f01),
      Vec2(f10, f11),
      Vec2(f20, f21)
    )
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x2(
      f00, f01,
      f10, f11
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x2(
      f00, f01,
      f10, f11,
      f20, f21,
      f30, f31
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x3(
      f00, f01, f02,
      f10, f11, f12
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x3(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x3(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22,
      f30, f31, f32
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x4(
      f00, f01, f02, f03,
      f10, f11, f12, f13
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x4(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x4(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23,
      f30, f31, f32, f33
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = Mat3x2(
      Vec2d(d00, d01),
      Vec2d(d10, d11),
      Vec2d(d20, d21)
    )
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x2d(
      d00, d01,
      d10, d11
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x2d(
      d00, d01,
      d10, d11,
      d20, d21
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x2d(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x3d(
      d00, d01, d02,
      d10, d11, d12
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x3d(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x3d(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat2x4d(
      d00, d01, d02, d03,
      d10, d11, d12, d13
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }

    m = Mat3x2(Mat3x4d(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = Mat3x2(Mat4x4d(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23,
      d30, d31, d32, d33
    ))
    expectResult(classOf[Mat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }


    m = ConstMat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    )
    expectResult(classOf[ConstMat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = ConstMat3x2(
      Vec2(f00, f01),
      Vec2(f10, f11),
      Vec2(f20, f21)
    )
    expectResult(classOf[ConstMat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = ConstMat3x2(Mat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    ))
    expectResult(classOf[ConstMat3x2]) { m.getClass }
    expectResult((f00, f01)) { (m.m00, m.m01) }
    expectResult((f10, f11)) { (m.m10, m.m11) }
    expectResult((f20, f21)) { (m.m20, m.m21) }

    m = ConstMat3x2(
      Vec2d(d00, d01),
      Vec2d(d10, d11),
      Vec2d(d20, d21)
    )
    expectResult(classOf[ConstMat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }

    m = ConstMat3x2(Mat3x2d(
      d00, d01,
      d10, d11,
      d20, d21
    ))
    expectResult(classOf[ConstMat3x2]) { m.getClass }
    expectResult((toFloat(d00), toFloat(d01))) { (m.m00, m.m01) }
    expectResult((toFloat(d10), toFloat(d11))) { (m.m10, m.m11) }
    expectResult((toFloat(d20), toFloat(d21))) { (m.m20, m.m21) }
  }

  test("Unapply") {
    Mat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    ) match {
      case Mat3x2(c1, c2, c3) =>
        if (
          c1 != Vec2(f00, f01) ||
          c2 != Vec2(f10, f11) ||
          c3 != Vec2(f20, f21)
        ) throw new AssertionError()
    }
    ConstMat3x2(
      f00, f01,
      f10, f11,
      f20, f21
    ) match {
      case Mat3x2(c1, c2, c3) =>
        if (
          c1 != Vec2(f00, f01) ||
          c2 != Vec2(f10, f11) ||
          c3 != Vec2(f20, f21)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )

    val t: ConstMat3x2 = i
    expectResult(classOf[ConstMat3x2]) { t.getClass }
    assert(i == t)

    var c = ConstMat3x2(2); val v = i
    expectResult(classOf[Mat3x2]) { v.getClass }
    c = v; assert(i == c)
    expectResult(classOf[ConstMat3x2]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )
    val n = ConstMat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 2; c <- 0 until 3) {
      val t = Mat3x2(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat3x2d(M))
    for (r <- 0 until 2; c <- 0 until 3) {
      val t = Mat3x2d(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat3x2(
        1, 2,
        3, 4,
        5, 6
      )

      var count = 0
      for (c <- 0 until 3; r <- 0 until 2) {
        count += 1
        expectResult(count) { m(c, r) }
      }

      intercept[IndexOutOfBoundsException] {
        m(3, 1)
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

    val m = ConstMat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )

    expectResult(Vec2(m00, m01)) { m(0) }
    expectResult(Vec2(m10, m11)) { m(1) }
    expectResult(Vec2(m20, m21)) { m(2) }

    expectResult(classOf[ConstVec2]) { m(0).getClass }
    expectResult(classOf[ConstVec2]) { m(1).getClass }
    expectResult(classOf[ConstVec2]) { m(2).getClass }

    intercept[IndexOutOfBoundsException] {
      m(3)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )

    var count = 0
    for (c <- 0 until 3; r <- 0 until 2) {
      count += 1
      m(c, r) = count + 1
      expectResult(count + 1) { m(c, r) }
    }

    intercept[IndexOutOfBoundsException] {
      m(3, 1) = 1
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

    m = Mat3x2(0)

    m(0) = Vec2(m00, m01)
    m(1) = Vec2(m10, m11)
    m(2) = Vec2(m20, m21)

    expectResult(Vec2(m00, m01)) { m(0) }
    expectResult(Vec2(m10, m11)) { m(1) }
    expectResult(Vec2(m20, m21)) { m(2) }

    intercept[IndexOutOfBoundsException] {
      m(3) = Vec2(1)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1) = Vec2(1)
    }
  }

  test("Setters") {
    var m = Mat3x2(0)
    val i = ConstMat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )

    m = Mat3x2(0)
    m := i
    expectResult((m00, m01)) { (m.m00, m.m01) }
    expectResult((m10, m11)) { (m.m10, m.m11) }
    expectResult((m20, m21)) { (m.m20, m.m21) }
    
    m = Mat3x2(0)
    m := Mat2x2(i)
    expectResult((m00, m01)) { (m.m00, m.m01) }
    expectResult((m10, m11)) { (m.m10, m.m11) }
    expectResult((0, 0)) { (m.m20, m.m21) }
  }

  test("Const math") {
    val m = ConstMat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )
    assert(+m eq m)

    var t = Mat3x2(
      -m00, -m01,
      -m10, -m11,
      -m20, -m21
    )
    assert(-m == t)

    t = Mat3x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21
    )
    assert(m*2 == t)

    t = Mat3x2(
      m00/2, m01/2,
      m10/2, m11/2,
      m20/2, m21/2
    )
    assert(m/2 == t)

    t = Mat3x2(
      m00+2, m01+2,
      m10+2, m11+2,
      m20+2, m21+2
    )
    assert(m + 2 == t)

    t = Mat3x2(
      m00-2, m01-2,
      m10-2, m11-2,
      m20-2, m21-2
    )
    assert(m - 2 == t)

    val n: ConstMat3x2 = m*3

    t = Mat3x2(
      4*m00, 4*m01,
      4*m10, 4*m11,
      4*m20, 4*m21
    )
    assert(n + m == t)

    t = Mat3x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21
    )
    assert(n - m == t)

    t = Mat3x2(
      3, 3,
      3, 3,
      3, 3
    )
    assert(n / m == t)

    
    val mul23 = Mat2x2(
      38, 44,
      98, 116
    )
    assert(m*Mat2x3(M) == mul23)

    val mul33 = Mat3x2(
      38, 44,
      98, 116,
      158, 188
    )
    assert(m*Mat3x3(M) == mul33)

    val mul43 = Mat4x2(
      38, 44,
      98, 116,
      158, 188,
      218, 260
    )
    assert(m*Mat4x3(M) == mul43)

    assert(m*Vec3(1, 2, 3) == Vec2(38, 44))
  }

  test("Mutable math") {
    val m = Mat3x2(0)
    val i = ConstMat3x2(
      m00, m01,
      m10, m11,
      m20, m21
    )

    var t = Mat3x2(
      2*m00, 2*m01,
      2*m10, 2*m11,
      2*m20, 2*m21
    )
    m := i; m *= 2; assert(m == t)

    t = Mat3x2(
      m00/2, m01/2,
      m10/2, m11/2,
      m20/2, m21/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat3x2(
      m00+2, m01+2,
      m10+2, m11+2,
      m20+2, m21+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat3x2(
      m00-2, m01-2,
      m10-2, m11-2,
      m20-2, m21-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat3x2 = i*3

    t = Mat3x2(
      4*m00, 4*m01,
      4*m10, 4*m11,
      4*m20, 4*m21
    )
    m := i; m += n; assert(m == t)

    t = Mat3x2(
      -2*m00, -2*m01,
      -2*m10, -2*m11,
      -2*m20, -2*m21
    )
    m := i; m -= n; assert(m == t)

    t = Mat3x2(
      38, 44,
      98, 116,
      158, 188
    )
    m := i; m *= Mat3x3(M); assert(m == t)

    t = Mat3x2(
      1, 1,
      1, 1,
      1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
