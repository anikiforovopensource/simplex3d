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

package test.math.floatx

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
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, 0, 0)) { (m.m00, m.m10, m.m20) }
    expect((0, f00, 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(
      f00, f10, f20,
      f01, f11, f21
    )
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(
      Vec3(f00, f10, f20),
      Vec3(f01, f11, f21)
    )
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x2(
      f00, f10,
      f01, f11
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, 0)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x3(
      f00, f10,
      f01, f11,
      f02, f12
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, 0)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x4(
      f00, f10,
      f01, f11,
      f02, f12,
      f03, f13
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, 0)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x2(
      f00, f10, f20,
      f01, f11, f21
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x3(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x4(
      f00, f10, f20,
      f01, f11, f21,
      f02, f12, f22,
      f03, f13, f23
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x2(
      f00, f10, f20, f30,
      f01, f11, f21, f31
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x3(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x4(
      f00, f10, f20, f30,
      f01, f11, f21, f31,
      f02, f12, f22, f32,
      f03, f13, f23, f33
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(
      Vec3d(d00, d10, d20),
      Vec3d(d01, d11, d21)
    )
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x2d(
      d00, d10,
      d01, d11
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0)) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x3d(
      d00, d10,
      d01, d11,
      d02, d12
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0)) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat2x4d(
      d00, d10,
      d01, d11,
      d02, d12,
      d03, d13
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), 0)) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), 0)) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x2d(
      d00, d10, d20,
      d01, d11, d21
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x3d(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat3x4d(
      d00, d10, d20,
      d01, d11, d21,
      d02, d12, d22,
      d03, d13, d23
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x2d(
      d00, d10, d20, d30,
      d01, d11, d21, d31
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x3d(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = Mat3x2(Mat4x4d(
      d00, d10, d20, d30,
      d01, d11, d21, d31,
      d02, d12, d22, d32,
      d03, d13, d23, d33
    ))
    expect(classOf[Mat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }


    m = ConstMat3x2(
      f00, f10, f20,
      f01, f11, f21
    )
    expect(classOf[ConstMat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = ConstMat3x2(
      Vec3(f00, f10, f20),
      Vec3(f01, f11, f21)
    )
    expect(classOf[ConstMat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = ConstMat3x2(Mat3x2(
      f00, f10, f20,
      f01, f11, f21
    ))
    expect(classOf[ConstMat3x2]) { m.getClass }
    expect((f00, f10, f20)) { (m.m00, m.m10, m.m20) }
    expect((f01, f11, f21)) { (m.m01, m.m11, m.m21) }

    m = ConstMat3x2(
      Vec3d(d00, d10, d20),
      Vec3d(d01, d11, d21)
    )
    expect(classOf[ConstMat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }

    m = ConstMat3x2(Mat3x2d(
      d00, d10, d20,
      d01, d11, d21
    ))
    expect(classOf[ConstMat3x2]) { m.getClass }
    expect((toFloat(d00), toFloat(d10), toFloat(d20))) { (m.m00, m.m10, m.m20) }
    expect((toFloat(d01), toFloat(d11), toFloat(d21))) { (m.m01, m.m11, m.m21) }
  }

  test("Unapply") {
    Mat3x2(
      f00, f10, f20,
      f01, f11, f21
    ) match {
      case Mat3x2(c1, c2) =>
        if (
          c1 != Vec3(f00, f10, f20) ||
          c2 != Vec3(f01, f11, f21)
        ) throw new AssertionError()
    }
    ConstMat3x2(
      f00, f10, f20,
      f01, f11, f21
    ) match {
      case Mat3x2(c1, c2) =>
        if (
          c1 != Vec3(f00, f10, f20) ||
          c2 != Vec3(f01, f11, f21)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat3x2(
      m00, m10, m20,
      m01, m11, m21
    )

    val t: ConstMat3x2 = i
    expect(classOf[ConstMat3x2]) { t.getClass }
    assert(i == t)

    var c = ConstMat3x2(2); val v = i
    expect(classOf[Mat3x2]) { v.getClass }
    c = v; assert(i == c)
    expect(classOf[ConstMat3x2]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat3x2(
      m00, m10, m20,
      m01, m11, m21
    )
    val n = ConstMat3x2(
      m00, m10, m20,
      m01, m11, m21
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 3; c <- 0 until 2) {
      val t = Mat3x2(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat3x2d(M))
    for (r <- 0 until 3; c <- 0 until 2) {
      val t = Mat3x2d(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat3x2(
        1, 2, 3,
        4, 5, 6
      )

      var count = 0
      for (c <- 0 until 2; r <- 0 until 3) {
        count += 1
        expect(count) { m(c, r) }
      }

      intercept[IndexOutOfBoundsException] {
        m(2, 1)
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

    val m = ConstMat3x2(
      m00, m10, m20,
      m01, m11, m21
    )

    expect(Vec3(m00, m10, m20)) { m(0) }
    expect(Vec3(m01, m11, m21)) { m(1) }

    expect(classOf[ConstVec3]) { m(0).getClass }
    expect(classOf[ConstVec3]) { m(1).getClass }

    intercept[IndexOutOfBoundsException] {
      m(2)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat3x2(
      m00, m10, m20,
      m01, m11, m21
    )

    var count = 0
    for (c <- 0 until 2; r <- 0 until 3) {
      count += 1
      m(c, r) = count + 1
      expect(count + 1) { m(c, r) }
    }

    intercept[IndexOutOfBoundsException] {
      m(2, 1) = 1
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

    m = Mat3x2(0)

    m(0) = Vec3(m00, m10, m20)
    m(1) = Vec3(m01, m11, m21)

    expect(Vec3(m00, m10, m20)) { m(0) }
    expect(Vec3(m01, m11, m21)) { m(1) }

    m = Mat3x2(0)

    m(0) = Vec2(m00, m10)
    m(1) = Vec2(m01, m11)

    expect(Vec3(m00, m10, 0)) { m(0) }
    expect(Vec3(m01, m11, 0)) { m(1) }

    intercept[IndexOutOfBoundsException] {
      m(2) = Vec3(1)
      m(2) = Vec2(1)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1) = Vec3(1)
      m(-1) = Vec2(1)
    }
  }

  test("Setters") {
    var m = Mat3x2(0)
    val i = ConstMat3x2(
      m00, m10, m20,
      m01, m11, m21
    )

    m = Mat3x2(0)
    m := i
    expect((m00, m10, m20)) { (m.m00, m.m10, m.m20) }
    expect((m01, m11, m21)) { (m.m01, m.m11, m.m21) }
  }

  test("Const math") {
    val m = ConstMat3x2(
      m00, m10, m20,
      m01, m11, m21
    )
    assert(+m eq m)

    var t = Mat3x2(
      -m00, -m10, -m20,
      -m01, -m11, -m21
    )
    assert(-m == t)

    t = Mat3x2(
      2*m00, 2*m10, 2*m20,
      2*m01, 2*m11, 2*m21
    )
    assert(m*2 == t)

    t = Mat3x2(
      m00/2, m10/2, m20/2,
      m01/2, m11/2, m21/2
    )
    assert(m/2 == t)

    t = Mat3x2(
      m00+2, m10+2, m20+2,
      m01+2, m11+2, m21+2
    )
    assert(m + 2 == t)

    t = Mat3x2(
      m00-2, m10-2, m20-2,
      m01-2, m11-2, m21-2
    )
    assert(m - 2 == t)

    val n: ConstMat3x2 = m*3

    t = Mat3x2(
      4*m00, 4*m10, 4*m20,
      4*m01, 4*m11, 4*m21
    )
    assert(n + m == t)

    t = Mat3x2(
      2*m00, 2*m10, 2*m20,
      2*m01, 2*m11, 2*m21
    )
    assert(n - m == t)

    t = Mat3x2(
      3, 3, 3,
      3, 3, 3
    )
    assert(n / m == t)

    
    val mul22 = Mat3x2(
      11, 14, 17,
      35, 46, 57
    )
    assert(m*Mat2x2(M) == mul22)

    val mul23 = Mat3x3(
      11, 14, 17,
      35, 46, 57,
      59, 78, 97
    )
    assert(m*Mat2x3(M) == mul23)

    val mul24 = Mat3x4(
      11, 14, 17,
      35, 46, 57,
      59, 78, 97,
      83, 110, 137
    )
    assert(m*Mat2x4(M) == mul24)

    assert(m*Vec2(1, 2) == Vec3(11, 14, 17))
  }

  test("Mutable math") {
    val m = Mat3x2(0)
    val i = ConstMat3x2(
      m00, m10, m20,
      m01, m11, m21
    )

    var t = Mat3x2(
      2*m00, 2*m10, 2*m20,
      2*m01, 2*m11, 2*m21
    )
    m := i; m *= 2; assert(m == t)

    t = Mat3x2(
      m00/2, m10/2, m20/2,
      m01/2, m11/2, m21/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat3x2(
      m00+2, m10+2, m20+2,
      m01+2, m11+2, m21+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat3x2(
      m00-2, m10-2, m20-2,
      m01-2, m11-2, m21-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat3x2 = i*3

    t = Mat3x2(
      4*m00, 4*m10, 4*m20,
      4*m01, 4*m11, 4*m21
    )
    m := i; m += n; assert(m == t)

    t = Mat3x2(
      -2*m00, -2*m10, -2*m20,
      -2*m01, -2*m11, -2*m21
    )
    m := i; m -= n; assert(m == t)

    t = Mat3x2(
      11, 14, 17,
      35, 46, 57
    )
    m := i; m *= Mat2x2(M); assert(m == t)

    t = Mat3x2(
      1, 1, 1,
      1, 1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
