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
class Mat2x3dTest extends FunSuite {
  
  test("Clone") {
    var t: ReadMat2x3 = Mat2x3(5)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstMat2x3(5)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var m: ReadMat2x3 = Mat2x3(1)

    m = Mat2x3(d00)
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, 0, 0)) { (m.m00, m.m01, m.m02) }
    expectResult((0, d00, 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    )
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(
      Vec3(d00, d01, d02),
      Vec3(d10, d11, d12)
    )
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x2(
      d00, d01,
      d10, d11
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x2(
      d00, d01,
      d10, d11,
      d20, d21
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x2(
      d00, d01,
      d10, d11,
      d20, d21,
      d30, d31
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, 0)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x3(
      d00, d01, d02,
      d10, d11, d12,
      d20, d21, d22,
      d30, d31, d32
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23,
      d30, d31, d32, d33
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(
      Vec3f(f00, f01, f02),
      Vec3f(f10, f11, f12)
    )
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x2f(
      f00, f01,
      f10, f11
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x2f(
      f00, f01,
      f10, f11,
      f20, f21
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x2f(
      f00, f01,
      f10, f11,
      f20, f21,
      f30, f31
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), 0)) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), 0)) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x3f(
      f00, f01, f02,
      f10, f11, f12
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x3f(
      f00, f01, f02,
      f10, f11, f12,
      f20, f21, f22,
      f30, f31, f32
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat2x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat3x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = Mat2x3(Mat4x4f(
      f00, f01, f02, f03,
      f10, f11, f12, f13,
      f20, f21, f22, f23,
      f30, f31, f32, f33
    ))
    expectResult(classOf[Mat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }


    m = ConstMat2x3(
      d00, d01, d02,
      d10, d11, d12
    )
    expectResult(classOf[ConstMat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = ConstMat2x3(
      Vec3(d00, d01, d02),
      Vec3(d10, d11, d12)
    )
    expectResult(classOf[ConstMat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = ConstMat2x3(Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    ))
    expectResult(classOf[ConstMat2x3]) { m.getClass }
    expectResult((d00, d01, d02)) { (m.m00, m.m01, m.m02) }
    expectResult((d10, d11, d12)) { (m.m10, m.m11, m.m12) }

    m = ConstMat2x3(
      Vec3f(f00, f01, f02),
      Vec3f(f10, f11, f12)
    )
    expectResult(classOf[ConstMat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }

    m = ConstMat2x3(Mat2x3f(
      f00, f01, f02,
      f10, f11, f12
    ))
    expectResult(classOf[ConstMat2x3]) { m.getClass }
    expectResult((toDouble(f00), toDouble(f01), toDouble(f02))) { (m.m00, m.m01, m.m02) }
    expectResult((toDouble(f10), toDouble(f11), toDouble(f12))) { (m.m10, m.m11, m.m12) }
  }

  test("Unapply") {
    Mat2x3(
      d00, d01, d02,
      d10, d11, d12
    ) match {
      case Mat2x3(c1, c2) =>
        if (
          c1 != Vec3(d00, d01, d02) ||
          c2 != Vec3(d10, d11, d12)
        ) throw new AssertionError()
    }
    ConstMat2x3(
      d00, d01, d02,
      d10, d11, d12
    ) match {
      case Mat2x3(c1, c2) =>
        if (
          c1 != Vec3(d00, d01, d02) ||
          c2 != Vec3(d10, d11, d12)
        ) throw new AssertionError()
    }
  }

  test("Const conversions") {
    val i = Mat2x3(
      m00, m01, m02,
      m10, m11, m12
    )

    val t: ConstMat2x3 = i
    expectResult(classOf[ConstMat2x3]) { t.getClass }
    assert(i == t)

    var c = ConstMat2x3(2); val v = i
    expectResult(classOf[Mat2x3]) { v.getClass }
    c = v; assert(i == c)
    expectResult(classOf[ConstMat2x3]) { c.getClass }
  }

  test("Equality methods") {
    val m = Mat2x3(
      m00, m01, m02,
      m10, m11, m12
    )
    val n = ConstMat2x3(
      m00, m01, m02,
      m10, m11, m12
    )
    assert(m == m)
    assert(m == n)
    assert(n == m)
    assert(n == n)

    assert(m.equals(n))
    assert(!m.equals(Nil))

    for (r <- 0 until 3; c <- 0 until 2) {
      val t = Mat2x3(n)
      t(c, r) = -1
      assert(t != n)
    }

    assert(m == Mat2x3f(M))
    for (r <- 0 until 3; c <- 0 until 2) {
      val t = Mat2x3f(M)
      t(c, r) = -1
      assert(m != t)
    }
  }

  test("Indexed read") {
    {
      val m = ConstMat2x3(
        1, 2, 3,
        4, 5, 6
      )

      var count = 0
      for (c <- 0 until 2; r <- 0 until 3) {
        count += 1
        expectResult(count) { m(c, r) }
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

    val m = ConstMat2x3(
      m00, m01, m02,
      m10, m11, m12
    )

    expectResult(Vec3(m00, m01, m02)) { m(0) }
    expectResult(Vec3(m10, m11, m12)) { m(1) }

    expectResult(classOf[ConstVec3]) { m(0).getClass }
    expectResult(classOf[ConstVec3]) { m(1).getClass }

    intercept[IndexOutOfBoundsException] {
      m(2)
    }
    intercept[IndexOutOfBoundsException] {
      m(-1)
    }
  }

  test("Indexed write") {
    var m = Mat2x3(
      m00, m01, m02,
      m10, m11, m12
    )

    var count = 0
    for (c <- 0 until 2; r <- 0 until 3) {
      count += 1
      m(c, r) = count + 1
      expectResult(count + 1) { m(c, r) }
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

    m = Mat2x3(0)

    m(0) = Vec3(m00, m01, m02)
    m(1) = Vec3(m10, m11, m12)

    expectResult(Vec3(m00, m01, m02)) { m(0) }
    expectResult(Vec3(m10, m11, m12)) { m(1) }

    m = Mat2x3(0)

    m(0) = Vec2(m00, m01)
    m(1) = Vec2(m10, m11)

    expectResult(Vec3(m00, m01, 0)) { m(0) }
    expectResult(Vec3(m10, m11, 0)) { m(1) }

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
    var m = Mat2x3(0)
    val i = ConstMat2x3(
      m00, m01, m02,
      m10, m11, m12
    )

    m = Mat2x3(0)
    m := i
    expectResult((m00, m01, m02)) { (m.m00, m.m01, m.m02) }
    expectResult((m10, m11, m12)) { (m.m10, m.m11, m.m12) }
  }

  test("Const math") {
    val m = ConstMat2x3(
      m00, m01, m02,
      m10, m11, m12
    )
    assert(+m eq m)

    var t = Mat2x3(
      -m00, -m01, -m02,
      -m10, -m11, -m12
    )
    assert(-m == t)

    t = Mat2x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12
    )
    assert(m*2 == t)

    t = Mat2x3(
      m00/2, m01/2, m02/2,
      m10/2, m11/2, m12/2
    )
    assert(m/2 == t)

    t = Mat2x3(
      m00+2, m01+2, m02+2,
      m10+2, m11+2, m12+2
    )
    assert(m + 2 == t)

    t = Mat2x3(
      m00-2, m01-2, m02-2,
      m10-2, m11-2, m12-2
    )
    assert(m - 2 == t)

    val n: ConstMat2x3 = m*3

    t = Mat2x3(
      4*m00, 4*m01, 4*m02,
      4*m10, 4*m11, 4*m12
    )
    assert(n + m == t)

    t = Mat2x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12
    )
    assert(n - m == t)

    t = Mat2x3(
      3, 3, 3,
      3, 3, 3
    )
    assert(n / m == t)


    val mul22 = Mat2x3(
      11, 14, 17,
      35, 46, 57
    )
    assert(m*Mat2x2(M) == mul22)

    val mul32 = Mat3x3(
      11, 14, 17,
      35, 46, 57,
      59, 78, 97
    )
    assert(m*Mat3x2(M) == mul32)

    val mul42 = Mat4x3(
      11, 14, 17,
      35, 46, 57,
      59, 78, 97,
      83, 110, 137
    )
    assert(m*Mat4x2(M) == mul42)

    assert(m*Vec2(1, 2) == Vec3(11, 14, 17))
  }

  test("Mutable math") {
    val m = Mat2x3(0)
    val i = ConstMat2x3(
      m00, m01, m02,
      m10, m11, m12
    )

    var t = Mat2x3(
      2*m00, 2*m01, 2*m02,
      2*m10, 2*m11, 2*m12
    )
    m := i; m *= 2; assert(m == t)

    t = Mat2x3(
      m00/2, m01/2, m02/2,
      m10/2, m11/2, m12/2
    )
    m := i; m /= 2; assert(m == t)

    t = Mat2x3(
      m00+2, m01+2, m02+2,
      m10+2, m11+2, m12+2
    )
    m := i; m += 2; assert(m == t)

    t = Mat2x3(
      m00-2, m01-2, m02-2,
      m10-2, m11-2, m12-2
    )
    m := i; m -= 2; assert(m == t)

    val n: ConstMat2x3 = i*3

    t = Mat2x3(
      4*m00, 4*m01, 4*m02,
      4*m10, 4*m11, 4*m12
    )
    m := i; m += n; assert(m == t)

    t = Mat2x3(
      -2*m00, -2*m01, -2*m02,
      -2*m10, -2*m11, -2*m12
    )
    m := i; m -= n; assert(m == t)

    t = Mat2x3(
      11, 14, 17,
      35, 46, 57
    )
    m := i; m *= Mat2x2(M); assert(m == t)

    t = Mat2x3(
      1, 1, 1,
      1, 1, 1
    )
    m := i; m/= m; assert(m == t)
  }

}
