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

package test.math.floatx

import org.scalatest._
import test.math.BooleanCombinations

import simplex3d.math._
import simplex3d.math.doublex._
import simplex3d.math.float._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4fTest extends FunSuite {

  test("Clone") {
    var t: ReadVec4 = Vec4(1)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstVec4(1)
    assert(t.clone() eq t)
  }

  test("Factories") {
    def test(x: Float, y: Float, z: Float, w: Float) {
      var u: ReadVec4 = Vec4(x)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(x) { u.y }
      expect(x) { u.z }
      expect(x) { u.w }

      u = ConstVec4(x)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(x) { u.y }
      expect(x) { u.z }
      expect(x) { u.w }

      u = Vec4(Mat2(x, y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Mat2d(x, y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Quat4(w, x, y, z))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Quat4d(w, x, y, z))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, y, z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4(x, y, Vec2i(toInt(z), toInt(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4(x, Vec2i(toInt(y), toInt(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2i(toInt(x), toInt(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3i(toInt(y), toInt(z), toInt(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4(Vec3i(toInt(x), toInt(y), toInt(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec4(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, y, Vec2(toFloat(z), toFloat(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec2(toFloat(y), toFloat(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2(toFloat(x), toFloat(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3(toFloat(y), toFloat(z), toFloat(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec3(toFloat(x), toFloat(y), toFloat(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, y, Vec2d(toDouble(z), toDouble(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec2d(toDouble(y), toDouble(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2d(toDouble(x), toDouble(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Mat2(x, y, z, w))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Mat2d(x, y, z, w))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Quat4(w, x, y, z))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Quat4d(w, x, y, z))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, y, z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = ConstVec4(x, y, Vec2i(toInt(z), toInt(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = ConstVec4(x, Vec2i(toInt(y), toInt(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2i(toInt(x), toInt(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3i(toInt(y), toInt(z), toInt(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = ConstVec4(Vec3i(toInt(x), toInt(y), toInt(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, y, Vec2(toFloat(z), toFloat(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec2(toFloat(y), toFloat(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2(toFloat(x), toFloat(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3(toFloat(y), toFloat(z), toFloat(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec3(toFloat(x), toFloat(y), toFloat(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, y, Vec2d(toDouble(z), toDouble(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec2d(toDouble(y), toDouble(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2d(toDouble(x), toDouble(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }
    }

    test(2, 3, 4, 5)
    val eps = 1e-5f
    test(2 + eps, 3 + eps, 4 + eps, 5 + eps)
  }

  test("Boolean factories") {
    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec4 = Vec4(Vec4b(x, y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      u = Vec4(toFloat(x), toFloat(y), Vec2b(z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      u = Vec4(toFloat(x), Vec2b(y, z), toFloat(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      u = Vec4(Vec2b(x, y), toFloat(z), toFloat(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      u = Vec4(toFloat(x), Vec3b(y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      u = Vec4(Vec3b(x, y, z), toFloat(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(toFloat(x)) { u.x }
      expect(toFloat(y)) { u.y }
      expect(toFloat(z)) { u.z }
      expect(toFloat(w)) { u.w }

      var c: ReadVec4 = ConstVec4(Vec4b(x, y, z, w))
      expect(classOf[ConstVec4]) { c.getClass }
      expect(toFloat(x)) { c.x }
      expect(toFloat(y)) { c.y }
      expect(toFloat(z)) { c.z }
      expect(toFloat(w)) { c.w }
    }
  }

  test("Unapply") {
    val x = 1+1e-5f; val y = 2+1e-5f; val z = 3+1e-5f; val w = 4+1e-5f
    Vec4(x, y, z, w) match {
      case Vec4(ux, uy, uz, uw) =>
        if (ux != x || uy != y || uz != z || uw != w)
          throw new AssertionError()
    }
    ConstVec4(x, y, z, w) match {
      case Vec4(ux, uy, uz, uw) =>
        if (ux != x || uy != y || uz != z || uw != w)
          throw new AssertionError()
    }
  }

  test("Const conversions") {
    val x = 1f + 1e-5f
    val y = 2f + 1e-5f
    val z = 3f + 1e-5f
    val w = 4f + 1e-5f

    val t: ConstVec4 = Vec4(x, y, z, w)
    expect(classOf[ConstVec4]) { t.getClass }
    assert(Vec4(x, y, z, w) == t)

    var c: ConstVec4 = Vec4(x, y, z, w); var v = Vec4(3)
    expect(classOf[ConstVec4]) { c.getClass }
    v = c; assert(Vec4(x, y, z, w) == v)
    expect(classOf[Vec4]) { v.getClass }

    c = Vec4(5); v = Vec4(x, y, z, w)
    expect(classOf[Vec4]) { v.getClass }
    c = v; assert(Vec4(x, y, z, w) == c)
    expect(classOf[ConstVec4]) { c.getClass }
  }

  test("Equality methods") {
    val m = Vec4(4, 7, 9, 1)
    val c = ConstVec4(4, 7, 9, 1)

    assert(m == m)
    assert(m == c)
    assert(c == m)
    assert(c == c)

    assert(m.equals(c))
    assert(!m.equals(Nil))

    assert(Vec4(1, 2, 3, 4) != Vec4(9, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4(1, 9, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4(1, 2, 9, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4(1, 2, 3, 9))

    assert(Vec4(0) != Vec4b(false))

    assert(Vec4(1, 2, 3, 4) == Vec4i(1, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4i(9, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4i(1, 9, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4i(1, 2, 9, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4i(1, 2, 3, 9))

    assert(Vec4(1, 2, 3, 4) == Vec4d(1, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4d(9, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4d(1, 9, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4d(1, 2, 9, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4d(1, 2, 3, 9))
  }

  test("Indexed read") {
    val u = ConstVec4(3, 4, 5, 6)

    expect(3) { u(0) }
    expect(4) { u(1) }
    expect(5) { u(2) }
    expect(6) { u(3) }

    intercept[IndexOutOfBoundsException] {
      u(4)
    }
    intercept[IndexOutOfBoundsException] {
      u(-1)
    }
  }

  test("Indexed write") {
    val u = Vec4(3, 4, 5, 6)

    u(0) = 5
    assert(Vec4(5, 4, 5, 6) == u)

    u(1) = 6
    assert(Vec4(5, 6, 5, 6) == u)

    u(2) = 7
    assert(Vec4(5, 6, 7, 6) == u)

    u(3) = 8
    assert(Vec4(5, 6, 7, 8) == u)

    intercept[IndexOutOfBoundsException] {
      u(4) = 1
    }
    intercept[IndexOutOfBoundsException] {
      u(-1) = 1
    }
  }

  test("Setters") {
    val u = Vec4(0)

    u := Vec4(1, 2, 3, 4)
    expect(1) { u.x }
    expect(2) { u.y }
    expect(3) { u.z }
    expect(4) { u.w }
  }

  test("Const math") {
    val u = ConstVec4(6, 7, 8, 9)

    assert(+u eq u)

    assert(Vec4(-6, -7, -8, -9) == -u)

    assert(Vec4(12, 14, 16, 18) == u*2)
    assert(Vec4(3, 3.5f, 4, 4.5f) == u/2)

    assert(Vec4(8, 9, 10, 11) == u + 2)
    assert(Vec4(4, 5, 6, 7) == u - 2)

    val v = ConstVec4(2, 3, 4, 5)

    assert(Vec4(8, 10, 12, 14) == u + v)
    assert(Vec4(4) == u - v)
    assert(Vec4(12, 21, 32, 45) == u*v)
    assert(Vec4(3, 7/3f, 2, 9/5f) == u/v)

    val m4x2 = ConstMat4x2(
      2, 5, 4, 6,
      3, 4, 8, 2
    )
    assert(Vec2(65, 60) == v*m4x2)

    val m4x3 = ConstMat4x3(
      2, 5, 4, 6,
      3, 4, 8, 2,
      7, 4, 2, 5
    )
    assert(Vec3(65, 60, 59) == v*m4x3)

    val m4 = ConstMat4(
      2, 5, 4, 6,
      3, 4, 8, 2,
      7, 4, 2, 5,
      5, 9, 2, 3
    )
    assert(Vec4(65, 60, 59, 60) == v*m4)
  }

  test("Mutable math") {
    val u = Vec4(0)
    val i = ConstVec4(2, 3, 4, 5)

    u := i; u *= 2; assert(Vec4(4, 6, 8, 10) == u)
    u := i; u /= 2; assert(Vec4(1, 1.5f, 2, 2.5f) == u)

    u := i; u += Vec4(3, 4, 5, 6); assert(Vec4(5, 7, 9, 11) == u)
    u := i; u += u; assert(Vec4(4, 6, 8, 10) == u)
    u := i; u -= Vec4(2, 3, 4, 5); assert(Vec4(0) == u)
    u := i; u -= u; assert(Vec4(0) == u)

    u := i; u *= Vec4(5, 10, 15, 2); assert(Vec4(10, 30, 60, 10) == u)
    u := i; u *= u; assert(Vec4(4, 9, 16, 25) == u)
    u := i; u /= Vec4(2, 6, 2, 2); assert(Vec4(1, 0.5f, 2, 2.5f) == u)
    u := i; u /= u; assert(Vec4(1) == u)

    u := i
    val m4 = ConstMat4(
      2, 5, 4, 6,
      3, 4, 8, 2,
      7, 4, 2, 5,
      5, 9, 2, 3
    )
    u *= m4
    assert(Vec4(65, 60, 59, 60) == u)
  }

}
