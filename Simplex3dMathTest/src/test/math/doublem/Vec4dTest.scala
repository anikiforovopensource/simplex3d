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
import test.math.BooleanCombinations

import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4dTest extends FunSuite {

  test("Factories") {
    def test(x: Double, y: Double, z: Double, w: Double) {
      var u: AnyVec4 = Vec4(x)
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

      u = Vec4(Mat2f(float(x), float(y), float(z), float(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = Vec4(Quat4f(float(w), float(x), float(y), float(z)))
      expect(classOf[Vec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = Vec4(Quat4(w, x, y, z))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Mat2(x, y, z, w))
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

      u = Vec4(Vec4i(int(x), int(y), int(z), int(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = Vec4(x, y, Vec2i(int(z), int(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = Vec4(x, Vec2i(int(y), int(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2i(int(x), int(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3i(int(y), int(z), int(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = Vec4(Vec3i(int(x), int(y), int(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec4f(float(x), float(y), float(z), float(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = Vec4(x, y, Vec2f(float(z), float(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = Vec4(x, Vec2f(float(y), float(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2f(float(x), float(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3f(float(y), float(z), float(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = Vec4(Vec3f(float(x), float(y), float(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec4(double(x), double(y), double(z), double(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, y, Vec2(double(z), double(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec2(double(y), double(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec2(double(x), double(y)), z, w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(x, Vec3(double(y), double(z), double(w)))
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4(Vec3(double(x), double(y), double(z)), w)
      expect(classOf[Vec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Mat2f(float(x), float(y), float(z), float(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = ConstVec4(Quat4f(float(w), float(x), float(y), float(z)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = ConstVec4(Quat4(w, x, y, z))
      expect(classOf[ConstVec4]) { u.getClass }
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

      u = ConstVec4(x, y, z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4i(int(x), int(y), int(z), int(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = ConstVec4(x, y, Vec2i(int(z), int(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = ConstVec4(x, Vec2i(int(y), int(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2i(int(x), int(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3i(int(y), int(z), int(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(int(w)) { u.w }

      u = ConstVec4(Vec3i(int(x), int(y), int(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(int(x)) { u.x }
      expect(int(y)) { u.y }
      expect(int(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4f(float(x), float(y), float(z), float(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = ConstVec4(x, y, Vec2f(float(z), float(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = ConstVec4(x, Vec2f(float(y), float(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2f(float(x), float(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3f(float(y), float(z), float(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(float(w)) { u.w }

      u = ConstVec4(Vec3f(float(x), float(y), float(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(float(x)) { u.x }
      expect(float(y)) { u.y }
      expect(float(z)) { u.z }
      expect(w) { u.w }

      u = ConstVec4(ConstVec4(double(x), double(y), double(z), double(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, y, Vec2(double(z), double(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec2(double(y), double(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec2(double(x), double(y)), z, w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(x, Vec3(double(y), double(z), double(w)))
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = ConstVec4(Vec3(double(x), double(y), double(z)), w)
      expect(classOf[ConstVec4]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }
    }

    test(2, 3, 4, 5)
    val eps = 1e-15
    test(2 + eps, 3 + eps, 4 + eps, 5 + eps)
  }

  test("Boolean factories") {
    BooleanCombinations.test { (x, y, z, w) =>
      var u: AnyVec4 = Vec4(Vec4b(x, y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      u = Vec4(double(x), double(y), Vec2b(z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      u = Vec4(double(x), Vec2b(y, z), double(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      u = Vec4(Vec2b(x, y), double(z), double(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      u = Vec4(double(x), Vec3b(y, z, w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      u = Vec4(Vec3b(x, y, z), double(w))
      expect(classOf[Vec4]) { u.getClass }
      expect(double(x)) { u.x }
      expect(double(y)) { u.y }
      expect(double(z)) { u.z }
      expect(double(w)) { u.w }

      var c: AnyVec4 = ConstVec4(Vec4b(x, y, z, w))
      expect(classOf[ConstVec4]) { c.getClass }
      expect(double(x)) { c.x }
      expect(double(y)) { c.y }
      expect(double(z)) { c.z }
      expect(double(w)) { c.w }
    }
  }

  test("Unapply") {
    val x = 1+1e-15; val y = 2+1e-15; val z = 3+1e-15; val w = 4+1e-15
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
    val x = 1d + 1e-15
    val y = 2d + 1e-15
    val z = 3d + 1e-15
    val w = 4d + 1e-15

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
    val m = Vec3(4, 7, 9)
    val c = ConstVec3(4, 7, 9)

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

    assert(Vec4(1, 2, 3, 4) == Vec4f(1, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4f(9, 2, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4f(1, 9, 3, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4f(1, 2, 9, 4))
    assert(Vec4(1, 2, 3, 4) != Vec4f(1, 2, 3, 9))
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

    u.set(5, 6, 7, 8)
    expect(5) { u.x }
    expect(6) { u.y }
    expect(7) { u.z }
    expect(8) { u.w }
  }

  test("Const math") {
    val u = ConstVec4(6, 7, 8, 9)

    assert(+u eq u)

    assert(Vec4(-6, -7, -8, -9) == -u)

    assert(Vec4(12, 14, 16, 18) == u*2)
    assert(Vec4(3, 3.5, 4, 4.5) == u/2)

    assert(Vec4(8, 9, 10, 11) == u + 2)
    assert(Vec4(4, 5, 6, 7) == u - 2)

    val v = ConstVec4(2, 3, 4, 5)

    assert(Vec4(8, 10, 12, 14) == u + v)
    assert(Vec4(4) == u - v)
    assert(Vec4(12, 21, 32, 45) == u*v)
    assert(Vec4(3, 7/3d, 2, 9/5d) == u/v)

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
    u := i; u /= 2; assert(Vec4(1, 1.5, 2, 2.5) == u)

    u := i; u += 2; assert(Vec4(4, 5, 6, 7) == u)
    u := i; u -= 2; assert(Vec4(0, 1, 2, 3) == u)

    u := i; u += Vec4(3, 4, 5, 6); assert(Vec4(5, 7, 9, 11) == u)
    u := i; u += u; assert(Vec4(4, 6, 8, 10) == u)
    u := i; u -= Vec4(2, 3, 4, 5); assert(Vec4(0) == u)
    u := i; u -= u; assert(Vec4(0) == u)

    u := i; u *= Vec4(5, 10, 15, 2); assert(Vec4(10, 30, 60, 10) == u)
    u := i; u *= u; assert(Vec4(4, 9, 16, 25) == u)
    u := i; u /= Vec4(2, 6, 2, 2); assert(Vec4(1, 0.5, 2, 2.5) == u)
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
