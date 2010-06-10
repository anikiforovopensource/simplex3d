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

package test.math

import org.scalatest._

import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4bTest extends FunSuite {

  test("Factories") {
    var u: AnyVec4b = Vec4b(true)
    expect(classOf[Vec4b]) { u.getClass }
    expect(true) { u.x }
    expect(true) { u.y }
    expect(true) { u.z }
    expect(true) { u.w }

    u = Vec4b(false)
    expect(classOf[Vec4b]) { u.getClass }
    expect(false) { u.x }
    expect(false) { u.y }
    expect(false) { u.z }
    expect(false) { u.w }

    u = Vec4b(Mat2f(1, 1, 1, 1))
    expect(true) { u.x }
    expect(true) { u.y }
    expect(true) { u.z }
    expect(true) { u.w }

    u = Vec4b(Mat2f(0, 0, 0, 0))
    expect(false) { u.x }
    expect(false) { u.y }
    expect(false) { u.z }
    expect(false) { u.w }

    u = Vec4b(Mat2d(1, 1, 1, 1))
    expect(true) { u.x }
    expect(true) { u.y }
    expect(true) { u.z }
    expect(true) { u.w }

    u = Vec4b(Mat2d(0, 0, 0, 0))
    expect(false) { u.x }
    expect(false) { u.y }
    expect(false) { u.z }
    expect(false) { u.w }

    BooleanCombinations.test { (x, y, z, w) =>
      var u: AnyVec4b = Vec4b(x, y, z, w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec4b(x, y, z, w))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, y, Vec2b(z, w))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec2b(y, z), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec2b(x, y), z, w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec3b(y, z, w))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec3b(x, y, z), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec4i(int(x), int(y), int(z), int(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, y, Vec2i(int(z), int(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec2i(int(y), int(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec2i(int(x), int(y)), z, w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec3i(int(y), int(z), int(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec3i(int(x), int(y), int(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec4f(float(x), float(y), float(z), float(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, y, Vec2f(float(z), float(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec2f(float(y), float(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec2f(float(x), float(y)), z, w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec3f(float(y), float(z), float(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec3f(float(x), float(y), float(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec4d(double(x), double(y), double(z), double(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, y, Vec2d(double(z), double(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec2d(double(y), double(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec2d(double(x), double(y)), z, w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(x, Vec3d(double(y), double(z), double(w)))
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u = Vec4b(Vec3d(double(x), double(y), double(z)), w)
      expect(classOf[Vec4b]) { u.getClass }
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      var c: AnyVec4b = ConstVec4b(x, y, z, w)
      expect(classOf[ConstVec4b]) { c.getClass }
      expect(x) { c.x }
      expect(y) { c.y }
      expect(z) { c.z }
      expect(w) { c.w }

      c = ConstVec4b(Vec4b(x, y, z, w))
      expect(classOf[ConstVec4b]) { c.getClass }
      expect(x) { c.x }
      expect(y) { c.y }
      expect(z) { c.z }
      expect(w) { c.w }

      c = ConstVec4b(Vec4i(int(x), int(y), int(z), int(w)))
      expect(classOf[ConstVec4b]) { c.getClass }
      expect(x) { c.x }
      expect(y) { c.y }
      expect(z) { c.z }
      expect(w) { c.w }

      c = ConstVec4b(Vec4f(float(x), float(y), float(z), float(w)))
      expect(classOf[ConstVec4b]) { c.getClass }
      expect(x) { c.x }
      expect(y) { c.y }
      expect(z) { c.z }
      expect(w) { c.w }

      c = ConstVec4b(Vec4d(double(x), double(y), double(z), double(w)))
      expect(classOf[ConstVec4b]) { c.getClass }
      expect(x) { c.x }
      expect(y) { c.y }
      expect(z) { c.z }
      expect(w) { c.w }
    }
  }

  test("Unapply") {
    BooleanCombinations.test { (x, y, z, w) =>
      Vec4b(x, y, z, w) match {
        case Vec4b(ux, uy, uz, uw) =>
          if (ux != x || uy != y || uz != z || uw != w)
            throw new AssertionError()
      }
      ConstVec4b(x, y, z, w) match {
        case Vec4b(ux, uy, uz, uw) =>
          if (ux != x || uy != y || uz != z || uw != w)
            throw new AssertionError()
      }
    }
  }

  test("Const conversions") {
    BooleanCombinations.test { (x, y, z, w) =>
      val t: ConstVec4b = Vec4b(x, y, z, w)
      expect(classOf[ConstVec4b]) { t.getClass }
      assert(Vec4b(x, y, z, w) == t)

      var c: ConstVec4b = Vec4b(x, y, z, w); var v = Vec4b(false)
      expect(classOf[ConstVec4b]) { c.getClass }
      v = c; assert(Vec4b(x, y, z, w) == v)
      expect(classOf[Vec4b]) { v.getClass }

      c = Vec4b(true); v = Vec4b(x, y, z, w)
      expect(classOf[Vec4b]) { v.getClass }
      c = v; assert(Vec4b(x, y, z, w) == c)
      expect(classOf[ConstVec4b]) { c.getClass }
    }
  }

  test("Equality methods") {
    BooleanCombinations.test { (x, y, z, w) =>
      val m = Vec4b(x, y, z, w)
      val c = ConstVec4b(x, y, z, w)

      assert(m == m)
      assert(m == c)
      assert(c == m)
      assert(c == c)

      assert(m.equals(c))
      assert(!m.equals(Nil))

      assert(Vec4b(x, y, z, w) != Vec4b(!x, y, z, w))
      assert(Vec4b(x, y, z, w) != Vec4b(x, !y, z, w))
      assert(Vec4b(x, y, z, w) != Vec4b(x, y, !z, w))
      assert(Vec4b(x, y, z, w) != Vec4b(x, y, z, !w))
    }
  }

  test("Indexed read") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = ConstVec4b(x, y, z, w)

      expect(x) { u(0) }
      expect(y) { u(1) }
      expect(z) { u(2) }
      expect(w) { u(3) }

      intercept[IndexOutOfBoundsException] {
        u(4)
      }
      intercept[IndexOutOfBoundsException] {
        u(-1)
      }
    }
  }

  test("Indexed write") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec4b(x, y, z, w)

      u(0) = !x
      assert(Vec4b(!x, y, z, w) == u)

      u(1) = !y
      assert(Vec4b(!x, !y, z, w) == u)

      u(2) = !z
      assert(Vec4b(!x, !y, !z, w) == u)

      u(3) = !w
      assert(Vec4b(!x, !y, !z, !w) == u)

      intercept[IndexOutOfBoundsException] {
        u(4) = true
      }
      intercept[IndexOutOfBoundsException] {
        u(-1) = true
      }
    }
  }

  test("Setters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec4b(!x)

      u := Vec4b(x, y, z, w)
      expect(x) { u.x }
      expect(y) { u.y }
      expect(z) { u.z }
      expect(w) { u.w }

      u.set(!x, !y, !z, !w)
      expect(!x) { u.x }
      expect(!y) { u.y }
      expect(!z) { u.z }
      expect(!w) { u.w }
    }
  }

  test("Getters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec4b(x, y, z, w)
      expect(x) { u.r }
      expect(y) { u.g }
      expect(z) { u.b }
      expect(w) { u.a }
    }
  }

}
