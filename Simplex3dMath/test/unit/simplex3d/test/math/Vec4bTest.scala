/*
 * Simplex3dMath - Test Package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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

package simplex3d.test.math

import org.scalatest._

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4bTest extends FunSuite {

  test("Clone") {
    var t: ReadVec4b = Vec4b(true)
    assert(t.clone ne t)
    assert(t.clone == t)

    t = ConstVec4b(true)
    assert(t.clone eq t)
  }

  test("Factories") {
    var u: ReadVec4b = Vec4b(true)
    expectResult(classOf[Vec4b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = Vec4b(false)
    expectResult(classOf[Vec4b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = Vec4b(Mat2f(1, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = Vec4b(Mat2f(0, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = Vec4b(Mat2d(1, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = Vec4b(Mat2d(0, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = Vec4b(Quat4f(0, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(false) { u.w }

    u = Vec4b(Quat4f(1, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(true) { u.w }

    u = Vec4b(Quat4d(0, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(false) { u.w }

    u = Vec4b(Quat4d(1, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(true) { u.w }

    u = ConstVec4b(true)
    expectResult(classOf[ConstVec4b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = ConstVec4b(false)
    expectResult(classOf[ConstVec4b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = ConstVec4b(Mat2f(1, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = ConstVec4b(Mat2f(0, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = ConstVec4b(Mat2d(1, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(true) { u.w }

    u = ConstVec4b(Mat2d(0, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(false) { u.w }

    u = ConstVec4b(Quat4f(0, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(false) { u.w }

    u = ConstVec4b(Quat4f(1, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(true) { u.w }

    u = ConstVec4b(Quat4d(0, 1, 1, 1))
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }
    expectResult(false) { u.w }

    u = ConstVec4b(Quat4d(1, 0, 0, 0))
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }
    expectResult(true) { u.w }

    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec4b = Vec4b(x, y, z, w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec4b(x, y, z, w))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, y, Vec2b(z, w))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec2b(y, z), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec2b(x, y), z, w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec3b(y, z, w))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec3b(x, y, z), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, y, Vec2i(toInt(z), toInt(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec2i(toInt(y), toInt(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec2i(toInt(x), toInt(y)), z, w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec3i(toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec3i(toInt(x), toInt(y), toInt(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, y, Vec2f(toFloat(z), toFloat(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec2f(toFloat(y), toFloat(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec2f(toFloat(x), toFloat(y)), z, w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec3f(toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec3f(toFloat(x), toFloat(y), toFloat(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, y, Vec2d(toDouble(z), toDouble(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec2d(toDouble(y), toDouble(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec2d(toDouble(x), toDouble(y)), z, w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      u = Vec4b(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
      expectResult(classOf[Vec4b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }

      var c: ReadVec4b = ConstVec4b(x, y, z, w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(ConstVec4b(x, y, z, w))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, y, Vec2b(z, w))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec2b(y, z), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec2b(x, y), z, w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec3b(y, z, w))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec3b(x, y, z), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, y, Vec2i(toInt(z), toInt(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec2i(toInt(y), toInt(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec2i(toInt(x), toInt(y)), z, w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec3i(toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec3i(toInt(x), toInt(y), toInt(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, y, Vec2f(toFloat(z), toFloat(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec2f(toFloat(y), toFloat(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec2f(toFloat(x), toFloat(y)), z, w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec3f(toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec3f(toFloat(x), toFloat(y), toFloat(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, y, Vec2d(toDouble(z), toDouble(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec2d(toDouble(y), toDouble(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec2d(toDouble(x), toDouble(y)), z, w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }

      c = ConstVec4b(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
      expectResult(classOf[ConstVec4b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
      expectResult(w) { c.w }
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
      expectResult(classOf[ConstVec4b]) { t.getClass }
      assert(Vec4b(x, y, z, w) == t)

      var c = ConstVec4b(true); val v = Vec4b(x, y, z, w)
      expectResult(classOf[Vec4b]) { v.getClass }
      c = v; assert(Vec4b(x, y, z, w) == c)
      expectResult(classOf[ConstVec4b]) { c.getClass }
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

      assert(Vec4b(x, y, z, w) != Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      assert(Vec4b(x, y, z, w) != Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      assert(Vec4b(x, y, z, w) != Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
    }
  }

  test("Indexed read") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = ConstVec4b(x, y, z, w)

      expectResult(x) { u(0) }
      expectResult(y) { u(1) }
      expectResult(z) { u(2) }
      expectResult(w) { u(3) }

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
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
      expectResult(w) { u.w }
    }
  }

  test("Getters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec4b(x, y, z, w)
      expectResult(x) { u.r }
      expectResult(y) { u.g }
      expectResult(z) { u.b }
      expectResult(w) { u.a }
    }
  }

}
