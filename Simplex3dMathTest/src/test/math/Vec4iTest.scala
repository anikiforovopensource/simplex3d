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

package test.math

import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class Vec4iTest extends FunSuite {

  test("Clone") {
    var t: ReadVec4i = Vec4i(1)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstVec4i(1)
    assert(t.clone() eq t)
  }

  test("Factories") {
    val x = 3
    val y = 4
    val z = 5
    val w = 6

    var u: ReadVec4i = Vec4i(x)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(x) { u.y }
    expect(x) { u.z }
    expect(x) { u.w }

    u = ConstVec4i(x)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(x) { u.y }
    expect(x) { u.z }
    expect(x) { u.w }

    u = Vec4i(Mat2f(x, y, z, w))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Mat2d(x, y, z, w))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Quat4f(w, x, y, z))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Quat4d(w, x, y, z))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, y, z, w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, y, Vec2i(toInt(z), toInt(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec2i(toInt(y), toInt(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec2i(toInt(x), toInt(y)), z, w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec3i(toInt(y), toInt(z), toInt(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec3i(toInt(x), toInt(y), toInt(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, y, Vec2f(toFloat(z), toFloat(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec2f(toFloat(y), toFloat(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec2f(toFloat(x), toFloat(y)), z, w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec3f(toFloat(y), toFloat(z), toFloat(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec3f(toFloat(x), toFloat(y), toFloat(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, y, Vec2d(toDouble(z), toDouble(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec2d(toDouble(y), toDouble(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec2d(toDouble(x), toDouble(y)), z, w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = Vec4i(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
    expect(classOf[Vec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Mat2f(x, y, z, w))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Mat2d(x, y, z, w))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Quat4f(w, x, y, z))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Quat4d(w, x, y, z))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, y, z, w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(ConstVec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, y, Vec2i(toInt(z), toInt(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec2i(toInt(y), toInt(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec2i(toInt(x), toInt(y)), z, w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec3i(toInt(y), toInt(z), toInt(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec3i(toInt(x), toInt(y), toInt(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, y, Vec2f(toFloat(z), toFloat(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec2f(toFloat(y), toFloat(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec2f(toFloat(x), toFloat(y)), z, w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec3f(toFloat(y), toFloat(z), toFloat(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec3f(toFloat(x), toFloat(y), toFloat(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, y, Vec2d(toDouble(z), toDouble(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec2d(toDouble(y), toDouble(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec2d(toDouble(x), toDouble(y)), z, w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(x, Vec3d(toDouble(y), toDouble(z), toDouble(w)))
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }

    u = ConstVec4i(Vec3d(toDouble(x), toDouble(y), toDouble(z)), w)
    expect(classOf[ConstVec4i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
    expect(w) { u.w }
  }

  test("Boolean factories") {
    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec4i = Vec4i(Vec4b(x, y, z, w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4i(toInt(x), toInt(y), Vec2b(z, w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4i(toInt(x), Vec2b(y, z), toInt(w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4i(Vec2b(x, y), toInt(z), toInt(w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4i(toInt(x), Vec3b(y, z, w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      u = Vec4i(Vec3b(x, y, z), toInt(w))
      expect(classOf[Vec4i]) { u.getClass }
      expect(toInt(x)) { u.x }
      expect(toInt(y)) { u.y }
      expect(toInt(z)) { u.z }
      expect(toInt(w)) { u.w }

      var c: ReadVec4i = ConstVec4i(Vec4b(x, y, z, w))
      expect(classOf[ConstVec4i]) { c.getClass }
      expect(toInt(x)) { c.x }
      expect(toInt(y)) { c.y }
      expect(toInt(z)) { c.z }
      expect(toInt(w)) { c.w }
    }
  }

  test("Unapply") {
    val x = 1; val y = 2; val z = 3; val w = 4
    Vec4i(x, y, z, w) match {
      case Vec4i(ux, uy, uz, uw) =>
        if (ux != x || uy != y || uz != z || uw != w)
          throw new AssertionError()
    }
    ConstVec4i(x, y, z, w) match {
      case Vec4i(ux, uy, uz, uw) =>
        if (ux != x || uy != y || uz != z || uw != w)
          throw new AssertionError()
    }
  }

  test("Const conversions") {
    val x = 1
    val y = 2
    val z = 3
    val w = 4

    val t: ConstVec4i = Vec4i(x, y, z, w)
    expect(classOf[ConstVec4i]) { t.getClass }
    assert(Vec4i(x, y, z, w) == t)

    var c = ConstVec4i(5); val v = Vec4i(x, y, z, w)
    expect(classOf[Vec4i]) { v.getClass }
    c = v; assert(Vec4i(x, y, z, w) == c)
    expect(classOf[ConstVec4i]) { c.getClass }
  }

  test("Equality methods") {
    val m = Vec4i(4, 7, 9, 1)
    val c = ConstVec4i(4, 7, 9, 1)

    assert(m == m)
    assert(m == c)
    assert(c == m)
    assert(c == c)

    assert(m.equals(c))
    assert(!m.equals(Nil))

    assert(Vec4i(1, 2, 3, 4) != Vec4i(9, 2, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 9, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 2, 9, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4i(1, 2, 3, 9))

    assert(Vec4i(0) != Vec4b(false))

    assert(Vec4i(1, 2, 3, 4) == Vec4f(1, 2, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4f(9, 2, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4f(1, 9, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4f(1, 2, 9, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4f(1, 2, 3, 9))

    assert(Vec4i(1, 2, 3, 4) == Vec4d(1, 2, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4d(9, 2, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4d(1, 9, 3, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4d(1, 2, 9, 4))
    assert(Vec4i(1, 2, 3, 4) != Vec4d(1, 2, 3, 9))
  }

  test("Indexed read") {
    val u = ConstVec4i(3, 4, 5, 6)

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
    val u = Vec4i(3, 4, 5, 6)

    u(0) = 5
    assert(Vec4i(5, 4, 5, 6) == u)

    u(1) = 6
    assert(Vec4i(5, 6, 5, 6) == u)

    u(2) = 7
    assert(Vec4i(5, 6, 7, 6) == u)

    u(3) = 8
    assert(Vec4i(5, 6, 7, 8) == u)

    intercept[IndexOutOfBoundsException] {
      u(4) = 1
    }
    intercept[IndexOutOfBoundsException] {
      u(-1) = 1
    }
  }

  test("Setters") {
    val u = Vec4i(0)

    u := Vec4i(1, 2, 3, 4)
    expect(1) { u.x }
    expect(2) { u.y }
    expect(3) { u.z }
    expect(4) { u.w }
  }

  test("Const math") {
    val u = ConstVec4i(10, 20, 30, 40)

    assert(+u eq u)
    
    assert(Vec4i(-10, -20, -30, -40) == -u)
    assert(Vec4i(~10, ~20, ~30, ~40) == ~u)
    
    assert(Vec4i(20, 40, 60, 80) == u*2)
    assert(Vec4i(5, 10, 15, 20) == u / 2)

    assert(Vec4i(12, 22, 32, 42) == u + 2)
    assert(Vec4i(8, 18, 28, 38) == u - 2)

    assert(Vec4i(1, 2, 0, 1) == u % 3)

    val v = ConstVec4i(2, 3, 4, 5)

    assert(Vec4i(12, 23, 34, 45) == u + v)
    assert(Vec4i(8, 17, 26, 35) == u - v)
    assert(Vec4i(20, 60, 120, 200) == u * v)
    assert(Vec4i(5, 6, 7, 8) == u / v)
    assert(Vec4i(0, 2, 2, 0) == u % v)

    val b = ConstVec4i(0xF, 0xFF, 0xFFF, 0xFFFF)

    assert(Vec4i(0, 0xF, 0xFF, 0xFFF) == b >> 4)
    assert(Vec4i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4, -0xFFFF >>> 4) == -b >>> 4)
    assert(Vec4i(0xF0, 0xFF0, 0xFFF0, 0xFFFF0) == b << 4)

    assert(Vec4i(0xF, 0xF, 0xF, 0xF) == (b & 0xF))
    assert(Vec4i(0xFF, 0xFF, 0xFFF, 0xFFFF) == (b | 0xFF))
    assert(Vec4i(0xF0, 0, 0xF00, 0xFF00) == (b ^ 0xFF))

    assert(Vec4i(0x3, 0xF, 0x3F, 0xFF) == (b >> Vec4i(2, 4, 6, 8)))
    assert(Vec4i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6, -0xFFFF >>> 8) == (-b >>> Vec4i(2, 4, 6, 8)))
    assert(Vec4i(0xF0, 0xFF00, 0xFFF000, 0xFFFF0000) == (b << Vec4i(4, 8, 12, 16)))

    assert(Vec4i(0xF, 0xFF, 0xFF, 0xF) == (b & Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
    assert(Vec4i(0xFFFF, 0xFFF, 0xFFF, 0xFFFF) == (b | Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
    assert(Vec4i(0xFFF0, 0xF00, 0xF00, 0xFFF0) == (b ^ Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF)))
  }

  test("Mutable math") {
    val i = ConstVec4i(10, 20, 30, 40)
    val u = Vec4i(0)

    u := i; u *= 2; assert(Vec4i(20, 40, 60, 80) == u)
    u := i; u /= 2; assert(Vec4i(5, 10, 15, 20) == u)

    u := i; u += 2; assert(Vec4i(12, 22, 32, 42) == u)
    u := i; u -= 2; assert(Vec4i(8, 18, 28, 38) == u)

    u := i; u %= 3; assert(Vec4i(1, 2, 0, 1) == u)

    val v = ConstVec4i(2, 3, 4, 5)

    u := i; u += v; assert(Vec4i(12, 23, 34, 45) == u)
    u := i; u -= v; assert(Vec4i(8, 17, 26, 35) == u)
    u := i; u *= v; assert(Vec4i(20, 60, 120, 200) == u)
    u := i; u /= v; assert(Vec4i(5, 6, 7, 8) == u)
    u := i; u %= v; assert(Vec4i(0, 2, 2, 0) == u)

    val b = ConstVec4i(0xF, 0xFF, 0xFFF, 0xFFFF)

    u := b; u >>= 4; assert(Vec4i(0, 0xF, 0xFF, 0xFFF) == u)
    u := -b; u >>>= 4; assert(Vec4i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4, -0xFFFF >>> 4) == u)
    u := b; u <<= 4; assert(Vec4i(0xF0, 0xFF0, 0xFFF0, 0xFFFF0) == u)

    u := b; u &= 0xF; assert(Vec4i(0xF, 0xF, 0xF, 0xF) == u)
    u := b; u |= 0xFF; assert(Vec4i(0xFF, 0xFF, 0xFFF, 0xFFFF) == u)
    u := b; u ^= 0xFF; assert(Vec4i(0xF0, 0, 0xF00, 0xFF00) == u)

    u := b; u >>= Vec4i(2, 4, 6, 8); assert(Vec4i(0x3, 0xF, 0x3F, 0xFF) == u)
    u := -b; u >>>= Vec4i(2, 4, 6, 8); assert(Vec4i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6, -0xFFFF >>> 8) == u)
    u := b; u <<= Vec4i(4, 8, 12, 16); assert(Vec4i(0xF0, 0xFF00, 0xFFF000, 0xFFFF0000) == u)

    u := b; u &= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xF, 0xFF, 0xFF, 0xF) == u)
    u := b; u |= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xFFFF, 0xFFF, 0xFFF, 0xFFFF) == u)
    u := b; u ^= Vec4i(0xFFFF, 0xFFF, 0xFF, 0xF); assert(Vec4i(0xFFF0, 0xF00, 0xF00, 0xFFF0) == u)
  }

}
