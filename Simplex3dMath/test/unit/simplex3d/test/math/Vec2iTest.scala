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
class Vec2iTest extends FunSuite {

  test("Clone") {
    var t: ReadVec2i = Vec2i(1)
    assert(t.clone ne t)
    assert(t.clone == t)

    t = ConstVec2i(1)
    assert(t.clone eq t)
  }

  test("Factories") {
    val x = 3
    val y = 4
    val z = 5
    val w = 6

    var u: ReadVec2i = Vec2i(x)
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(x) { u.y }

    u = ConstVec2i(x)
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(x) { u.y }

    u = Vec2i(x, y)
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec2i(toInt(x), toInt(y)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec3i(toInt(x), toInt(y), toInt(z)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec2f(toFloat(x), toFloat(y)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec2d(toDouble(x), toDouble(y)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = Vec2i(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
    expectResult(classOf[Vec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(x, y)
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(ConstVec2i(toInt(x), toInt(y)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec3i(toInt(x), toInt(y), toInt(z)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec2f(toFloat(x), toFloat(y)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec2d(toDouble(x), toDouble(y)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }

    u = ConstVec2i(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
    expectResult(classOf[ConstVec2i]) { u.getClass }
    expectResult(x) { u.x }
    expectResult(y) { u.y }
  }

  test("Boolean factories") {
    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec2i = Vec2i(Vec2b(x, y))
      expectResult(classOf[Vec2i]) { u.getClass }
      expectResult(toInt(x)) { u.x }
      expectResult(toInt(y)) { u.y }

      u = Vec2i(Vec3b(x, y, z))
      expectResult(classOf[Vec2i]) { u.getClass }
      expectResult(toInt(x)) { u.x }
      expectResult(toInt(y)) { u.y }

      u = Vec2i(Vec4b(x, y, z, w))
      expectResult(classOf[Vec2i]) { u.getClass }
      expectResult(toInt(x)) { u.x }
      expectResult(toInt(y)) { u.y }

      var c: ReadVec2i = ConstVec2i(Vec2b(x, y))
      expectResult(classOf[ConstVec2i]) { c.getClass }
      expectResult(toInt(x)) { c.x }
      expectResult(toInt(y)) { c.y }
    }
  }

  test("Unapply") {
    val x = 1; val y = 2
    Vec2i(x, y) match {
      case Vec2i(ux, uy) =>
        if (ux != x || uy != y)
          throw new AssertionError()
    }
    ConstVec2i(x, y) match {
      case Vec2i(ux, uy) =>
        if (ux != x || uy != y)
          throw new AssertionError()
    }
  }

  test("Const conversions") {
    val x = 1
    val y = 2

    val t: ConstVec2i = Vec2i(x, y)
    expectResult(classOf[ConstVec2i]) { t.getClass }
    assert(Vec2i(x, y) == t)

    var c = ConstVec2i(5); val v = Vec2i(x, y)
    expectResult(classOf[Vec2i]) { v.getClass }
    c = v; assert(Vec2i(x, y) == c)
    expectResult(classOf[ConstVec2i]) { c.getClass }
  }

  test("Equality methods") {
    val m = Vec2i(4, 7)
    val c = ConstVec2i(4, 7)

    assert(m == m)
    assert(m == c)
    assert(c == m)
    assert(c == c)

    assert(m.equals(c))
    assert(!m.equals(Nil))

    assert(Vec2i(1, 2) != Vec2i(9, 2))
    assert(Vec2i(1, 2) != Vec2i(1, 9))

    assert(Vec2i(0) != Vec2b(false))

    assert(Vec2i(1, 2) == Vec2f(1, 2))
    assert(Vec2i(1, 2) != Vec2f(9, 2))
    assert(Vec2i(1, 2) != Vec2f(1, 9))

    assert(Vec2i(1, 2) == Vec2d(1, 2))
    assert(Vec2i(1, 2) != Vec2d(9, 2))
    assert(Vec2i(1, 2) != Vec2d(1, 9))
  }

  test("Indexed read") {
    val u = ConstVec2i(3, 4)

    expectResult(3) { u(0) }
    expectResult(4) { u(1) }

    intercept[IndexOutOfBoundsException] {
      u(2)
    }
    intercept[IndexOutOfBoundsException] {
      u(-1)
    }
  }

  test("Indexed write") {
    val u = Vec2i(3, 4)

    u(0) = 5
    assert(Vec2i(5, 4) == u)

    u(1) = 6
    assert(Vec2i(5, 6) == u)

    intercept[IndexOutOfBoundsException] {
      u(2) = 1
    }
    intercept[IndexOutOfBoundsException] {
      u(-1) = 1
    }
  }

  test("Setters") {
    val u = Vec2i(0)

    u := Vec2i(1, 2)
    expectResult(1) { u.x }
    expectResult(2) { u.y }
  }

  test("Const math") {
    val u = ConstVec2i(10, 20)

    assert(+u eq u)
    
    assert(Vec2i(-10, -20) == -u)
    assert(Vec2i(~10, ~20) == ~u)
    
    assert(Vec2i(20, 40) == u*2)
    assert(Vec2i(5, 10) == u / 2)

    assert(Vec2i(12, 22) == u + 2)
    assert(Vec2i(8, 18) == u - 2)

    assert(Vec2i(1, 2) == u % 3)

    val v = ConstVec2i(2, 3)

    assert(Vec2i(12, 23) == u + v)
    assert(Vec2i(8, 17) == u - v)
    assert(Vec2i(20, 60) == u * v)
    assert(Vec2i(5, 6) == u / v)
    assert(Vec2i(0, 2) == u % v)

    val b = ConstVec2i(0xF, 0xFF)

    assert(Vec2i(0, 0xF) == b >> 4)
    assert(Vec2i(-0xF >>> 4, -0xFF >>> 4) == -b >>> 4)
    assert(Vec2i(0xF0, 0xFF0) == b << 4)

    assert(Vec2i(0xF, 0xF) == (b & 0xF))
    assert(Vec2i(0xFF, 0xFF) == (b | 0xFF))
    assert(Vec2i(0xF0, 0) == (b ^ 0xFF))

    assert(Vec2i(0x3, 0xF) == (b >> Vec2i(2, 4)))
    assert(Vec2i(-0xF >>> 2, -0xFF >>> 4) == (-b >>> Vec2i(2, 4)))
    assert(Vec2i(0xF0, 0xFF00) == (b << Vec2i(4, 8)))

    assert(Vec2i(0xF, 0xF) == (b & Vec2i(0xFF, 0xF)))
    assert(Vec2i(0xFF, 0xFF) == (b | Vec2i(0xFF, 0xF)))
    assert(Vec2i(0xF0, 0xF0) == (b ^ Vec2i(0xFF, 0xF)))
  }

  test("Mutable math") {
    val i = ConstVec2i(10, 20)
    val u = Vec2i(0)

    u := i; u *= 2; assert(Vec2i(20, 40) == u)
    u := i; u /= 2; assert(Vec2i(5, 10) == u)

    u := i; u += 2; assert(Vec2i(12, 22) == u)
    u := i; u -= 2; assert(Vec2i(8, 18) == u)

    u := i; u %= 3; assert(Vec2i(1, 2) == u)

    val v = ConstVec2i(2, 3)

    u := i; u += v; assert(Vec2i(12, 23) == u)
    u := i; u -= v; assert(Vec2i(8, 17) == u)
    u := i; u *= v; assert(Vec2i(20, 60) == u)
    u := i; u /= v; assert(Vec2i(5, 6) == u)
    u := i; u %= v; assert(Vec2i(0, 2) == u)

    val b = ConstVec2i(0xF, 0xFF)

    u := b; u >>= 4; assert(Vec2i(0, 0xF) == u)
    u := -b; u >>>= 4; assert(Vec2i(-0xF >>> 4, -0xFF >>> 4) == u)
    u := b; u <<= 4; assert(Vec2i(0xF0, 0xFF0) == u)
    
    u := b; u &= 0xF; assert(Vec2i(0xF, 0xF) == u)
    u := b; u |= 0xFF; assert(Vec2i(0xFF, 0xFF) == u)
    u := b; u ^= 0xFF; assert(Vec2i(0xF0, 0) == u)

    u := b; u >>= Vec2i(2, 4); assert(Vec2i(0x3, 0xF) == u)
    u := -b; u >>>= Vec2i(2, 4); assert(Vec2i(-0xF >>> 2, -0xFF >>> 4) == u)
    u := b; u <<= Vec2i(4, 8); assert(Vec2i(0xF0, 0xFF00) == u)

    u := b; u &= Vec2i(0xFF, 0xF); assert(Vec2i(0xF, 0xF) == u)
    u := b; u |= Vec2i(0xFF, 0xF); assert(Vec2i(0xFF, 0xFF) == u)
    u := b; u ^= Vec2i(0xFF, 0xF); assert(Vec2i(0xF0, 0xF0) == u)
  }

}
