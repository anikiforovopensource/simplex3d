/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Simplex3d Team
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
class Vec3iTest extends FunSuite {

  test("Clone") {
    var t: ReadVec3i = Vec3i(1)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstVec3i(1)
    assert(t.clone() eq t)
  }

  test("Factories") {
    val x = 3
    val y = 4
    val z = 5
    val w = 6

    var u: ReadVec3i = Vec3i(x)
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(x) { u.y }
    expect(x) { u.z }

    u = ConstVec3i(x)
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(x) { u.y }
    expect(x) { u.z }

    u = Vec3i(x, y, z)
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec3i(Int(x), Int(y), Int(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(x, Vec2i(Int(y), Int(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec2i(Int(x), Int(y)), z)
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec4i(Int(x), Int(y), Int(z), Int(w)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec3f(Float(x), Float(y), Float(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(x, Vec2f(Float(y), Float(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec2f(Float(x), Float(y)), z)
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec4f(Float(x), Float(y), Float(z), Float(w)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec3d(Double(x), Double(y), Double(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(x, Vec2d(Double(y), Double(z)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec2d(Double(x), Double(y)), z)
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = Vec3i(Vec4d(Double(x), Double(y), Double(z), Double(w)))
    expect(classOf[Vec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(x, y, z)
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(ConstVec3i(Int(x), Int(y), Int(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(x, Vec2i(Int(y), Int(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec2i(Int(x), Int(y)), z)
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec4i(Int(x), Int(y), Int(z), Int(w)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec3f(Float(x), Float(y), Float(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(x, Vec2f(Float(y), Float(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec2f(Float(x), Float(y)), z)
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec4f(Float(x), Float(y), Float(z), Float(w)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec3d(Double(x), Double(y), Double(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(x, Vec2d(Double(y), Double(z)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec2d(Double(x), Double(y)), z)
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }

    u = ConstVec3i(Vec4d(Double(x), Double(y), Double(z), Double(w)))
    expect(classOf[ConstVec3i]) { u.getClass }
    expect(x) { u.x }
    expect(y) { u.y }
    expect(z) { u.z }
  }

  test("Boolean factories") {
    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec3i = Vec3i(Vec3b(x, y, z))
      expect(classOf[Vec3i]) { u.getClass }
      expect(Int(x)) { u.x }
      expect(Int(y)) { u.y }
      expect(Int(z)) { u.z }

      u = Vec3i(Int(x), Vec2b(y, z))
      expect(classOf[Vec3i]) { u.getClass }
      expect(Int(x)) { u.x }
      expect(Int(y)) { u.y }
      expect(Int(z)) { u.z }

      u = Vec3i(Vec2b(x, y), Int(z))
      expect(classOf[Vec3i]) { u.getClass }
      expect(Int(x)) { u.x }
      expect(Int(y)) { u.y }
      expect(Int(z)) { u.z }

      u = Vec3i(Vec4b(x, y, z, w))
      expect(classOf[Vec3i]) { u.getClass }
      expect(Int(x)) { u.x }
      expect(Int(y)) { u.y }
      expect(Int(z)) { u.z }

      var c: ReadVec3i = ConstVec3i(Vec3b(x, y, z))
      expect(classOf[ConstVec3i]) { c.getClass }
      expect(Int(x)) { c.x }
      expect(Int(y)) { c.y }
      expect(Int(z)) { c.z }
    }
  }

  test("Unapply") {
    val x = 1; val y = 2; val z = 3
    Vec3i(x, y, z) match {
      case Vec3i(ux, uy, uz) =>
        if (ux != x || uy != y || uz != z)
          throw new AssertionError()
    }
    ConstVec3i(x, y, z) match {
      case Vec3i(ux, uy, uz) =>
        if (ux != x || uy != y || uz != z)
          throw new AssertionError()
    }
  }

  test("Const conversions") {
    val x = 1
    val y = 2
    val z = 3

    val t: ConstVec3i = Vec3i(x, y, z)
    expect(classOf[ConstVec3i]) { t.getClass }
    assert(Vec3i(x, y, z) == t)

    var c: ConstVec3i = Vec3i(x, y, z); var v = Vec3i(3)
    expect(classOf[ConstVec3i]) { c.getClass }
    v = c; assert(Vec3i(x, y, z) == v)
    expect(classOf[Vec3i]) { v.getClass }

    c = Vec3i(5); v = Vec3i(x, y, z)
    expect(classOf[Vec3i]) { v.getClass }
    c = v; assert(Vec3i(x, y, z) == c)
    expect(classOf[ConstVec3i]) { c.getClass }
  }

  test("Equality methods") {
    val m = Vec3i(4, 7, 9)
    val c = ConstVec3i(4, 7, 9)

    assert(m == m)
    assert(m == c)
    assert(c == m)
    assert(c == c)

    assert(m.equals(c))
    assert(!m.equals(Nil))

    assert(Vec3i(1, 2, 3) != Vec3i(9, 2, 3))
    assert(Vec3i(1, 2, 3) != Vec3i(1, 9, 3))
    assert(Vec3i(1, 2, 3) != Vec3i(1, 2, 9))

    assert(Vec3i(0) != Vec3b(false))

    assert(Vec3i(1, 2, 3) == Vec3f(1, 2, 3))
    assert(Vec3i(1, 2, 3) != Vec3f(9, 2, 3))
    assert(Vec3i(1, 2, 3) != Vec3f(1, 9, 3))
    assert(Vec3i(1, 2, 3) != Vec3f(1, 2, 9))

    assert(Vec3i(1, 2, 3) == Vec3d(1, 2, 3))
    assert(Vec3i(1, 2, 3) != Vec3d(9, 2, 3))
    assert(Vec3i(1, 2, 3) != Vec3d(1, 9, 3))
    assert(Vec3i(1, 2, 3) != Vec3d(1, 2, 9))
  }

  test("Indexed read") {
    val u = ConstVec3i(3, 4, 5)

    expect(3) { u(0) }
    expect(4) { u(1) }
    expect(5) { u(2) }

    intercept[IndexOutOfBoundsException] {
      u(3)
    }
    intercept[IndexOutOfBoundsException] {
      u(-1)
    }
  }

  test("Indexed write") {
    val u = Vec3i(3, 4, 5)

    u(0) = 5
    assert(Vec3i(5, 4, 5) == u)

    u(1) = 6
    assert(Vec3i(5, 6, 5) == u)

    u(2) = 7
    assert(Vec3i(5, 6, 7) == u)

    intercept[IndexOutOfBoundsException] {
      u(3) = 1
    }
    intercept[IndexOutOfBoundsException] {
      u(-1) = 1
    }
  }

  test("Setters") {
    val u = Vec3i(0)

    u := Vec3i(1, 2, 3)
    expect(1) { u.x }
    expect(2) { u.y }
    expect(3) { u.z }
  }

  test("Const math") {
    val u = ConstVec3i(10, 20, 30)

    assert(+u eq u)
    
    assert(Vec3i(-10, -20, -30) == -u)
    assert(Vec3i(~10, ~20, ~30) == ~u)
    
    assert(Vec3i(20, 40, 60) == u*2)
    assert(Vec3i(5, 10, 15) == u / 2)

    assert(Vec3i(12, 22, 32) == u + 2)
    assert(Vec3i(8, 18, 28) == u - 2)

    assert(Vec3i(1, 2, 0) == u % 3)

    val v = ConstVec3i(2, 3, 4)

    assert(Vec3i(12, 23, 34) == u + v)
    assert(Vec3i(8, 17, 26) == u - v)
    assert(Vec3i(20, 60, 120) == u * v)
    assert(Vec3i(5, 6, 7) == u / v)
    assert(Vec3i(0, 2, 2) == u % v)

    val b = ConstVec3i(0xF, 0xFF, 0xFFF)

    assert(Vec3i(0, 0xF, 0xFF) == b >> 4)
    assert(Vec3i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4) == -b >>> 4)
    assert(Vec3i(0xF0, 0xFF0, 0xFFF0) == b << 4)

    assert(Vec3i(0xF, 0xF, 0xF) == (b & 0xF))
    assert(Vec3i(0xFF, 0xFF, 0xFFF) == (b | 0xFF))
    assert(Vec3i(0xF0, 0, 0xF00) == (b ^ 0xFF))

    assert(Vec3i(0x3, 0xF, 0x3F) == (b >> Vec3i(2, 4, 6)))
    assert(Vec3i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6) == (-b >>> Vec3i(2, 4, 6)))
    assert(Vec3i(0xF0, 0xFF00, 0xFFF000) == (b << Vec3i(4, 8, 12)))

    assert(Vec3i(0xF, 0xFF, 0xF) == (b & Vec3i(0xFFF, 0xFF, 0xF)))
    assert(Vec3i(0xFFF, 0xFF, 0xFFF) == (b | Vec3i(0xFFF, 0xFF, 0xF)))
    assert(Vec3i(0xFF0, 0x00, 0xFF0) == (b ^ Vec3i(0xFFF, 0xFF, 0xF)))
  }

  test("Mutable math") {
    val i = ConstVec3i(10, 20, 30)
    val u = Vec3i(0)

    u := i; u *= 2; assert(Vec3i(20, 40, 60) == u)
    u := i; u /= 2; assert(Vec3i(5, 10, 15) == u)

    u := i; u += 2; assert(Vec3i(12, 22, 32) == u)
    u := i; u -= 2; assert(Vec3i(8, 18, 28) == u)

    u := i; u %= 3; assert(Vec3i(1, 2, 0) == u)

    val v = ConstVec3i(2, 3, 4)

    u := i; u += v; assert(Vec3i(12, 23, 34) == u)
    u := i; u -= v; assert(Vec3i(8, 17, 26) == u)
    u := i; u *= v; assert(Vec3i(20, 60, 120) == u)
    u := i; u /= v; assert(Vec3i(5, 6, 7) == u)
    u := i; u %= v; assert(Vec3i(0, 2, 2) == u)

    val b = ConstVec3i(0xF, 0xFF, 0xFFF)

    u := b; u >>= 4; assert(Vec3i(0, 0xF, 0xFF) == u)
    u := -b; u >>>= 4; assert(Vec3i(-0xF >>> 4, -0xFF >>> 4, -0xFFF >>> 4) == u)
    u := b; u <<= 4; assert(Vec3i(0xF0, 0xFF0, 0xFFF0) == u)

    u := b; u &= 0xF; assert(Vec3i(0xF, 0xF, 0xF) == u)
    u := b; u |= 0xFF; assert(Vec3i(0xFF, 0xFF, 0xFFF) == u)
    u := b; u ^= 0xFF; assert(Vec3i(0xF0, 0, 0xF00) == u)

    u := b; u >>= Vec3i(2, 4, 6); assert(Vec3i(0x3, 0xF, 0x3F) == u)
    u := -b; u >>>= Vec3i(2, 4, 6); assert(Vec3i(-0xF >>> 2, -0xFF >>> 4, -0xFFF >>> 6) == u)
    u := b; u <<= Vec3i(4, 8, 12); assert(Vec3i(0xF0, 0xFF00, 0xFFF000) == u)

    u := b; u &= Vec3i(0xFFF, 0xFF, 0xF); assert(Vec3i(0xF, 0xFF, 0xF) == u)
    u := b; u |= Vec3i(0xFFF, 0xFF, 0xF); assert(Vec3i(0xFFF, 0xFF, 0xFFF) == u)
    u := b; u ^= Vec3i(0xFFF, 0xFF, 0xF); assert(Vec3i(0xFF0, 0x00, 0xFF0) == u)
  }

}
