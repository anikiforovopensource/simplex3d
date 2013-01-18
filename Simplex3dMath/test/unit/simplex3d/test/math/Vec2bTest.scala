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
class Vec2bTest extends FunSuite {

  test("Clone") {
    var t: ReadVec2b = Vec2b(true)
    assert(t.clone() ne t)
    assert(t.clone() == t)

    t = ConstVec2b(true)
    assert(t.clone() eq t)
  }

  test("Factories") {
    var u: ReadVec2b = Vec2b(true)
    expectResult(classOf[Vec2b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }

    u = Vec2b(false)
    expectResult(classOf[Vec2b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }

    u = ConstVec2b(true)
    expectResult(classOf[ConstVec2b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }

    u = ConstVec2b(false)
    expectResult(classOf[ConstVec2b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }

    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec2b = Vec2b(x, y)
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec2b(x, y))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec3b(x, y, z))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec4b(x, y, z, w))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec2i(toInt(x), toInt(y)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec3i(toInt(x), toInt(y), toInt(z)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec2f(toFloat(x), toFloat(y)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec2d(toDouble(x), toDouble(y)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      u = Vec2b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[Vec2b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }

      var c: ReadVec2b = ConstVec2b(x, y)
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(ConstVec2b(x, y))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec3b(x, y, z))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec4b(x, y, z, w))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec2i(toInt(x), toInt(y)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec3i(toInt(x), toInt(y), toInt(z)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec2f(toFloat(x), toFloat(y)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec2d(toDouble(x), toDouble(y)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }

      c = ConstVec2b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[ConstVec2b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
    }
  }

  test("Unapply") {
    BooleanCombinations.test { (x, y, z, w) =>
      Vec2b(x, y) match {
        case Vec2b(ux, uy) =>
          if (ux != x || uy != y) throw new AssertionError()
      }
      ConstVec2b(x, y) match {
        case Vec2b(ux, uy) =>
          if (ux != x || uy != y) throw new AssertionError()
      }
    }
  }

  test("Const conversions") {
    BooleanCombinations.test { (x, y, z, w) =>
      val t: ConstVec2b = Vec2b(x, y)
      expectResult(classOf[ConstVec2b]) { t.getClass }
      assert(Vec2b(x, y) == t)

      var c = ConstVec2b(true); val v = Vec2b(x, y)
      expectResult(classOf[Vec2b]) { v.getClass }
      c = v; assert(Vec2b(x, y) == c)
      expectResult(classOf[ConstVec2b]) { c.getClass }
    }
  }

  test("Equality methods") {
    BooleanCombinations.test { (x, y, z, w) =>
      val m = Vec2b(x, y)
      val c = ConstVec2b(x, y)

      assert(m == m)
      assert(m == c)
      assert(c == m)
      assert(c == c)

      assert(m.equals(c))
      assert(!m.equals(Nil))

      assert(Vec2b(x, y) != Vec2b(!x, y))
      assert(Vec2b(x, y) != Vec2b(x, !y))

      assert(Vec2b(x, y) != Vec2i(toInt(x), toInt(y)))
      assert(Vec2b(x, y) != Vec2f(toFloat(x), toFloat(y)))
      assert(Vec2b(x, y) != Vec2d(toDouble(x), toDouble(y)))
    }
  }

  test("Indexed read") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = ConstVec2b(x, y)

      expectResult(x) { u(0) }
      expectResult(y) { u(1) }

      intercept[IndexOutOfBoundsException] {
        u(2)
      }
      intercept[IndexOutOfBoundsException] {
        u(-1)
      }
    }
  }

  test("Indexed write") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec2b(x, y)

      u(0) = !x
      assert(Vec2b(!x, y) == u)

      u(1) = !y
      assert(Vec2b(!x, !y) == u)

      intercept[IndexOutOfBoundsException] {
        u(2) = true
      }
      intercept[IndexOutOfBoundsException] {
        u(-1) = true
      }
    }
  }

  test("Setters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec2b(!x)

      u := Vec2b(x, y)
      expectResult(x) { u.x }
      expectResult(y) { u.y }
    }
  }

  test("Getters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec2b(x, y)
      expectResult(x) { u.r }
      expectResult(y) { u.g }
    }
  }

}
