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
class Vec3bTest extends FunSuite {

  test("Clone") {
    var t: ReadVec3b = Vec3b(true)
    assert(t.clone ne t)
    assert(t.clone == t)

    t = ConstVec3b(true)
    assert(t.clone eq t)
  }

  test("Factories") {
    var u: ReadVec3b = Vec3b(true)
    expectResult(classOf[Vec3b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }

    u = Vec3b(false)
    expectResult(classOf[Vec3b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }

    u = ConstVec3b(true)
    expectResult(classOf[ConstVec3b]) { u.getClass }
    expectResult(true) { u.x }
    expectResult(true) { u.y }
    expectResult(true) { u.z }

    u = ConstVec3b(false)
    expectResult(classOf[ConstVec3b]) { u.getClass }
    expectResult(false) { u.x }
    expectResult(false) { u.y }
    expectResult(false) { u.z }

    BooleanCombinations.test { (x, y, z, w) =>
      var u: ReadVec3b = Vec3b(x, y, z)
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec3b(x, y, z))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(x, Vec2b(y, z))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec2b(x, y), z)
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec4b(x, y, z, w))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec3i(toInt(x), toInt(y), toInt(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(x, Vec2i(toInt(y), toInt(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec2i(toInt(x), toInt(y)), z)
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(x, Vec2f(toFloat(y), toFloat(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec2f(toFloat(x), toFloat(y)), z)
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(x, Vec2d(toDouble(y), toDouble(z)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec2d(toDouble(x), toDouble(y)), z)
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      u = Vec3b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[Vec3b]) { u.getClass }
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }

      var c: ReadVec3b = ConstVec3b(x, y, z)
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(ConstVec3b(x, y, z))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(x, Vec2b(y, z))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec2b(x, y), z)
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec4b(x, y, z, w))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec3i(toInt(x), toInt(y), toInt(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(x, Vec2i(toInt(y), toInt(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec2i(toInt(x), toInt(y)), z)
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec4i(toInt(x), toInt(y), toInt(z), toInt(w)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec3f(toFloat(x), toFloat(y), toFloat(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(x, Vec2f(toFloat(y), toFloat(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec2f(toFloat(x), toFloat(y)), z)
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec4f(toFloat(x), toFloat(y), toFloat(z), toFloat(w)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec3d(toDouble(x), toDouble(y), toDouble(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(x, Vec2d(toDouble(y), toDouble(z)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec2d(toDouble(x), toDouble(y)), z)
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }

      c = ConstVec3b(Vec4d(toDouble(x), toDouble(y), toDouble(z), toDouble(w)))
      expectResult(classOf[ConstVec3b]) { c.getClass }
      expectResult(x) { c.x }
      expectResult(y) { c.y }
      expectResult(z) { c.z }
    }
  }

  test("Unapply") {
    BooleanCombinations.test { (x, y, z, w) =>
      Vec3b(x, y, z) match {
        case Vec3b(ux, uy, uz) =>
          if (ux != x || uy != y || uz != z)
            throw new AssertionError()
      }
      ConstVec3b(x, y, z) match {
        case Vec3b(ux, uy, uz) =>
          if (ux != x || uy != y || uz != z)
            throw new AssertionError()
      }
    }
  }

  test("Const conversions") {
    BooleanCombinations.test { (x, y, z, w) =>
      val t: ConstVec3b = Vec3b(x, y, z)
      expectResult(classOf[ConstVec3b]) { t.getClass }
      assert(Vec3b(x, y, z) == t)

      var c = ConstVec3b(true); val v = Vec3b(x, y, z)
      expectResult(classOf[Vec3b]) { v.getClass }
      c = v; assert(Vec3b(x, y, z) == c)
      expectResult(classOf[ConstVec3b]) { c.getClass }
    }
  }

  test("Equality methods") {
    BooleanCombinations.test { (x, y, z, w) =>
      val m = Vec3b(x, y, z)
      val c = ConstVec3b(x, y, z)

      assert(m == m)
      assert(m == c)
      assert(c == m)
      assert(c == c)

      assert(m.equals(c))
      assert(!m.equals(Nil))

      assert(Vec3b(x, y, z) != Vec3b(!x, y, z))
      assert(Vec3b(x, y, z) != Vec3b(x, !y, z))
      assert(Vec3b(x, y, z) != Vec3b(x, y, !z))

      assert(Vec3b(x, y, z) != Vec3i(toInt(x), toInt(y), toInt(z)))
      assert(Vec3b(x, y, z) != Vec3f(toFloat(x), toFloat(y), toFloat(z)))
      assert(Vec3b(x, y, z) != Vec3d(toDouble(x), toDouble(y), toDouble(z)))
    }
  }

  test("Indexed read") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = ConstVec3b(x, y, z)

      expectResult(x) { u(0) }
      expectResult(y) { u(1) }
      expectResult(z) { u(2) }

      intercept[IndexOutOfBoundsException] {
        u(3)
      }
      intercept[IndexOutOfBoundsException] {
        u(-1)
      }
    }
  }

  test("Indexed write") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec3b(x, y, z)

      u(0) = !x
      assert(Vec3b(!x, y, z) == u)

      u(1) = !y
      assert(Vec3b(!x, !y, z) == u)

      u(2) = !z
      assert(Vec3b(!x, !y, !z) == u)

      intercept[IndexOutOfBoundsException] {
        u(3) = true
      }
      intercept[IndexOutOfBoundsException] {
        u(-1) = true
      }
    }
  }

  test("Setters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec3b(!x)

      u := Vec3b(x, y, z)
      expectResult(x) { u.x }
      expectResult(y) { u.y }
      expectResult(z) { u.z }
    }
  }

  test("Getters") {
    BooleanCombinations.test { (x, y, z, w) =>
      val u = Vec3b(x, y, z)
      expectResult(x) { u.r }
      expectResult(y) { u.g }
      expectResult(z) { u.b }
    }
  }

}
