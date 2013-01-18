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

package simplex3d.test.math

import org.scalatest._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
class CommonMathTest extends FunSuite {

  test("Casting") {
    val b0: Boolean = false
    val i0: Int = 0
    val f0: Float = 0
    val d0: Double = 0

    val b1: Boolean = true
    val i1: Int = 1
    val f1: Float = 1
    val d1: Double = 1

    val i2: Int = 2
    val f2: Float = 2.1f
    val d2: Double = 2.1

    val i3: Int = -1
    val f3: Float = -1
    val d3: Double = -1

    assert(toBoolean(b1).isInstanceOf[Boolean])
    assert(toBoolean(i1).isInstanceOf[Boolean])
    assert(toBoolean(f1).isInstanceOf[Boolean])
    assert(toBoolean(d1).isInstanceOf[Boolean])

    assert(toBoolean(b1))
    assert(toBoolean(i1))
    assert(toBoolean(f1))
    assert(toBoolean(d1))

    assert(toBoolean(i2))
    assert(toBoolean(f2))
    assert(toBoolean(d2))

    assert(toBoolean(i3))
    assert(toBoolean(f3))
    assert(toBoolean(d3))

    assert(!toBoolean(b0))
    assert(!toBoolean(i0))
    assert(!toBoolean(f0))
    assert(!toBoolean(d0))

    assert(toInt(b1).isInstanceOf[Int])
    assert(toInt(i1).isInstanceOf[Int])
    assert(toInt(f1).isInstanceOf[Int])
    assert(toInt(d1).isInstanceOf[Int])

    assert(toInt(b1) == 1)
    assert(toInt(i1) == 1)
    assert(toInt(f1) == 1)
    assert(toInt(d1) == 1)

    assert(toInt(b0) == 0)
    assert(toInt(i0) == 0)
    assert(toInt(f0) == 0)
    assert(toInt(d0) == 0)

    assert(toFloat(b1).isInstanceOf[Float])
    assert(toFloat(i1).isInstanceOf[Float])
    assert(toFloat(f1).isInstanceOf[Float])
    assert(toFloat(d1).isInstanceOf[Float])

    assert(toFloat(b1) == 1f)
    assert(toFloat(i1) == 1f)
    assert(toFloat(f1) == 1f)
    assert(toFloat(d1) == 1f)

    assert(toFloat(b0) == 0f)
    assert(toFloat(i0) == 0f)
    assert(toFloat(f0) == 0f)
    assert(toFloat(d0) == 0f)

    assert(toDouble(b1).isInstanceOf[Double])
    assert(toDouble(i1).isInstanceOf[Double])
    assert(toDouble(f1).isInstanceOf[Double])
    assert(toDouble(d1).isInstanceOf[Double])

    assert(toDouble(b1) == 1d)
    assert(toDouble(i1) == 1d)
    assert(toDouble(f1) == 1d)
    assert(toDouble(d1) == 1d)

    assert(toDouble(b0) == 0d)
    assert(toDouble(i0) == 0d)
    assert(toDouble(f0) == 0d)
    assert(toDouble(d0) == 0d)


    // Vectors
    assert(!toBoolean(Vec2b(false, false)))
    assert(!toBoolean(Vec2b(false, true)))
    assert(toBoolean(Vec2b(true, false)))
    assert(toBoolean(Vec2b(true, true)))

    assert(!toBoolean(Vec2i(0, 0)))
    assert(!toBoolean(Vec2i(0, 1)))
    assert(toBoolean(Vec2i(1, 0)))
    assert(toBoolean(Vec2i(1, 1)))

    assert(!toBoolean(Vec2f(0, 0)))
    assert(!toBoolean(Vec2f(0, 1)))
    assert(toBoolean(Vec2f(1, 0)))
    assert(toBoolean(Vec2f(1, 1)))

    assert(!toBoolean(Vec2d(0, 0)))
    assert(!toBoolean(Vec2d(0, 1)))
    assert(toBoolean(Vec2d(1, 0)))
    assert(toBoolean(Vec2d(1, 1)))


    assert(toInt(Vec2b(false, false)) == 0)
    assert(toInt(Vec2b(false, true)) == 0)
    assert(toInt(Vec2b(true, false)) == 1)
    assert(toInt(Vec2b(true, true)) == 1)

    assert(toInt(Vec2i(7, 8)) == 7)
    assert(toInt(Vec2f(7.5f, 8.5f)) == 7)
    assert(toInt(Vec2d(7.5, 8.5)) == 7)


    assert(toFloat(Vec2b(false, false)) == 0f)
    assert(toFloat(Vec2b(false, true)) == 0f)
    assert(toFloat(Vec2b(true, false)) == 1f)
    assert(toFloat(Vec2b(true, true)) == 1f)

    assert(toFloat(Vec2i(7, 8)) == 7f)
    assert(toFloat(Vec2f(7.5f, 8.5f)) == 7.5f)
    assert(toFloat(Vec2d(7.5, 8.5)) == 7.5f)


    assert(toDouble(Vec2b(false, false)) == 0d)
    assert(toDouble(Vec2b(false, true)) == 0d)
    assert(toDouble(Vec2b(true, false)) == 1d)
    assert(toDouble(Vec2b(true, true)) == 1d)

    assert(toDouble(Vec2i(7, 8)) == 7d)
    assert(toDouble(Vec2f(7.5f, 8.5f)) == 7.5)
    assert(toDouble(Vec2d(7.5, 8.5)) == 7.5)
  }

  test("Boolean functions") {
    BooleanCombinations.test { (x, y, z, w) =>
      assert(any(Vec2b(x, y)) == (x || y))
      assert(all(Vec2b(x, y)) == (x && y))
      assert(not(Vec2b(x, y)) == Vec2b(!x, !y))

      assert(any(Vec3b(x, y, z)) == (x || y || z))
      assert(all(Vec3b(x, y, z)) == (x && y && z))
      assert(not(Vec3b(x, y, z)) == Vec3b(!x, !y, !z))

      assert(any(Vec4b(x, y, z, w)) == (x || y || z || w))
      assert(all(Vec4b(x, y, z, w)) == (x && y && z && w))
      assert(not(Vec4b(x, y, z, w)) == Vec4b(!x, !y, !z, !w))
    }
  }


  val random = new java.util.Random
  import random._

  test("Int functions") {
    expectResult(0) { abs(0) }
    expectResult(1) { abs(-1) }
    expectResult(1) { abs(1) }

    expectResult(0) { sign(0) }
    expectResult(-1){ sign(-1)}
    expectResult(1) { sign(1) }

    expectResult(2) { min(2, 3) }
    expectResult(3) { min(4, 3) }

    expectResult(3) { max(2, 3) }
    expectResult(4) { max(4, 3) }

    expectResult(10) { clamp(5, 10, 20) }
    expectResult(15) { clamp(15, 10, 20) }
    expectResult(20) { clamp(25, 10, 20) }
  }

  val testData = {
    (0, 0, 0, 0,
     0, 0, 0, 0,
     0, 0, 0, 0) ::
    (-1, -1, -1, -1,
     0, 0, 0, 0,
     0, 0, 0, 0) ::
    (1, 1, 1, 1,
     0, 0, 0, 0,
     0, 0, 0, 0) ::
    (5, 5, 5, 5,
     10, 10, 10, 10,
     20, 20, 20, 20) ::
    (10, 10, 10, 10,
     10, 10, 10, 10,
     20, 20, 20, 20) ::
    (15, 15, 15, 15,
     10, 10, 10, 10,
     20, 20, 20, 20) ::
    (20, 20, 20, 20,
     10, 10, 10, 10,
     20, 20, 20, 20) ::
    (25, 25, 25, 25,
     10, 10, 10, 10,
     20, 20, 20, 20) ::
    Nil
  }

  test("Vec2i functions") {
    def test(x: Int, y: Int, r: Int, g: Int, s: Int, t: Int) {
      expectResult(Vec2i(abs(x), abs(y))) { abs(Vec2i(x, y)) }
      expectResult(Vec2i(sign(x), sign(y))) { sign(Vec2i(x, y)) }

      expectResult(Vec2i(min(x, s), min(y, s))) { min(Vec2i(x, y), s) }
      expectResult(Vec2i(min(x, r), min(y, g))) {
        min(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2i(max(x, s), max(y, s))) { max(Vec2i(x, y), s) }
      expectResult(Vec2i(max(x, r), max(y, g))) {
        max(Vec2i(x, y), Vec2i(r, g))
      }

      expectResult(Vec2i(clamp(x, s, t), clamp(y, s, t))) {
        clamp(Vec2i(x, y), s, t)
      }
      expectResult(Vec2i(clamp(x, r, s), clamp(y, g, t))) {
        clamp(Vec2i(x, y), Vec2i(r, g), Vec2i(s, t))
      }

      expectResult(Vec2b(x < r, y < g)) {
        lessThan(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2b(x <= r, y <= g)) {
        lessThanEqual(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2b(x > r, y > g)) {
        greaterThan(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2b(x >= r, y >= g)) {
        greaterThanEqual(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2b(x == r, y == g)) {
        equal(Vec2i(x, y), Vec2i(r, g))
      }
      expectResult(Vec2b(x != r, y != g)) {
        notEqual(Vec2i(x, y), Vec2i(r, g))
      }
    }

    for ((x, y, _, _, r, g, _, _, s, t, _, _) <- testData) {
      test(x, y,
         r, g,
         s, t)
    }

    setSeed(1)
    for (i <- 0 until 1000) {
      test(nextInt, nextInt,
         nextInt, nextInt,
         nextInt, nextInt)
    }
  }

  test("Vec3i functions") {
    def test(x: Int, y: Int, z: Int,
         r: Int, g: Int, b: Int,
         s: Int, t: Int, p: Int)
    {
      expectResult(Vec3i(abs(x), abs(y), abs(z))) {
        abs(Vec3i(x, y, z))
      }
      expectResult(Vec3i(sign(x), sign(y), sign(z))) {
        sign(Vec3i(x, y, z))
      }

      expectResult(Vec3i(min(x, s), min(y, s), min(z, s))) {
        min(Vec3i(x, y, z), s)
      }
      expectResult(Vec3i(min(x, r), min(y, g), min(z, b))) {
        min(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3i(max(x, s), max(y, s), max(z, s))) {
        max(Vec3i(x, y, z), s)
      }
      expectResult(Vec3i(max(x, r), max(y, g), max(z, b))) {
        max(Vec3i(x, y, z), Vec3i(r, g, b))
      }

      expectResult(Vec3i(clamp(x, s, t), clamp(y, s, t), clamp(z, s, t))) {
        clamp(Vec3i(x, y, z), s, t)
      }
      expectResult(Vec3i(clamp(x, r, s), clamp(y, g, t), clamp(z, b, p))) {
        clamp(Vec3i(x, y, z), Vec3i(r, g, b), Vec3i(s, t, p))
      }

      expectResult(Vec3b(x < r, y < g, z < b)) {
        lessThan(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3b(x <= r, y <= g, z <= b)) {
        lessThanEqual(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3b(x > r, y > g, z > b)) {
        greaterThan(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3b(x >= r, y >= g, z >= b)) {
        greaterThanEqual(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3b(x == r, y == g, z == b)) {
        equal(Vec3i(x, y, z), Vec3i(r, g, b))
      }
      expectResult(Vec3b(x != r, y != g, z != b)) {
        notEqual(Vec3i(x, y, z), Vec3i(r, g, b))
      }
    }

    for ((x, y, z, _, r, g, b, _, s, t, p, _) <- testData) {
      test(x, y, z,
         r, g, b,
         s, t, p)
    }

    setSeed(1)
    for (i <- 0 until 1000) {
      test(nextInt, nextInt, nextInt,
         nextInt, nextInt, nextInt,
         nextInt, nextInt, nextInt)
    }
  }

  test("Vec4i functions") {
    def test(x: Int, y: Int, z: Int, w: Int,
         r: Int, g: Int, b: Int, a: Int,
         s: Int, t: Int, p: Int, q: Int)
    {
      expectResult(Vec4i(abs(x), abs(y), abs(z), abs(w))) {
        abs(Vec4i(x, y, z, w))
      }
      expectResult(Vec4i(sign(x), sign(y), sign(z), sign(w))) {
        sign(Vec4i(x, y, z, w))
      }

      expectResult(Vec4i(min(x, s), min(y, s), min(z, s), min(w, s))) {
        min(Vec4i(x, y, z, w), s)
      }
      expectResult(Vec4i(min(x, r), min(y, g), min(z, b), min(w, a))) {
        min(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4i(max(x, s), max(y, s), max(z, s), max(w, s))) {
        max(Vec4i(x, y, z, w), s)
      }
      expectResult(Vec4i(max(x, r), max(y, g), max(z, b), max(w, a))) {
        max(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }

      expectResult(Vec4i(clamp(x, s, t), clamp(y, s, t),
             clamp(z, s, t), clamp(w, s, t)))
      {
        clamp(Vec4i(x, y, z, w), s, t)
      }
      expectResult(Vec4i(clamp(x, r, s), clamp(y, g, t),
             clamp(z, b, p), clamp(w, a, q)))
      {
        clamp(Vec4i(x, y, z, w), Vec4i(r, g, b, a), Vec4i(s, t, p, q))
      }

      expectResult(Vec4b(x < r, y < g, z < b, w < a)) {
        lessThan(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4b(x <= r, y <= g, z <= b, w <= a)) {
        lessThanEqual(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4b(x > r, y > g, z > b, w > a)) {
        greaterThan(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4b(x >= r, y >= g, z >= b, w >= a)) {
        greaterThanEqual(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4b(x == r, y == g, z == b, w == a)) {
        equal(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
      expectResult(Vec4b(x != r, y != g, z != b, w != a)) {
        notEqual(Vec4i(x, y, z, w), Vec4i(r, g, b, a))
      }
    }

    for ((x, y, z, w, r, g, b, a, s, t, p, q) <- testData) {
      test(x, y, z, w,
         r, g, b, a,
         s, t, p, q)
    }

    setSeed(1)
    for (i <- 0 until 1000) {
      test(nextInt, nextInt, nextInt, nextInt,
         nextInt, nextInt, nextInt, nextInt,
         nextInt, nextInt, nextInt, nextInt)
    }
  }
}
