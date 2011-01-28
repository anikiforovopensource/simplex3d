/*
 * Simplex3d, MathTest package
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

package test.math.doublex

import org.scalatest._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import scala.Double.{
  NaN => nan,
  PositiveInfinity => posinf,
  NegativeInfinity => neginf
}


/**
 * @author Aleksey Nikiforov (lex)
 */
class DoubleMathVec2Test extends FunSuite {

  val random = new java.util.Random
  import random._
  def randomDouble = random.nextDouble

  test("Vec2d numeric functions") {
    assert(all(isnan(Vec2(nan))))
    assert(!any(isnan(Vec2(neginf))))
    assert(!any(isnan(Vec2(posinf))))
    assert(!any(isnan(Vec2(0))))

    assert(!any(isinf(Vec2(nan))))
    assert(all(isinf(Vec2(neginf))))
    assert(all(isinf(Vec2(posinf))))
    assert(!any(isinf(Vec2(0))))

    assert(!any(isposinf(Vec2(nan))))
    assert(!any(isposinf(Vec2(neginf))))
    assert(all(isposinf(Vec2(posinf))))
    assert(!any(isposinf(Vec2(0))))

    assert(!any(isneginf(Vec2(nan))))
    assert(all(isneginf(Vec2(neginf))))
    assert(!any(isneginf(Vec2(posinf))))
    assert(!any(isneginf(Vec2(0))))

    {
      val u = Vec2(0)
      val i = Vec2(0)

      u := modf(Vec2(1.25, 2.125), i)
      assert(Vec2(0.25, 0.125) == u)
      assert(Vec2(1, 2) == i)

      u := modf(Vec2(-1.25, -2.125), i)
      assert(Vec2(-0.25, -0.125) == u)
      assert(Vec2(-1, -2) == i)

      u := modf(Vec2(nan, nan), i)
      assert(all(isnan(u)))
      assert(all(isnan(i)))

      u := modf(Vec2(posinf, posinf), i)
      assert(all(isnan(u)))
      assert(all(isposinf(i)))

      u := modf(Vec2(neginf, neginf), i)
      assert(all(isnan(u)))
      assert(all(isneginf(i)))
    }

    assert(sqrt(2) == length(Vec2(1, 1)))
    assert(5 == length(Vec2(-3, -4)))

    assert(2 == distance(Vec2(1, 1), Vec2(3, 1)))

    assert(11 == dot(Vec2(1, 2), Vec2(3, 4)))

    assert(Vec2(1/sqrt(2), 1/sqrt(2)) == normalize(Vec2(1, 1)))
    assert(Vec2(-1/sqrt(2), -1/sqrt(2)) == normalize(Vec2(-1, -1)))
    assert(Vec2(-1/sqrt(2), 1/sqrt(2)) == normalize(Vec2(-1, 1)))
    assert(Vec2(1/sqrt(2), -1/sqrt(2)) == normalize(Vec2(1, -1)))

    assert(Vec2(-2, -2) == faceforward(Vec2(2, 2), Vec2(0, 1), Vec2(1, 0)))
    assert(Vec2(-2, -2) == faceforward(Vec2(2, 2), Vec2(1, 0), Vec2(1, 0)))
    assert(Vec2(-2, -2) == faceforward(Vec2(2, 2), Vec2(1, 1), Vec2(1, 0)))
    assert(Vec2(2, 2) == faceforward(Vec2(2, 2), Vec2(-1, -1), Vec2(1, 0)))

    assert(Vec2(2, -2) == reflect(Vec2(2, 2), Vec2(0, 1)))
    assert(Vec2(-2, 2) == reflect(Vec2(-2, -2), Vec2(0, 1)))

    assert(approxEqual(
        Vec2(0.3, -0.9539392),
        refract(
          Vec2(1, 0),
          Vec2(0, 1),
          0.3
        ),
        1e-7
    ))

    assert(approxEqual(
        Vec2(0.21213204, -0.977241),
        refract(
          normalize(Vec2(1, 1)),
          Vec2(0, 1),
          0.3
        ),
        1e-7
    ))
  }

  test("Vec2d forward functions") {
    // test functions agnostic to range
    def testRange(
      x: Double, y: Double,
      r: Double, g: Double,
      s: Double, t: Double
    ) {
      expect(Vec2(radians(x), radians(y))) { radians(Vec2(x, y)) }
      expect(Vec2(degrees(x), degrees(y))) { degrees(Vec2(x, y)) }

      expect(Vec2(sin(x), sin(y))) { sin(Vec2(x, y)) }
      expect(Vec2(cos(x), cos(y))) { cos(Vec2(x, y)) }
      expect(Vec2(tan(x), tan(y))) { tan(Vec2(x, y)) }

      expect(Vec2(atan(x, r), atan(y, g))) {
        atan(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2(atan(x), atan(y))) { atan(Vec2(x, y)) }

      expect(Vec2(sinh(x), sinh(y))) { sinh(Vec2(x, y)) }
      expect(Vec2(cosh(x), cosh(y))) { cosh(Vec2(x, y)) }
      expect(Vec2(tanh(x), tanh(y))) { tanh(Vec2(x, y)) }

      expect(Vec2(asinh(x), asinh(y))) { asinh(Vec2(x, y)) }

      expect(Vec2(exp(x), exp(y))) { exp(Vec2(x, y)) }
      expect(Vec2(exp2(x), exp2(y))) { exp2(Vec2(x, y)) }

      expect(Vec2(abs(x), abs(y))) { abs(Vec2(x, y)) }
      expect(Vec2(sign(x), sign(y))) { sign(Vec2(x, y)) }
      expect(Vec2(floor(x), floor(y))) { floor(Vec2(x, y)) }
      expect(Vec2(trunc(x), trunc(y))) { trunc(Vec2(x, y)) }
      expect(Vec2(round(x), round(y))) { round(Vec2(x, y)) }
      expect(Vec2(roundEven(x), roundEven(y))) { roundEven(Vec2(x, y)) }
      expect(Vec2(ceil(x), ceil(y))) { ceil(Vec2(x, y)) }
      expect(Vec2(fract(x), fract(y))) { fract(Vec2(x, y)) }
      expect(Vec2(mod(x, s), mod(y, s))) { mod(Vec2(x, y), s) }
      expect(Vec2(mod(x, r), mod(y, g))) { mod(Vec2(x, y), Vec2(r, g)) }

      expect(Vec2(min(x, s), min(y, s))) { min(Vec2(x, y), s) }
      expect(Vec2(min(x, r), min(y, g))) {
        min(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2(max(x, s), max(y, s))) { max(Vec2(x, y), s) }
      expect(Vec2(max(x, r), max(y, g))) {
        max(Vec2(x, y), Vec2(r, g))
      }

      expect(Vec2(clamp(x, s, t), clamp(y, s, t))) {
        clamp(Vec2(x, y), s, t)
      }
      expect(Vec2(clamp(x, r, s), clamp(y, g, t))) {
        clamp(Vec2(x, y), Vec2(r, g), Vec2(s, t))
      }

      expect(Vec2(mix(x, r, s), mix(y, g, s))) {
        mix(Vec2(x, y), Vec2(r, g), s)
      }
      expect(Vec2(mix(x, r, s), mix(y, g, t))) {
        mix(Vec2(x, y), Vec2(r, g), Vec2(s, t))
      }
      val bool1 = s > 0; val bool2 = t > 0
      expect(Vec2(mix(x, r, Double(bool1)), mix(y, g, Double(bool2)))) {
        mix(Vec2(x, y), Vec2(r, g), Vec2b(bool1, bool2))
      }

      expect(Vec2(step(s, x), step(s, y))) { step(s, Vec2(x, y)) }
      expect(Vec2(step(x, r), step(y, g))) {
        step(Vec2(x, y), Vec2(r, g))
      }
      
      expect(Vec2(smoothstep(s, t, x), smoothstep(s, t, y))) {
        smoothstep(s, t, Vec2(x, y))
      }
      expect(Vec2(smoothstep(x, r, s), smoothstep(y, g, t))) {
        smoothstep(Vec2(x, y), Vec2(r, g), Vec2(s, t))
      }
    }
    setSeed(1)
    def doubleRange = (randomDouble*2 - 1)*1000
    for (i <- 0 until 1000) {
      testRange(
        doubleRange, doubleRange,
        doubleRange, doubleRange,
        doubleRange, doubleRange
      )
    }

    // range -1 to 1
    def test1(x: Double, y: Double) {
      expect(Vec2(asin(x), asin(y))) { asin(Vec2(x, y)) }
      expect(Vec2(acos(x), acos(y))) { acos(Vec2(x, y)) }

      expect(Vec2(atanh(x), atanh(y))) { atanh(Vec2(x, y)) }
    }
    setSeed(1)
    def double1 = (randomDouble*2 - 1)
    for (i <- 0 until 1000) {
      test1(double1, double1)
    }

    // test positive
    def testPositive(x: Double, y: Double, r: Double, g: Double) {
      expect(Vec2(acosh(x + 1), acosh(y + 1))) {
        acosh(Vec2(x + 1, y + 1))
      }

      expect(Vec2(pow(x, r), pow(y, g))) {
        pow(Vec2(x, y), Vec2(r, g))
      }

      expect(Vec2(log(x), log(y))) { log(Vec2(x, y)) }
      expect(Vec2(log2(x), log2(y))) { log2(Vec2(x, y)) }

      expect(Vec2(sqrt(x), sqrt(y))) { sqrt(Vec2(x, y)) }
      expect(Vec2(inversesqrt(x), inversesqrt(y))) {
        inversesqrt(Vec2(x, y))
      }
    }
    setSeed(1)
    def doublePos = randomDouble*1000
    for (i <- 0 until 1000) {
      testPositive(
        doublePos, doublePos,
        doublePos, doublePos
      )
    }
  }

  test("Vec2d boolean functions") {
    def test(x: Double, y: Double, r: Double, g: Double) {
      expect(Vec2b(x < r, y < g)) {
        lessThan(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2b(x <= r, y <= g)) {
        lessThanEqual(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2b(x > r, y > g)) {
        greaterThan(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2b(x >= r, y >= g)) {
        greaterThanEqual(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2b(x == r, y == g)) {
        equal(Vec2(x, y), Vec2(r, g))
      }
      expect(Vec2b(x != r, y != g)) {
        notEqual(Vec2(x, y), Vec2(r, g))
      }
    }

    setSeed(1)
    def doubleRange = (randomDouble*2 - 1)*1000
    for (i <- 0 until 1000) {
      test(
        doubleRange, doubleRange,
        doubleRange, doubleRange
      )
    }
  }
}
