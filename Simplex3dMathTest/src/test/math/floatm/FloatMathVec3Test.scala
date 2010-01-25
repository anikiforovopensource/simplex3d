/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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

package test.math.floatm

import org.scalatest._

import simplex3d.math._
import simplex3d.math.BaseMath._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class FloatMathVec3Test extends FunSuite {

    val random = new java.util.Random
    import random._
    def randomFloat = random.nextFloat

    test("Vec3f forward functions") {
        // test functions agnostic to range
        def testRange(x: Float, y: Float, z: Float,
                      r: Float, g: Float, b: Float,
                      s: Float, t: Float, p: Float)
        {
            expect(Vec3(radians(x), radians(y), radians(z))) {
                radians(Vec3(x, y, z))
            }
            expect(Vec3(degrees(x), degrees(y), degrees(z))) {
                degrees(Vec3(x, y, z))
            }

            expect(Vec3(sin(x), sin(y), sin(z))) { sin(Vec3(x, y, z)) }
            expect(Vec3(cos(x), cos(y), cos(z))) { cos(Vec3(x, y, z)) }
            expect(Vec3(tan(x), tan(y), tan(z))) { tan(Vec3(x, y, z)) }

            expect(Vec3(atan(x, r), atan(y, g), atan(z, b))) {
                atan(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3(atan(x), atan(y), atan(z))) { atan(Vec3(x, y, z)) }

            expect(Vec3(sinh(x), sinh(y), sinh(z))) { sinh(Vec3(x, y, z)) }
            expect(Vec3(cosh(x), cosh(y), cosh(z))) { cosh(Vec3(x, y, z)) }
            expect(Vec3(tanh(x), tanh(y), tanh(z))) { tanh(Vec3(x, y, z)) }

            expect(Vec3(asinh(x), asinh(y), asinh(z))) { asinh(Vec3(x, y, z)) }

            expect(Vec3(exp(x), exp(y), exp(z))) { exp(Vec3(x, y, z)) }
            expect(Vec3(exp2(x), exp2(y), exp2(z))) { exp2(Vec3(x, y, z)) }

            expect(Vec3(abs(x), abs(y), abs(z))) { abs(Vec3(x, y, z)) }
            expect(Vec3(sign(x), sign(y), sign(z))) { sign(Vec3(x, y, z)) }
            expect(Vec3(floor(x), floor(y), floor(z))) { floor(Vec3(x, y, z)) }
            expect(Vec3(trunc(x), trunc(y), trunc(z))) { trunc(Vec3(x, y, z)) }
            expect(Vec3(round(x), round(y), round(z))) { round(Vec3(x, y, z)) }
            expect(Vec3(roundEven(x), roundEven(y), roundEven(z))) {
                roundEven(Vec3(x, y, z))
            }
            expect(Vec3(ceil(x), ceil(y), ceil(z))) { ceil(Vec3(x, y, z)) }
            expect(Vec3(fract(x), fract(y), fract(z))) { fract(Vec3(x, y, z)) }
            expect(Vec3(mod(x, s), mod(y, s), mod(z, s))) {
                mod(Vec3(x, y, z), s)
            }
            expect(Vec3(mod(x, r), mod(y, g), mod(z, b))) {
                mod(Vec3(x, y, z), Vec3(r, g, b))
            }

            expect(Vec3(min(x, s), min(y, s), min(z, s))) {
                min(Vec3(x, y, z), s)
            }
            expect(Vec3(min(x, r), min(y, g), min(z, b))) {
                min(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3(max(x, s), max(y, s), max(z, s))) {
                max(Vec3(x, y, z), s)
            }
            expect(Vec3(max(x, r), max(y, g), max(z, b))) {
                max(Vec3(x, y, z), Vec3(r, g, b))
            }

            expect(Vec3(clamp(x, s, t), clamp(y, s, t), clamp(z, s, t))) {
                clamp(Vec3(x, y, z), s, t)
            }
            expect(Vec3(clamp(x, r, s), clamp(y, g, t), clamp(z, b, p))) {
                clamp(Vec3(x, y, z), Vec3(r, g, b), Vec3(s, t, p))
            }

            expect(Vec3(mix(x, r, s), mix(y, g, s), mix(z, b, s))) {
                mix(Vec3(x, y, z), Vec3(r, g, b), s)
            }
            expect(Vec3(mix(x, r, s), mix(y, g, t), mix(z, b, p))) {
                mix(Vec3(x, y, z), Vec3(r, g, b), Vec3(s, t, p))
            }
            val bool1 = s > 0; val bool2 = t > 0; val bool3 = p > 0
            expect(Vec3(mix(x, r, float(bool1)),
                        mix(y, g, float(bool2)),
                        mix(z, b, float(bool3))))
            {
                mix(Vec3(x, y, z), Vec3(r, g, b), Vec3b(bool1, bool2, bool3))
            }

            expect(Vec3(step(s, x), step(s, y), step(s, z))) {
                step(s, Vec3(x, y, z))
            }
            expect(Vec3(step(x, r), step(y, g), step(z, b))) {
                step(Vec3(x, y, z), Vec3(r, g, b))
            }
            
            expect(Vec3(smoothstep(s, t, x),
                        smoothstep(s, t, y),
                        smoothstep(s, t, z)))
            {
                smoothstep(s, t, Vec3(x, y, z))
            }
            expect(Vec3(smoothstep(x, r, s),
                        smoothstep(y, g, t),
                        smoothstep(z, b, p)))
            {
                smoothstep(Vec3(x, y, z), Vec3(r, g, b), Vec3(s, t, p))
            }
        }
        setSeed(1)
        def floatRange = (randomFloat*2 - 1)*1000
        for (i <- 0 until 1000) {
            testRange(floatRange, floatRange, floatRange,
                      floatRange, floatRange, floatRange,
                      floatRange, floatRange, floatRange)
        }

        // range -1 to 1
        def test1(x: Float, y: Float, z: Float) {
            expect(Vec3(asin(x), asin(y), asin(z))) { asin(Vec3(x, y, z)) }
            expect(Vec3(acos(x), acos(y), acos(z))) { acos(Vec3(x, y, z)) }

            expect(Vec3(atanh(x), atanh(y), atanh(z))) { atanh(Vec3(x, y, z)) }
        }
        setSeed(1)
        def float1 = (randomFloat*2 - 1)
        for (i <- 0 until 1000) {
            test1(float1, float1, float1)
        }

        // test positive
        def testPositive(x: Float, y: Float, z: Float,
                         r: Float, g: Float, b: Float)
        {
            expect(Vec3(acosh(x + 1), acosh(y + 1), acosh(z + 1))) {
                acosh(Vec3(x + 1, y + 1, z + 1))
            }

            expect(Vec3(pow(x, r), pow(y, g), pow(z, b))) {
                pow(Vec3(x, y, z), Vec3(r, g, b))
            }

            expect(Vec3(log(x), log(y), log(z))) { log(Vec3(x, y, z)) }
            expect(Vec3(log2(x), log2(y), log2(z))) { log2(Vec3(x, y, z)) }

            expect(Vec3(sqrt(x), sqrt(y), sqrt(z))) { sqrt(Vec3(x, y, z)) }
            expect(Vec3(inversesqrt(x), inversesqrt(y), inversesqrt(z))) {
                inversesqrt(Vec3(x, y, z))
            }
        }
        setSeed(1)
        def floatPos = randomFloat*1000
        for (i <- 0 until 1000) {
            testPositive(floatPos, floatPos, floatPos,
                         floatPos, floatPos, floatPos)
        }
    }

    test("Vec3f boolean functions") {
        def test(x: Float, y: Float, z: Float, r: Float, g: Float, b: Float) {
            expect(Vec3b(x < r, y < g, z < b)) {
                lessThan(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3b(x <= r, y <= g, z <= b)) {
                lessThanEqual(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3b(x > r, y > g, z > b)) {
                greaterThan(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3b(x >= r, y >= g, z >= b)) {
                greaterThanEqual(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3b(x == r, y == g, z == b)) {
                equal(Vec3(x, y, z), Vec3(r, g, b))
            }
            expect(Vec3b(x != r, y != g, z != b)) {
                notEqual(Vec3(x, y, z), Vec3(r, g, b))
            }
        }

        setSeed(1)
        def floatRange = (randomFloat*2 - 1)*1000
        for (i <- 0 until 1000) {
            test(floatRange, floatRange, floatRange,
                 floatRange, floatRange, floatRange)
        }
    }
}
