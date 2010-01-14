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

package test.math

import org.scalatest._

import simplex3d.math._
import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
class BaseMathTest extends FunSuite {

    test("Random") {
        for (i <- 0 until 1000) {
           val random = new java.util.Random(i)
           val t = (
                      random.nextBoolean,
                      random.nextInt.asInstanceOf[Byte],
                      random.nextInt(10).asInstanceOf[Byte],
                      random.nextInt.asInstanceOf[Short],
                      random.nextInt(10).asInstanceOf[Short],
                      random.nextInt,
                      random.nextInt(10),
                      random.nextFloat,
                      random.nextDouble,
                      Vec2b(random.nextBoolean,
                            random.nextBoolean),
                      Vec3b(random.nextBoolean,
                            random.nextBoolean,
                            random.nextBoolean),
                      Vec4b(random.nextBoolean,
                            random.nextBoolean,
                            random.nextBoolean,
                            random.nextBoolean)
                    )

            setSeed(i)
            val m = (
                      nextBoolean,
                      nextByte,
                      nextByte(10),
                      nextShort,
                      nextShort(10),
                      nextInt,
                      nextInt(10),
                      nextFloat,
                      nextDouble,
                      nextVec2b,
                      nextVec3b,
                      nextVec4b
                    )

            assert(t == m)
        }
    }

    test("Casting") {
        val b: Byte = 1
        val s: Short = 1
        val i: Int = 1
        val l: Long = 1
        val f: Float = 1
        val d: Double = 1

        assert(byte(b).isInstanceOf[Byte])
        assert(byte(s).isInstanceOf[Byte])
        assert(byte(i).isInstanceOf[Byte])
        assert(byte(l).isInstanceOf[Byte])
        assert(byte(f).isInstanceOf[Byte])
        assert(byte(d).isInstanceOf[Byte])

        assert(short(b).isInstanceOf[Short])
        assert(short(s).isInstanceOf[Short])
        assert(short(i).isInstanceOf[Short])
        assert(short(l).isInstanceOf[Short])
        assert(short(f).isInstanceOf[Short])
        assert(short(d).isInstanceOf[Short])

        assert(int(b).isInstanceOf[Int])
        assert(int(s).isInstanceOf[Int])
        assert(int(i).isInstanceOf[Int])
        assert(int(l).isInstanceOf[Int])
        assert(int(f).isInstanceOf[Int])
        assert(int(d).isInstanceOf[Int])

        assert(long(b).isInstanceOf[Long])
        assert(long(s).isInstanceOf[Long])
        assert(long(i).isInstanceOf[Long])
        assert(long(l).isInstanceOf[Long])
        assert(long(f).isInstanceOf[Long])
        assert(long(d).isInstanceOf[Long])

        assert(float(b).isInstanceOf[Float])
        assert(float(s).isInstanceOf[Float])
        assert(float(i).isInstanceOf[Float])
        assert(float(l).isInstanceOf[Float])
        assert(float(f).isInstanceOf[Float])
        assert(float(d).isInstanceOf[Float])

        assert(double(b).isInstanceOf[Double])
        assert(double(s).isInstanceOf[Double])
        assert(double(i).isInstanceOf[Double])
        assert(double(l).isInstanceOf[Double])
        assert(double(f).isInstanceOf[Double])
        assert(double(d).isInstanceOf[Double])
    }

    test("Boolean functions") {
        val combinations = {
            (false, false, false, false) ::
            (false, false, false, true ) ::
            (false, false, true,  false) ::
            (false, false, true,  true ) ::
            (false, true,  false, false) ::
            (false, true,  false, true ) ::
            (false, true,  true,  false) ::
            (false, true,  true,  true ) ::
            (true,  false, false, false) ::
            (true,  false, false, true ) ::
            (true,  false, true,  false) ::
            (true,  false, true,  true ) ::
            (true,  true,  false, false) ::
            (true,  true,  false, true ) ::
            (true,  true,  true,  false) ::
            (true,  true,  true,  true ) ::
            Nil
        }

        def testCombination(x: Boolean, y: Boolean, z: Boolean, w: Boolean) {
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

        for ((x, y, z, w) <- combinations) {
            testCombination(x, y, z, w)
        }
    }
}
