/*
 * Simplex3d, SimplexNoise test to ReferenceImpl
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package noise

import simplex3d.math.BaseMath
import simplex3d.math.doublem.renamed._

object Test {
    def main(args: Array[String]) {
        //test()
        new NoiseBench().run
    }

    def test() {
        val random = new java.util.Random(1)
        def r = (random.nextDouble - 0.5)*2e8

        for (i <- 0 until 1000000) {
            val v2 = Vec2(r, r)
            val v3 = Vec3(r, r, r)
            val v4 = Vec4(r, r, r, r)

//            assert(SimplexNoise1.noise1(v2) ==
//                   SimplexNoise.noise1(v2))
//
//            assert(SimplexNoise1.noise1(v3) ==
//                   SimplexNoise.noise1(v3))
//
//            assert(SimplexNoise1.noise1(v4) ==
//                   SimplexNoise.noise1(v4))
        }

        println("Test to reference: OK")
    }
}

class NoiseBench {
    val length = 100000
    val loops = 400

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testJavaPort(length, loops)
        val javaPortTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testGlslPort(length, loops)
        val glslPortTime = System.currentTimeMillis - start

        println("Java port time: " + javaPortTime + ".")
        println("Glsl port time: " + glslPortTime + ".")
    }

    def testJavaPort(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                var n = 0d
//                n += ext.toxi.math.noise.SimplexNoise.noise(i, i+1)
//                n += ext.toxi.math.noise.SimplexNoise.noise(i+2, i+3, i+4)
                n += ext.toxi.math.noise.SimplexNoise.noise(i+5, i+6, i+7, i+8)
                answer += BaseMath.int(n)

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testGlslPort(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length) {

                // Bench code
                var n = 0d
//                n += noise.SimplexNoise.noise(i, i+1)
//                n += noise.SimplexNoise.noise(i+2, i+3, i+4)
                n += noise.SimplexNoise.noise(i+5, i+6, i+7, i+8)
                answer += BaseMath.int(n)

                i += 1
            }
            l += 1
        }

        println(answer)
    }
}
