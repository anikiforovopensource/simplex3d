/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
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

package bench.math

import simplex3d.math._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ExpandArgsBench {
    def main(args: Array[String]) {
        new ExpanArgs().run()
    }
}

class ExpanArgs {
    val length = 10000
    val loops = 200000
    
    val random = new scala.util.Random(1)
    def rd = random.nextDouble()
    val data = new Array[Vec4](length)
    for (i <- 0 until length) {
        data(i) = Vec4(rd, rd, rd, rd)
    }

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testPointExpand(length, loops)
        val pointExpandTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testPoint(length, loops)
        val pointTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testRegular(length, loops)
        val regularTime = System.currentTimeMillis - start

        println("Reg time: " + regularTime +
                ", point time: " + pointTime +
                ", pointExpand time: " + pointExpandTime + ".")
    }

    def testRegular(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 1) {

                // Bench code
                val b = reg_greaterThan(data(i), data(i + 1))
                if (b.x) answer += 1

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testPoint(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 1) {

                // Bench code
                val b = point_greaterThan(data(i), data(i + 1))
                if (b.x) answer += 1

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testPointExpand(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 1) {

                // Bench code
                val b = pointExpand_greaterThan(data(i), data(i + 1))
                if (b.x) answer += 1

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def reg_greaterThan(u: AnyVec4, v: AnyVec4) :Vec4b = {
        Vec4b(
            u.x > v.x,
            u.y > v.y,
            u.z > v.z,
            u.w > v.w
        )
    }

    def point_greaterThan(u: AnyVec4, v: AnyVec4) = {
        pointImpl_greaterThan(u, v, Vec4b(false))
    }

    def pointImpl_greaterThan(u: AnyVec4, v: AnyVec4, r: Vec4b) = {
        r.x = u.x > v.x
        r.y = u.y > v.y
        r.z = u.z > v.z
        r.w = u.w > v.w
        
        r
    }

    def pointExpand_greaterThan(u: AnyVec4, v: AnyVec4) = {
        pointExpandImpl_greaterThan(u.x, u.y, u.z, u.w,
                                    v.x, v.y, v.z, v.w,
                                    Vec4b(false))
    }

    def pointExpandImpl_greaterThan(
        x0: Double, y0: Double, z0: Double, w0: Double,
        x1: Double, y1: Double, z1: Double, w1: Double,
        r: Vec4b
    ) = {
        r.x = x0 > x1
        r.y = y0 > y1
        r.z = z0 > z1
        r.w = w0 > w1

        r
    }
}
