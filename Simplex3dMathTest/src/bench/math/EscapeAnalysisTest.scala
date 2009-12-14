/*
 * Simplex3D, Math tests
 * Copyright (C) 2009 Simplex3D team
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.math


/**
 * @author Aleksey Nikiforov (lex)
 */
object EscapeBench {
    def main(args: Array[String]) {
        new EscapeBenchTest().run()
    }
}

class EscapeBenchTest {
    val length = 10000
    val loops = 100000
    
    val random = new scala.util.Random(1)
    def rd = random.nextDouble()
    val data = new Array[Vec4a](length)
    for (i <- 0 until length) {
        data(i) = new Vec4a(rd, rd, rd, rd)
    }

    def run() {
        var start = 0L

        start = System.currentTimeMillis
        testNew(length, loops)
        val makeNewTime = System.currentTimeMillis - start

        start = System.currentTimeMillis
        testTemp(length, loops)
        val tempTime = System.currentTimeMillis - start

        println("new time: " + makeNewTime +
                ", temp time: " + tempTime + ".")
    }

    def testNew(length: Int, loops: Int) {
        var answer = 0

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 2) {

                val t = data(i) + data(i + 1) + data(i + 2)
                answer += (t.x + t.y + t.z + t.w).asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

    def testTemp(length: Int, loops: Int) {
        var answer = 0
        val tmp = new Vec4a(0, 0, 0, 0)

        var l = 0; while (l < loops) {
            var i = 0; while (i < length - 2) {

                val t = data(i).add(data(i + 1), tmp).add(data(i + 2), tmp)
                answer += (t.x + t.y + t.z + t.w).asInstanceOf[Int]

                i += 1
            }
            l += 1
        }

        println(answer)
    }

}

class Vec4a(var x: Double, var y: Double, var z: Double, var w: Double) {
    def +(u: Vec4a) = new Vec4a(x + u.x, y + u.y, z + u.z, w + u.w)
    def add(u: Vec4a, r: Vec4a) = {
        r.x = x + u.x
        r.y = y + u.y
        r.z = z + u.z
        r.w = w + u.w
        r
    }
}
