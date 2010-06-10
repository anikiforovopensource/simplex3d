/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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

import simplex3d.math.floatm.FloatMath._
import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ReadFactoryBench {
  def main(args: Array[String]) {
    new ReadFactory().run()
  }
}

class ReadFactory {
  val length = 20000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    newTest(length, loops)
    val newTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    factoryTest(length, loops)
    val factoryTime = System.currentTimeMillis - start

    println("Float. new time: " + newTime +
            ", factory time: " + factoryTime + ".")
  }

  def newTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val i4 = Vec4i(i, i + 1, i + 2, i + 3)
        val f4 = Vec4f(i4.x, i4.y, i4.z, i4.w)*1.1f
        val t1 = i4 % 10
        val t2 = Vec4b(bool(t1.x), bool(t1.y), bool(t1.z), bool(t1.w))
        val d4 = Vec4d(double(t2.x), double(t2.y),
                 double(t2.z), double(t2.w)) +
             Vec4d(f4.x, f4.y, f4.z, f4.w)*2.3 +
             Vec4d(i4.x, i4.y, i4.z, i4.w)*1.7
        answer += (int(d4.x + d4.y + d4.z + d4.w))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def factoryTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val i4 = Vec4i(i, i + 1, i + 2, i + 3)
        val f4 = Vec4f(i4)*1.1f
        val d4 = Vec4d(Vec4b(i4 % 10)) + Vec4d(f4)*2.3 + Vec4d(i4)*1.7
        answer += (int(d4.x + d4.y + d4.z + d4.w))

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
