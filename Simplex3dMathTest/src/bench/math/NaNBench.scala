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

package bench.math

import simplex3d.math._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NaNBench {
  def main(args: Array[String]) {
    new NanFloat().run()
  }
}

class NanFloat {
  val length = 100000
  val loops = 40000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testComp(length, loops)
    val compTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testIsnan(length, loops)
    val isnanTime = System.currentTimeMillis - start

    println("Float. comp time: " + compTime +
            ", isnan time: " + isnanTime + ".")
  }

  def getNan(i: Int) :Float = {
    if (i % 2 == 0) scala.Float.NaN
    else 1
  }

  def testComp(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val n = getNan(i)
        if (n != n) answer += 1
        else answer += 3

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testIsnan(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val n = getNan(i)
        if (isnan(n)) answer += 3
        else answer += 1

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
