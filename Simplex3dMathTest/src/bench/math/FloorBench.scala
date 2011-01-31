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
import java.lang.StrictMath

/**
 * @author Aleksey Nikiforov (lex)
 */
object FloorBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val length = 10000
  val loops = 20000

  def test() {
    var start = 0L

    start = System.currentTimeMillis
    testOptimised(length, loops)
    val optimisedTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplemented(length, loops)
    val implementedTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testJava(length, loops)
    val javaTime = System.currentTimeMillis - start

    println("Optimised time: " + optimisedTime + ".")
    println("Implemented time: " + implementedTime + ".")
    println("System time: " + javaTime + ".")
  }

  final def floorOpt(x: Float) :Float = {
    if (x > Int.MaxValue || x < Int.MinValue) x
    else {
      val i = x.toInt
      if (x > 0 || x == i) i else if(java.lang.Float.isNaN(x)) x else i - 1
    }
  }

  def testJava(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        answer += (StrictMath.floor((-i + 0.12345f)*100)).toInt

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testOptimised(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        answer += (floorOpt((-i + 0.12345f)*100)).toInt

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testImplemented(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        answer += (floatx.functions.floor((-i + 0.12345f)*100)).toInt

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
