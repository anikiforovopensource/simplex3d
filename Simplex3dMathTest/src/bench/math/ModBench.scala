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

package bench.math

import simplex3d.math._
import simplex3d.math.floatx.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ModBench {
  def main(args: Array[String]) {
    new ModFloat().run()
  }
}

class ModFloat {
  val length = 10000
  val loops = 20000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testSimple(length, loops)
    val simpleTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAnother(length, loops)
    val anotherTime = System.currentTimeMillis - start

    println("Float. another time: " + anotherTime +
            ", normal time: " + simpleTime + ".")
  }

  def modSimple(x: Float, y: Float) :Float = {
    x - y*floor(x/y)
  }

  def modAnother(x: Float, y: Float) :Float = {
    if (x*y < 0) {
      y + x % y
    } else {
      x % y
    }
  }

  def testSimple(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        answer += toInt(modSimple(-i*123 + 0.1234f, -i + 0.2345f))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testAnother(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        answer += toInt(modAnother(-i*123 + 0.1234f, -i + 0.2345f))

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
