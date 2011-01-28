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

package bench.scala

import java.nio._

import simplex3d.math._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ShiftMultBench {
  def main(args: Array[String]) {
    run()
    run()
    run()
  }

  val mult = 4
  val shift = 2

  val length = 1000000000
  val loops = 1

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testShift(length, loops)
    System.gc
    val shiftTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMult(length, loops)
    System.gc
    val multTime = System.currentTimeMillis - start

    println("Shift time: " + shiftTime + ".")
    println("Mult time: " + multTime + ".")
  }

  def testShift(length: Int, loops: Int) {
    var answer = 0
    
    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        answer |= l + (i << shift)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testMult(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        answer |= l + i*mult

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
