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

package simplex3d.bench.scala


/**
 * @author Aleksey Nikiforov (lex)
 */
object DivMulBench {
  def main(args: Array[String]) {
    new DivMulCase().run()
  }
}

class DivMulCase {
  val length = 100000
  val loops = 10000
  
  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testFloatDiv(length, loops)
    val timeFloatDiv = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testIntDiv(length, loops)
    val timeIntDiv = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testFloatMul(length, loops)
    val timeFloatMul = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDoubleDiv(length, loops)
    val timeDoubleDiv = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDoubleMul(length, loops)
    val timeDoubleMul = System.currentTimeMillis - start

    println("FloatDiv: " + timeFloatDiv + ".")
    println("IntDiv: " + timeIntDiv + ".")
    println("FloatMul: " + timeFloatMul + ".")
    println("DoubleDiv: " + timeDoubleDiv + ".")
    println("DoubleMul: " + timeDoubleMul + ".")
  }

  def testIntDiv(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += i/9

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testFloatDiv(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += (i/32767f).asInstanceOf[Int]

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testFloatMul(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += (i*3.05185094759971922971e-5f).asInstanceOf[Int]

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testDoubleDiv(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += (i/32767d).asInstanceOf[Int]

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testDoubleMul(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += (i*3.05185094759971922971e-5).asInstanceOf[Int]

        i += 1
      }
      l += 1
    }

    println(answer)
  }

}
