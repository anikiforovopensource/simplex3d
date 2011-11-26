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
object FunBench {
  def main(args: Array[String]) {
    new FunCase().run()
  }
}

class FunCase {
  val length = 10000
  val loops = 100000
  
  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testAbstract(length, loops)
    val abstractTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testGeneric(length, loops)
    val genericTime = System.currentTimeMillis - start

    println("Generic time: " + genericTime +
            ", abstract time: " + abstractTime + ".")
  }

  val genericFun = (a: Int, b: Int, c: Int) => {
    (a + b)*c
  }

  private abstract class Fun {
    def apply(a: Int, b: Int, c: Int) :Int
  }
  private val abstractFun = new Fun { def apply(a: Int, b: Int, c: Int) = {
      (a + b)*c
  }}

  def testGeneric(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += genericFun(i, i + 1, i + 2)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testAbstract(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length - 1) {

        // Bench code
        answer += abstractFun(i, i + 1, i + 2)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

}
