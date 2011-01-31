/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MinBench {

  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  val loops = 100*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testJava(loops)
    val javaTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedDouble(loops)
    val implementedDoubleTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Implemented Double time: " + implementedDoubleTime + ".")
    println("java.Math time: " + javaTime + ".")
  }

  final def testImplementedDouble(loops: Int) {
    var a = 0

    var i = -loops; while (i < loops) {
      a += min(i*1.111, 1000 + i).toInt

      i += 1
    }

    println(a)
  }

  final def testJava(loops: Int) {
    var a = 0

    var i = -loops; while (i < loops) {
      a += java.lang.Math.min(i*1.111, 1000 + i).toInt

      i += 1
    }

    println(a)
  }
}
