/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010, Simplex3d Team
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
object IntMathBench {

  private final val values = Array(6, 8, 100, 45, 490567, 92790, 2893472, 568353)

  // Implicit cast is eliminated by escape analysis.
  // Performance impact is small to moderate, depending on the size of the objects.
  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  val len = 1000*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testMax(len)
    System.gc()
    val maxTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testLessThan(len)
    System.gc()
    val lessThanTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Max time: " + maxTime + ".")
    println("LessThan time: " + lessThanTime + ".")
  }

  final def testMax(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val b = max(i, values(i & 0x7))
      a ^= b

      i += 1
    }

    println(a)
  }

  final def testLessThan(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val b = Vec3i(lessThan(Vec3i(i, i+1, i+2), Vec3i(values(i & 0x7), values((i+1) & 0x7), values((i+2) & 0x7))))
      a += b.x + b.y + b.z

      i += 1
    }

    println(a)
  }
}
