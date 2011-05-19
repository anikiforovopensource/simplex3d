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
import simplex3d.math.floatx.{functions => fx}
import simplex3d.math.doublex.{functions => dx}


/**
 * @author Aleksey Nikiforov (lex)
 */
object RoundBench {

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
    testImplementedFloat(loops)
    val implementedFloatTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedDouble(loops)
    val implementedDoubleTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testJava(loops)
    val javaTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Implemented Float time: " + implementedFloatTime + ".")
    println("Implemented Double time: " + implementedDoubleTime + ".")
    println("java.Math time: " + javaTime + ".")
  }

  final def testImplementedFloat(loops: Int) {
    var a = 0

    var i = -loops; while (i < loops) {
      a += fx.round(i*1.111f).toInt

      i += 1
    }

    println(a)
  }

  final def testImplementedDouble(loops: Int) {
    var a = 0

    var i = -loops; while (i < loops) {
      a += dx.round(i*1.111).toInt

      i += 1
    }

    println(a)
  }

  final def testJava(loops: Int) {
    var a = 0

    var i = -loops; while (i < loops) {
      a += math.round(i*1.111).toInt

      i += 1
    }

    println(a)
  }

  // StrictMath.rint is faster
  private final def roundEven(x: Float) :Float = {
    if (x > scala.Int.MaxValue || x < scala.Int.MinValue) return x

    val i = x.toInt
    val h = x + 0.5f

    if (h == i + 1) i + (i & 1)
    else if (h == i) i - (i & 1)
    else if (x >= 0) (x + 0.5f).toInt
    else if (x < 0) (x - 0.5f).toInt
    else x
  }
}
