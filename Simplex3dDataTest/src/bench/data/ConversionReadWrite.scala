/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.buffer


import java.nio._

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.data._
import simplex3d.data.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ConversionReadWrite {
  def main(args: Array[String]) {
    for (i <- 0 until 6) { test() }
  }

  val length = 20000
  val loops = 20000

  val dataArray = DataArray[RFloat, UByte](length);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < length) {
      dataArray(i) = random.nextFloat
      i += 1
    }
  }


  def test() {
    println("\nTesting...")
    var start = 0L
    
    start = System.currentTimeMillis
    testConversion(dataArray, loops)
    System.gc()
    val conversionRwTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Conversion rw time: " + conversionRwTime + ".")
  }

  final def testConversion(s: DataArray[RFloat, UByte], loops: Int) {
    var answer = 0
    val seq = s
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      val sign = (l % 2)*2 - 1
      var i = 0; while (i < end) {

        seq(i) = seq(i) + 0.0001f*sign
        

        i += step
      }
      l += 1
    }

    println(answer)
  }
}
