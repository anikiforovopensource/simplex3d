/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
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
import simplex3d.math.floatm._
import simplex3d.buffer._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AbsAccessorBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val length = 1000
  val loops = 10000

  val random = new java.util.Random()
  val data = new Array[Float](length)
  random.setSeed(1)
  for (i <- 0 until length) {
    data(i) = random.nextFloat
  }

  val dataArray0 = DataArray[Float1, RawFloat](data)
  val dataBuffer0 = DataBuffer[Float1, RawFloat](data.length)
  dataBuffer0.put(dataArray0)
  val dataView0 = DataView[Float1, RawFloat](dataBuffer0, 0, 1)

  val dataArray1 = DataArray[Float1, UByte](data.length)
  dataArray1.put(dataArray0)
  val dataBuffer1 = DataBuffer[Float1, UByte](data.length)
  dataBuffer1.put(dataArray1)
  val dataView1 = DataView[Float1, UByte](dataBuffer1, 0, 1)


  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testSimple(dataArray0, loops)
    testSimple(dataBuffer0, loops)
    testSimple(dataView0, loops)
    testSimple(dataArray1, loops)
    testSimple(dataBuffer1, loops)
    testSimple(dataView1, loops)
    System.gc()
    val testSimpleTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInlined(dataArray0, loops)
    testInlined(dataBuffer0, loops)
    testInlined(dataView0, loops)
    testInlined(dataArray1, loops)
    testInlined(dataBuffer1, loops)
    testInlined(dataView1, loops)
    System.gc()
    val testInlinedTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Simple time: " + testSimpleTime + ".")
    println("Inlined time: " + testInlinedTime + ".")
  }

  final def testSimple(seq: DataSeq[Float1, RawData], loops: Int) {
    var answer = 0.0

    var l = 0; while (l < loops) {
      var i = seq.offset; while (i < seq.size) {
        answer += seq(i)

        i += seq.stride
      }
      l += 1
    }

    println(answer)
  }

  final def testInlined(seq: DataSeq[Float1, RawData], loops: Int) {
    var answer = 0.0
//    val size = seq.size
//    val offset = seq.offset
    val stride = seq.stride

    var l = 0; while (l < loops) {
      //var i = offset; while (i < size) {
      var i = seq.offset; while (i < seq.size) {
        answer += seq(i)

        i += stride
      }
      l += 1
    }

    println(answer)
  }
}
