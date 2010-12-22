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
import simplex3d.math.floatm._
import simplex3d.data._
import simplex3d.data.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AccessorBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val size = 1000
  val loops = 10000
  val buffLoops = 10*1000*1000

  val data = new Array[Float](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < size) {
      data(i) = random.nextFloat
      i += 1
    }
  }

  val dataArray0 = DataArray[RFloat, RFloat](data)
  val dataBuffer0 = DataBuffer[RFloat, RFloat](data.size)
  dataBuffer0.put(dataArray0)
  val dataView0 = DataView[RFloat, RFloat](dataBuffer0, 0, 1)

  val dataArray1 = DataArray[RFloat, UByte](data.size)
  dataArray1.put(dataArray0)
  val dataBuffer1 = DataBuffer[RFloat, UByte](data.size)
  dataBuffer1.put(dataArray1)
  val dataView1 = DataView[RFloat, UByte](dataBuffer1, 0, 1)


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
    
    start = System.currentTimeMillis
    testRawBuffer(dataArray0, buffLoops)
    System.gc()
    val testRawBufferTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testBuffer(dataArray0, buffLoops)
    System.gc()
    val testBufferTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Simple time: " + testSimpleTime + ".")
    println("Inlined time: " + testInlinedTime + ".")
    println("rawBuffer time: " + testRawBufferTime + ".")
    println("buffer time: " + testBufferTime + ".")
  }

  final def testSimple(seq: DataSeq[RFloat, Raw], loops: Int) {
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

  final def testInlined(seq: DataSeq[RFloat, Raw], loops: Int) {
    var answer = 0.0
    val size = seq.size
    val offset = seq.offset
    val stride = seq.stride

    var l = 0; while (l < loops) {
      var i = offset; while (i < size) {
//      var i = seq.offset; while (i < seq.size) {
        answer += seq(i)

        i += stride
      }
      l += 1
    }

    println(answer)
  }
  
  final def testRawBuffer(seq: DataSeq[RFloat, Raw], loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      answer += seq.rawBuffer.capacity
      
      l += 1
    }

    println(answer)
  }
  
  final def testBuffer(seq: DataSeq[RFloat, Raw], loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      answer += seq.buffer.capacity

      l += 1
    }

    println(answer)
  }
}
