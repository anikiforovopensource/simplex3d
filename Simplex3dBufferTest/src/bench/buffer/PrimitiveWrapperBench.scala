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
import simplex3d.math.floatm.FloatMath._
import simplex3d.buffer._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object PrimitiveWrapperBench {
  def main(args: Array[String]) {
    val tc = new PrimitiveWrapperBenchTC()
    tc.run()
  }
}

class PrimitiveWrapperBenchTC {
  val length = 20000
  val loops = 20000

  val random = new java.util.Random()

  val dataArray = new Array[Float](length)
  random.setSeed(1)
  for (i <- 0 until length) {
    dataArray(i) = random.nextFloat
  }

  val byteBuffer = ByteBuffer.allocateDirect(4*length).order(ByteOrder.nativeOrder)
  val dataBuffer = byteBuffer.asFloatBuffer
  random.setSeed(1)
  for (i <- 0 until length) {
    dataBuffer.put(i, random.nextFloat)
  }

  
  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testBuffer(dataBuffer, loops)
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testArray(dataArray, loops)
    val arrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRawWrapper(new ArrayWrapper1(dataArray), loops)
    val wrapperArray1 = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRawWrapper(new BufferWrapper1(dataBuffer), loops)
    val wrapperBuffer1 = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testImplementedFloat1(DataArray[Float1, RawFloat](dataArray), loops)
    val implementedArray1 = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedFloat1(DataBuffer[Float1, RawFloat](byteBuffer), loops)
    val implementedBuffer1 = System.currentTimeMillis - start

    println("Array time: " + arrayTime + ".")
    println("Buffer time: " + bufferTime + ".")

    println("Wrapper Array1 time: " + wrapperArray1 + ".")
    println("Wrapper Buffer1 time: " + wrapperBuffer1 + ".")

    println("Implemented Array1 time: " + implementedArray1 + ".")
    println("Implemented Buffer1 time: " + implementedBuffer1 + ".")
  }

  final def testArray(data: Array[Float], loops: Int) {
    var answer = 0
    val end = data.length
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = data(i)
        answer += int(v + 7.9f)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = data.get(i)
        answer += int(v + 7.9f)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testRawWrapper(seq: RawWrapper1, loops: Int) {
    var answer = 0
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        answer += int(v + 7.9f)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedFloat1(seq: DataSeq[Float1, _], loops: Int) {
    var answer = 0
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        answer += int(v + 7.9f)

        i += step
      }
      l += 1
    }

    println(answer)
  }
}

sealed abstract class RawWrapper1 {
  def size: Int
  def apply(i: Int) :Float
}
final class ArrayWrapper1(data: Array[Float]) extends RawWrapper1 {
  val size = data.length
  def apply(i: Int) = data(i)
}
final class BufferWrapper1(data: FloatBuffer) extends RawWrapper1 {
  val size = data.limit
  def apply(i: Int) = data.get(i)
}
