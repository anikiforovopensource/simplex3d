/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package bench.data


import java.nio._

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.floatx.functions._
import simplex3d.data._
import simplex3d.data.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object PrimitiveWrapperBench {

  def main(args: Array[String]) {
    test()
  }

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

  
  def test() {
    var start = 0L

    start = System.currentTimeMillis
    testBuffer(dataBuffer, loops)
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testArray(dataArray, loops)
    val arrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRawWrapper(new ArrayWrapper1(dataArray), loops)
    val wrapperArray = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testRawWrapper(new BufferWrapper1(dataBuffer), loops)
    val wrapperBuffer = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testImplementedRFloat(DataArray[RFloat, RFloat](dataArray), loops)
    val implementedArray = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedRFloat(DataBuffer[RFloat, RFloat](byteBuffer), loops)
    val implementedBuffer = System.currentTimeMillis - start

    println("Array time: " + arrayTime + ".")
    println("Buffer time: " + bufferTime + ".")

    println("Wrapper Array1 time: " + wrapperArray + ".")
    println("Wrapper Buffer1 time: " + wrapperBuffer + ".")

    println("Implemented Array1 time: " + implementedArray + ".")
    println("Implemented Buffer1 time: " + implementedBuffer + ".")
  }

  final def testArray(data: Array[Float], loops: Int) {
    var answer = 0
    val end = data.length
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = data(i)
        answer += (v + 7.9f).toInt

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
        answer += (v + 7.9f).toInt

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
        answer += (v + 7.9f).toInt

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedRFloat(seq: DataSeq[RFloat, _], loops: Int) {
    var answer = 0
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        answer += (v + 7.9f).toInt

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
