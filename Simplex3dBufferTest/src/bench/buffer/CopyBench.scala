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
object CopyBench {
  def main(args: Array[String]) {
    new CopyBenchTC().run()
  }
}

class CopyBenchTC {
  val length = 100000
  val loops = 10000

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

//    start = System.currentTimeMillis
//    testCopyBuffer(dataBuffer, loops)
//    val bufferTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testCopyArray(dataArray, loops)
//    val arrayTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testPutArray(dataArray, loops)
//    val putArrayTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testPutDataBuffer(byteBuffer, loops)
//    val putDataBufferTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testInterleavedPutDataBuffer(byteBuffer, loops)
//    val interleavedPutDataBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataBuffer(byteBuffer, loops)
    val noConvertPutDataBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataArray(dataArray, loops)
    val noConvertPutDataArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataArray(dataArray, loops)
    val noConvertPutDataArrayTime2 = System.currentTimeMillis - start


//    println("Array time: " + arrayTime + ".")
//    println("Buffer time: " + bufferTime + ".")
//    println("PutArray time: " + putArrayTime + ".")
//    println("PutDataBuffer time: " + putDataBufferTime + ".")
//    println("InterleavedPutDataBuffer time: " + interleavedPutDataBufferTime + ".")
    println("NoConvertPutDataBuffer time: " + noConvertPutDataBufferTime + ".")
    println("NoConvertPutDataArray time: " + noConvertPutDataArrayTime + ".")
    println("NoConvertPutDataArray time: " + noConvertPutDataArrayTime2 + ".")
  }

  def testCopyArray(data: Array[Float], loops: Int) {
    var answer = 0
    val dest = new Array[Float](data.size)

    var l = 0; while (l < loops) {
      System.arraycopy(data, 0, dest, 0, data.length)
      answer += int(dest(l % data.length)*1000)

      l += 1
    }

    println(answer)
  }

  def testCopyBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val dest = allocateDirectBuffer(data.capacity*4).asFloatBuffer

    var l = 0; while (l < loops) {
      data.clear()
      dest.clear()
      dest.put(data)
      answer += int(dest.get(l % data.capacity)*1000)

      l += 1
    }

    println(answer)
  }

  def testPutArray(data: Array[Float], loops: Int) {
    var answer = 0
    val dest = DataArray[Float1, RawFloat](data.size)

    var l = 0; while (l < loops) {
      dest.put(data)
      answer += int(dest(l % data.length)*1000)

      l += 1
    }

    println(answer)
  }

  def testPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val dest = DataArray[Float1, RawFloat](size)
    val src = DataBuffer[Float1, RawFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testInterleavedPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val offset = 2
    val stride = 2
    val dest = DataView[Float1, RawFloat](
      allocateDirectBuffer(
        size*4*(stride + 1) + offset*4
      ),
      offset, stride
    )
    val src = DataBuffer[Float1, RawFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testNoConvertPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val offset = 2
    val stride = 2
    val dest = DataView[Float1, RawFloat](
      allocateDirectBuffer(
        size*4*(stride + 1) + offset*4
      ),
      offset, stride
    )
    val src = DataBuffer[Float1, RawFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testNoConvertPutDataArray(data: Array[Float], loops: Int) {
    var answer = 0
    val size = data.length
    val offset = 2
    val stride = 2
    val dest = DataView[Float1, RawFloat](
      allocateDirectBuffer(
        size*4*(stride + 1) + offset*4
      ),
      offset, stride
    )
    val src = DataArray[Float1, RawFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }
}
