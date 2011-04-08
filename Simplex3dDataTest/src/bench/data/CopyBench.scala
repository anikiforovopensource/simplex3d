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
object CopyBench {

  def main(args: Array[String]) {
    init()

    test()
    test()
    test()
  }

  val length = 100000
  val loops = 1000

  val random = new java.util.Random()

  val dataArray = new Array[Float](length)
  val byteBuffer = ByteBuffer.allocateDirect(4*length).order(ByteOrder.nativeOrder)
  val dataBuffer = byteBuffer.asFloatBuffer
  
  def init() {
    random.setSeed(1)
    for (i <- 0 until length) {
      dataArray(i) = random.nextFloat
    }

    random.setSeed(1)
    for (i <- 0 until length) {
      dataBuffer.put(i, random.nextFloat)
    }
  }


  def test() {
    var start = 0L

    start = System.currentTimeMillis
    testCopyBuffer(dataBuffer, loops)
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testCopyArray(dataArray, loops)
    val arrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testPutArray(dataArray, loops)
    val putArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testPutDataBuffer(byteBuffer, loops)
    val putDataBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInterleavedPutDataBuffer(byteBuffer, loops)
    val interleavedPutDataBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataBuffer(byteBuffer, loops)
    val noConvertPutDataBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataArray(dataArray, loops)
    val noConvertPutDataArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testNoConvertPutDataArray(dataArray, loops)
    val noConvertPutDataArrayTime2 = System.currentTimeMillis - start


    println("Array time: " + arrayTime + ".")
    println("Buffer time: " + bufferTime + ".")
    println("PutArray time: " + putArrayTime + ".")
    println("PutDataBuffer time: " + putDataBufferTime + ".")
    println("InterleavedPutDataBuffer time: " + interleavedPutDataBufferTime + ".")
    println("NoConvertPutDataBuffer time: " + noConvertPutDataBufferTime + ".")
    println("NoConvertPutDataArray time: " + noConvertPutDataArrayTime + ".")
    println("NoConvertPutDataArray time: " + noConvertPutDataArrayTime2 + ".")
  }

  def testCopyArray(data: Array[Float], loops: Int) {
    var answer = 0
    val dest = new Array[Float](data.size)

    var l = 0; while (l < loops) {
      System.arraycopy(data, 0, dest, 0, data.length)
      answer += Int(dest(l % data.length)*1000)

      l += 1
    }

    println(answer)
  }

  def testCopyBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val dest = ByteBuffer.allocateDirect(data.capacity*4).order(ByteOrder.nativeOrder).asFloatBuffer

    var l = 0; while (l < loops) {
      data.clear()
      dest.clear()
      dest.put(data)
      answer += Int(dest.get(l % data.capacity)*1000)

      l += 1
    }

    println(answer)
  }

  def testPutArray(data: Array[Float], loops: Int) {
    var answer = 0
    val dest = DataArray[RFloat, RFloat](data.size)

    var l = 0; while (l < loops) {
      dest.put(data)
      answer += Int(dest(l % data.length)*1000)

      l += 1
    }

    println(answer)
  }

  def testPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val dest = DataArray[RFloat, RFloat](size)
    val src = DataBuffer[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += Int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testInterleavedPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val offset = 1
    val stride = 2
    val dest = DataView[RFloat, RFloat](
      ByteBuffer.allocateDirect(
        size*4*stride + offset*4
      ),
      offset, stride
    )
    val src = DataBuffer[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += Int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testNoConvertPutDataBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val size = data.capacity/4
    val offset = 1
    val stride = 2
    val dest = DataView[RFloat, RFloat](
      ByteBuffer.allocateDirect(
        size*4*stride + offset*4
      ),
      offset, stride
    )
    val src = DataBuffer[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += Int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }

  def testNoConvertPutDataArray(data: Array[Float], loops: Int) {
    var answer = 0
    val size = data.length
    val offset = 1
    val stride = 2
    val dest = DataView[RFloat, RFloat](
      ByteBuffer.allocateDirect(
        size*4*stride + offset*4
      ),
      offset, stride
    )
    val src = DataArray[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += Int(dest(l % size)*1000)

      l += 1
    }

    println(answer)
  }
}
