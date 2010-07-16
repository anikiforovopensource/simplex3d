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
object WrapperBench {
  def main(args: Array[String]) {
    //System.setProperty("simplex3d.buffer.optimize", "false")

    test()
    test()
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

  val convertedBytes = {
    val t = DataArray[Float1, UByte](length)
    t.put(dataArray)
    DataArray[Vec4f, UByte](t.array)
  }

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testBuffer(dataBuffer, loops)
    System.gc()
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testArray(dataArray, loops)
    System.gc()
    val arrayTime = System.currentTimeMillis - start

    
    start = System.currentTimeMillis
    testImplementedArray(dataArray, loops)
    System.gc()
    val implementedArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedBuffer(byteBuffer, loops)
    System.gc()
    val implementedBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedConversion(convertedBytes, loops)
    System.gc()
    val implementedConversionTime = System.currentTimeMillis - start


    // Choices
//    start = System.currentTimeMillis
//    testAbstractClassArray(dataArray, loops)
//    val absClassArrTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testGenericClassArray(dataArray, loops)
//    val genClassArrTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testGenericWrapperArray(dataArray, loops)
//    val genWrapperArrTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testAbstractClassBuffer(dataBuffer, loops)
//    val absClassBuffTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testAbstractInterleavedBuffer(dataBuffer, loops)
//    val absInterleavedTime = System.currentTimeMillis - start
//
//    start = System.currentTimeMillis
//    testGenericClassBuffer(dataBuffer, loops)
//    val genClassBufTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Array time: " + arrayTime + ".")
    println("Buffer time: " + bufferTime + ".")
    println("Implemented Array time: " + implementedArrayTime + ".")
    println("Implemented Buffer time: " + implementedBufferTime + ".")
    println("Implemented Conversion time: " + implementedConversionTime + ".")
//    println("Abstract class with Array time: " + absClassArrTime + ".")
//    println("Generic class with Array time: " + genClassArrTime + ".")
//    println("Generic Wrapper with Array time: " + genWrapperArrTime + ".")
//    println("Abstract class with Buffer time: " + absClassBuffTime + ".")
//    println("Abstract Interleaved with Buffer time: " + absInterleavedTime + ".")
//    println("Generic class with Buffer time: " + genClassBufTime + ".")
  }

  final def testArray(data: Array[Float], loops: Int) {
    var answer = 0
    val end = data.length - 3
    val step = 4

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = ConstVec4f(
          data(i), data(i + 1),
          data(i + 2), data(i + 3)
        )
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit - 3
    val step = 4

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = ConstVec4f(
          data.get(i), data.get(i + 1),
          data.get(i + 2), data.get(i + 3)
        )
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testAbstractClassArray(data: Array[Float], loops: Int) {
    var answer = 0
    val wrapper = new WrapperClass(new ArrayWrapper(data))
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testGenericClassArray(data: Array[Float], loops: Int) {
    var answer = 0
    val wrapper = new WrapperFunction(new ArrayWrapper(data), new ReadFactoryImpl)
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testAbstractClassBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val wrapper = new WrapperClass(new BufferWrapper(data))
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testGenericClassBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val wrapper = new WrapperFunction(new BufferWrapper(data), new ReadFactoryImpl)
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testAbstractInterleavedBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val wrapper = new InterleavedWrapper(new BufferWrapper(data), 0, 0)
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testGenericWrapperArray(data: Array[Float], loops: Int) {
    var answer = 0
    val wrapper :GenericWrapper[ConstVec4f] = new WrapperClass(new ArrayWrapper(data))
    val end = wrapper.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = wrapper(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedArray(data: Array[Float], loops: Int) {
    var answer = 0
    val seq = DataArray[Vec4f, RawFloat](data)
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedRoArray(data: Array[Float], loops: Int) {
    var answer = 0
    val seq = DataArray[Vec4f, RawFloat](data).asReadOnlySeq()
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val seq = DataBuffer[Vec4f, RawFloat](data)
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedConversion(s: DataSeq[Vec4f, UByte], loops: Int) {
    var answer = 0
    val seq = s
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }
}

abstract class RawWrapper {
  def size: Int
  def apply(i: Int) :Float
}
class ArrayWrapper(val data: Array[Float]) extends RawWrapper {
  val size = data.length
  def apply(i: Int) = data(i)
}
class BufferWrapper(val data: FloatBuffer) extends RawWrapper {
  val size = data.limit
  def apply(i: Int) = data.get(i)
}

abstract class GenericWrapper[T] {
  def size :Int
  def apply(i: Int) :T
}

class WrapperClass(val data: RawWrapper) extends GenericWrapper[ConstVec4f]{
  val size = data.size / 4
  def apply(i: Int) = {
    val j = i*4
    ConstVec4f(
      data(j), data(j + 1),
      data(j + 2), data(j + 3)
    )
  }
}

class InterleavedWrapper(val data: RawWrapper, offset: Int, stride: Int) {
  val components = 4
  val step = components + stride

  val size = data.size / 4
  def apply(i: Int) = {
    val j = i*step + offset
    ConstVec4f(
      data(j), data(j + 1),
      data(j + 2), data(j + 3)
    )
  }
}

abstract class ReadFactory[T] {
  def getFunction(data: RawWrapper) :ReadFunction[T]
}
final class ReadFactoryImpl extends ReadFactory[ConstVec4f] {
  def getFunction(data: RawWrapper) = new ReadFunctionImpl(data)
}
abstract class ReadFunction[T] {
  def apply(i: Int) :T
}
final class ReadFunctionImpl(val data: RawWrapper) extends ReadFunction[ConstVec4f] {
  def apply(i: Int) = ConstVec4f(
    data(i), data(i + 1),
    data(i + 2), data(i + 3)
  )
}

class WrapperFunction(
  val data: RawWrapper,
  val readFact: ReadFactory[ConstVec4f])
{
  private val read = readFact.getFunction(data)
  val size = data.size / 4
  def apply(i: Int) = read(i*4)
}
