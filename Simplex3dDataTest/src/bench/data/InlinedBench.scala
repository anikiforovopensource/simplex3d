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
import simplex3d.math.floatm.FloatMath._
import simplex3d.data._
import simplex3d.data.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object InlinedBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val length = 20000
  val loops = 20000

  val dataArray = DataArray[RFloat, RFloat](length);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < length) {
      dataArray(i) = random.nextFloat
      i += 1
    }
  }

  val dataBuffer = DataBuffer[RFloat, RFloat](length)
  dataBuffer.put(dataArray)

  val converted = {
    val tmp = DataArray[RFloat, UByte](length)
    tmp.put(dataArray)
    tmp
  }

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testBuffer(dataBuffer.buffer(), loops)
    System.gc()
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testArray(dataArray.array, loops)
    System.gc()
    val arrayTime = System.currentTimeMillis - start

    
    start = System.currentTimeMillis
    testImplementedArray(DataArray[Vec4f, RFloat](dataArray), loops)
    System.gc()
    val implementedArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedBuffer(DataBuffer[Vec4f, RFloat](dataBuffer), loops)
    System.gc()
    val implementedBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedConversion(DataArray[Vec4f, UByte](converted), loops)
    System.gc()
    val implementedConversionTime = System.currentTimeMillis - start


    start = System.currentTimeMillis
    testInlinedArray(DataArray[Vec4f, RFloat](dataArray), loops)
    System.gc()
    val inlinedArrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInlinedBuffer(DataBuffer[Vec4f, RFloat](dataBuffer), loops)
    System.gc()
    val inlinedBufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInlinedConversion(DataArray[Vec4f, UByte](converted), loops)
    System.gc()
    val inlinedConversionTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Array time: " + arrayTime + ".")
    println("Buffer time: " + bufferTime + ".")
    println("Implemented Array time: " + implementedArrayTime + ".")
    println("Implemented Buffer time: " + implementedBufferTime + ".")
    println("Implemented Conversion time: " + implementedConversionTime + ".")
    println("Inlined Array time: " + inlinedArrayTime + ".")
    println("Inlined Buffer time: " + inlinedBufferTime + ".")
    println("Inlined Conversion time: " + inlinedConversionTime + ".")
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

  final def testImplementedArray(s: DataArray[Vec4f, RFloat], loops: Int) {
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

  final def testImplementedBuffer(s: DataBuffer[Vec4f, RFloat], loops: Int) {
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

  final def testInlinedArray(seq: DataArray[Vec4f, RFloat], loops: Int) {
    var answer = 0

    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        //val j = seq.offset + seq.stride*i
        val j = i*4
        val v = ConstVec4f(
          seq.backing(j), seq.backing(j + 1),
          seq.backing(j + 2), seq.backing(j + 3)
        )
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testInlinedBuffer(seq: DataBuffer[Vec4f, RFloat], loops: Int) {
    var answer = 0

    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        //val j = seq.offset + seq.stride*i
        val j = i*4
        val v = ConstVec4f(
          seq.backing(j), seq.backing(j + 1),
          seq.backing(j + 2), seq.backing(j + 3)
        )
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testInlinedConversion(s: DataSeq[Vec4f, UByte], loops: Int) {
    var answer = 0
    val seq = s
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val j = seq.offset + seq.stride*i
        val v = ConstVec4f(
          seq.backing(j), seq.backing(j + 1),
          seq.backing(j + 2), seq.backing(j + 3)
        )
        val u = v * 7.9f
        answer += int(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }
}
