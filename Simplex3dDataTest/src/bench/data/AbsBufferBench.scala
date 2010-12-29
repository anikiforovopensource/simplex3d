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
import simplex3d.math.floatx.functions._
import simplex3d.data._
import simplex3d.data.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AbsBufferBench {
  def main(args: Array[String]) {
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
    val t = DataArray[RFloat, UByte](length)
    t.put(dataArray)
    DataArray[Vec4f, UByte](t.array)
  }

  def test() {
    println("\nTesting...")
    var start = 0L

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

    start = System.currentTimeMillis
    testImplementedAbs(DataArray[Vec4f, RFloat](dataArray), loops)
    System.gc()
    val implementedArrayAbsTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedAbs(DataBuffer[Vec4f, RFloat](byteBuffer), loops)
    System.gc()
    val implementedBufferAbsTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedAbs(convertedBytes, loops)
    System.gc()
    val implementedConversionAbsTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Implemented Array time: " + implementedArrayTime + ".")
    println("Implemented Buffer time: " + implementedBufferTime + ".")
    println("Implemented Conversion time: " + implementedConversionTime + ".")
    println("Abs Array time: " + implementedArrayAbsTime + ".")
    println("Abs Buffer time: " + implementedBufferAbsTime + ".")
    println("Abs Conversion time: " + implementedConversionAbsTime + ".")
  }

  final def testImplementedArray(data: Array[Float], loops: Int) {
    var answer = 0
    val seq = DataArray[Vec4f, RFloat](data)
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += toInt(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedBuffer(data: ByteBuffer, loops: Int) {
    var answer = 0
    val seq = DataBuffer[Vec4f, RFloat](data)
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += toInt(u.x + u.y + u.z + u.w)

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
        answer += toInt(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testImplementedAbs(seq: DataSeq[Vec4f, _], loops: Int) {
    var answer = 0
    val end = seq.size
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = seq(i)
        val u = v * 7.9f
        answer += toInt(u.x + u.y + u.z + u.w)

        i += step
      }
      l += 1
    }

    println(answer)
  }
}
