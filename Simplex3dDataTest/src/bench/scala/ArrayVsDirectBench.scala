/*
 * Simplex3dData - Test Package
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

package bench.scala

import java.nio._

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.floatx.functions._
import simplex3d.data._
import simplex3d.data.float._



/**
 * @author Aleksey Nikiforov (lex)
 */
object ArrayVsDirectBench {
  def main(args: Array[String]) {
    run()
    run()
    run()
  }

  val length = 10000
  val loops = 50000

  val random = new java.util.Random()

  val dataArray = new Array[Float](length)
  random.setSeed(1)
  for (i <- 0 until length) {
    dataArray(i) = random.nextFloat
  }

  val dataBuffer = ByteBuffer.allocateDirect(4*length).order(java.nio.ByteOrder.nativeOrder).asFloatBuffer
  random.setSeed(1)
  for (i <- 0 until length) {
    dataBuffer.put(i, random.nextFloat)
  }
  val wrappedBuffer = FloatBuffer.wrap(dataArray)

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testArray(dataArray, loops)
    System.gc
    val arrayTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDirect(dataBuffer, loops)
    System.gc
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testWrapped(wrappedBuffer, loops)
    System.gc
    val wrappedTime = System.currentTimeMillis - start

    println("Array time: " + arrayTime + ".")
    println("Direct time: " + bufferTime + ".")
    println("Wrapped time: " + wrappedTime + ".")
  }

  def testArray(data: Array[Float], loops: Int) {
    var answer = 0
    val end = data.length - 1
    val step = 2

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = ConstVec2f(data(i), data(i + 1))
        val u = v * 7.9f
        answer += (u.x + u.y).toInt

        i += step
      }
      l += 1
    }

    println(answer)
  }

  def testDirect(data: java.nio.FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit - 1
    val step = 2

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = ConstVec2f(data.get(i), data.get(i + 1))
        val u = v * 7.9f
        answer += (u.x + u.y).toInt

        i += step
      }
      l += 1
    }

    println(answer)
  }

  def testWrapped(data: java.nio.FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit - 1
    val step = 2

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        val v = ConstVec2f(data.get(i), data.get(i + 1))
        val u = v * 7.9f
        answer += (u.x + u.y).toInt

        i += step
      }
      l += 1
    }

    println(answer)
  }
}
