/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DuplicateBench {
  def main(args: Array[String]) {
    val tc = new DuplicateBench()
    tc.run()
    tc.run()
    tc.run()
    tc.run()
  }
}

class DuplicateBench {
  val length = 100
  val loops = 10000000

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
    System.gc()
    val bufferTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testBufferDuplicate(dataBuffer, loops)
    System.gc()
    val bufferDuplicateTime = System.currentTimeMillis - start


    println("Buffer time: " + bufferTime + ".")
    println("Buffer dup time: " + bufferDuplicateTime + ".")
  }

  final def testBuffer(data: FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit
    val step = 1

    var l = 0; while (l < loops) {
      var i = 0; while (i < end) {

        answer += toInt(data.get(i))

        i += step
      }
      l += 1
    }

    println(answer)
  }

  final def testBufferDuplicate(data: FloatBuffer, loops: Int) {
    var answer = 0
    val end = data.limit
    val step = 1

    var l = 0; while (l < loops) {
      val dup = data.duplicate()
      
      var i = 0; while (i < end) {

        answer += toInt(dup.get(i))

        i += step
      }
      l += 1
    }

    println(answer)
  }
}
