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
object ConvertPutBench {

  def main(args: Array[String]) {
    init()
    
    test()
    test()
    test()
  }

  val length = 10000
  val loops = 10000

  val random = new java.util.Random()
  val floatByteBuffer = ByteBuffer.allocateDirect(4*length).order(ByteOrder.nativeOrder)

  def init() {
    val floatBuffer = floatByteBuffer.asFloatBuffer
    random.setSeed(1)
    for (i <- 0 until length) {
      floatBuffer.put(i, random.nextFloat)
    }
  }

  def test() {
    var start = 0L

    start = System.currentTimeMillis
    testConvertPutSShort(floatByteBuffer, loops)
    val convertPutSShortTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testConvertPutHFloat(floatByteBuffer, loops)
    val convertPutHFloatTime = System.currentTimeMillis - start

    println("ConvertPutSShort time: " + convertPutSShortTime + ".")
    println("ConvertPutHFloat time: " + convertPutHFloatTime + ".")
  }

  def testConvertPutSShort(data: ByteBuffer, loops: Int) {
    var answer = 0

    val size = data.capacity/4
    val offset = 1
    val stride = 2
    val bytes = 2
    val dest = DataView[RFloat, SShort](
      ByteBuffer.allocateDirect(
        size*bytes*stride + offset*bytes
      ),
      offset, stride
    )
    val src = DataBuffer[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += (dest(l % size)*1000).toInt

      l += 1
    }

    println(answer)
  }

  def testConvertPutHFloat(data: ByteBuffer, loops: Int) {
    var answer = 0

    val size = data.capacity/4
    val offset = 1
    val stride = 2
    val bytes = 2
    val dest = DataView[RFloat, HFloat](
      ByteBuffer.allocateDirect(
        size*bytes*stride + offset*bytes
      ),
      offset, stride
    )
    val src = DataBuffer[RFloat, RFloat](data)

    var l = 0; while (l < loops) {
      dest.put(src)
      answer += (dest(l % size)*1000).toInt

      l += 1
    }

    println(answer)
  }
}
