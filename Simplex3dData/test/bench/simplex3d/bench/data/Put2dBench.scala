/*
 * Simplex3dData - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
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

package simplex3d.bench.data


import java.nio._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object Put2dBench {

  def main(args: Array[String]) {
    init()
    
    test()
    test()
    test()
  }

  val dims = ConstVec2i(1000)
  val copyDims = ConstVec2i(500)
  val loops = 1000

  val random = new java.util.Random()
  val dest = DataBuffer[Vec3, UByte](dims.x*dims.y)
  val src = DataBuffer[Vec3, UByte](dims.x*dims.y)
  

  def init() {
    random.setSeed(1)
    for (i <- 0 until dims.x*dims.y) {
      src(i) = Vec3(random.nextDouble, random.nextDouble, random.nextDouble)
    }
  }

  def test() {
    var start = 0L

    start = System.currentTimeMillis
    testPut2d(loops)
    val testPut2dTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testPut(loops)
    val testPutTime = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testCopyLoop(loops)
    val testCopyLoopTime = System.currentTimeMillis - start
    
    println()
    println("Put2d time: " + testPut2dTime + ".")
    println("Put time: " + testPutTime + ".")
    println("Copy loop time: " + testCopyLoopTime + ".")
    println()
  }

  def testPut2d(loops: Int) {
    val offset = ConstVec2i(1)
    val srcOffset = ConstVec2i(2)
    
    var l = 0; while (l < loops) {
      dest.put2d(dims, offset, src, dims, srcOffset, copyDims)

      l += 1
    }

    println(dest(0))
  }
  
  def testPut(loops: Int) {
    val offset = ConstVec2i(1)
    val srcOffset = ConstVec2i(2)
    
    var l = 0; while (l < loops) {
      dest.put(0, src, 0, copyDims.x*copyDims.y)

      l += 1
    }

    println(dest(0))
  }
  
  def testCopyLoop(loops: Int) {
    val offset = ConstVec2i(1)
    val srcOffset = ConstVec2i(2)
    
    var l = 0; while (l < loops) {
      def copyLoop() {
        val size = copyDims.x*copyDims.y*3
        val destBuff = dest.buffer()
        val srcBuff = src.buffer()

        var i = 0; while (i < size) {
          destBuff.put(i, srcBuff.get(i))

          i += 1
        }
      }; copyLoop()

      l += 1
    }

    println(dest(0))
  }
}
