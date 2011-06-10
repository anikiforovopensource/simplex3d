/*
 * Simplex3d, DataTest package
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

package bench.data


import java.nio._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.data._
import simplex3d.data.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DataSeqVsBatch {

  // Batch may perform better or worse depending on the number of distinct implementations used.
  // With only a few competing implementations, batch can be 30% faster.
  // With many competing implementations, batch can be 300% slower.
  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
    test()
  }

  val size = 100
  val loops = 200*1000

  val primitivesRFloat = DataArray[RFloat, RFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitivesRFloat.size) {
      primitivesRFloat(i) = random.nextFloat
      i += 1
    }
  }
  val implRFloat = DataArray[Vec3, RFloat](primitivesRFloat)
  
  val primitivesHFloat = DataArray[RFloat, HFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitivesHFloat.size) {
      primitivesHFloat(i) = random.nextFloat
      i += 1
    }
  }
  val implHFloat = DataArray[Vec3, HFloat](primitivesHFloat)
  
  val primitivesSInt = DataArray[RFloat, SInt](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitivesSInt.size) {
      primitivesSInt(i) = random.nextFloat
      i += 1
    }
  }
  val implSInt = DataArray[Vec3, SInt](primitivesSInt)
  
  val primitivesUByte = DataArray[RFloat, UByte](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitivesUByte.size) {
      primitivesUByte(i) = random.nextFloat
      i += 1
    }
  }
  val implUByte = DataArray[Vec3, UByte](primitivesUByte)


  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testDataSeq(implRFloat, loops)
    testDataSeq(implHFloat, loops)
    testDataSeq(implSInt, loops)
    testDataSeq(implUByte, loops)
    System.gc()
    val timeDataSeq = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testInlinedDataSeq(implRFloat, loops)
    testInlinedDataSeq(implHFloat, loops)
    testInlinedDataSeq(implSInt, loops)
    testInlinedDataSeq(implUByte, loops)
    System.gc()
    val timeInlinedDataSeq = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testBatch(implRFloat, loops)
    testBatch(implHFloat, loops)
    testBatch(implSInt, loops)
    testBatch(implUByte, loops)
    System.gc()
    val timeBatch = System.currentTimeMillis - start


    println("\nResults:")
    println("DataSeq time: " + timeDataSeq + ".")
    println("DataSeq inlined time: " + timeInlinedDataSeq + ".")
    println("Batch time: " + timeBatch + ".")
  }

  final def testDataSeq(seq: inDataSeq[Vec3, Raw], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }
  
  final def testInlinedDataSeq(seq: inDataSeq[Vec3, Raw], loops: Int) {
    var a = 0
    
    val p = seq.primitives
    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val j = seq.offset + i*seq.stride
        
        val x = p(j)
        val y = p(j + 1)
        val z = p(j + 2)
        
        a += (x + y + z).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }

  final def testBatch(seq: inBatch[ReadVec3], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }
}
