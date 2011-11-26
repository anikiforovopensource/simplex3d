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
import simplex3d.math.float._
import simplex3d.data._
import simplex3d.data.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DataReadBench {

  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
    test()
  }

  val size = 100
  val loops = 200*1000

  val primitives1 = DataArray[RFloat, RFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitives1.size) {
      primitives1(i) = random.nextFloat
      i += 1
    }
  }
  val impl1 = DataArray[Vec3, RFloat](primitives1)
  
  val primitives2 = DataArray[RFloat, HFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitives2.size) {
      primitives2(i) = random.nextFloat
      i += 1
    }
  }
  val impl2 = DataArray[Vec3, HFloat](primitives2)
  
  val primitives3 = DataArray[RFloat, SInt](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitives3.size) {
      primitives3(i) = random.nextFloat
      i += 1
    }
  }
  val impl3 = DataArray[Vec3, SInt](primitives3)
  
  val primitives4 = DataArray[RFloat, UByte](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitives4.size) {
      primitives4(i) = random.nextFloat
      i += 1
    }
  }
  val impl4 = DataArray[Vec3, UByte](primitives4)


  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testDedicated1(impl1, loops)
    testDedicated2(impl2, loops)
    testDedicated3(impl3, loops)
    testDedicated4(impl4, loops)
    System.gc()
    val timeDedicated = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testInlinedDataSeq(impl1, loops)
    testInlinedDataSeq(impl2, loops)
    testInlinedDataSeq(impl3, loops)
    testInlinedDataSeq(impl4, loops)
    System.gc()
    val timeInlinedDataSeq = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDataSeq(impl1, loops)
    testDataSeq(impl2, loops)
    testDataSeq(impl3, loops)
    testDataSeq(impl4, loops)
    System.gc()
    val timeDataSeq = System.currentTimeMillis - start
    
    start = System.currentTimeMillis
    testData(impl1, loops)
    testData(impl2, loops)
    testData(impl3, loops)
    testData(impl4, loops)
    System.gc()
    val timeData = System.currentTimeMillis - start


    println("\nResults:")
    println("Dedicated method time: " + timeDedicated + ".")
    println("DataSeq inlined time: " + timeInlinedDataSeq + ".")
    println("DataSeq time: " + timeDataSeq + ".")
    println("Data time: " + timeData + ".")
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
        
        a += (p(j) + p(j + 1) + p(j + 2)).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }

  final def testData(seq: inData[Vec3], loops: Int) {
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
  
  
  final def testDedicated1(seq: inDataSeq[Vec3, RFloat], loops: Int) {
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
  final def testDedicated2(seq: inDataSeq[Vec3, HFloat], loops: Int) {
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
  final def testDedicated3(seq: inDataSeq[Vec3, SInt], loops: Int) {
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
  final def testDedicated4(seq: inDataSeq[Vec3, UByte], loops: Int) {
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
