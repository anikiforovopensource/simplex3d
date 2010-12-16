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

import java.io._
import java.nio._
import simplex3d.math._
import simplex3d.math.floatm.renamed._
import simplex3d.math.floatm.FloatMath._
import simplex3d.buffer._
import simplex3d.buffer.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FactoryBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val size = 10
  val loops = 10*1000*1000


  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testPackageFactory(size, loops)
    System.gc()
    val packageFactoryTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMkDataSeq(size, loops)
    System.gc()
    val mkDataSeqTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAsReadOnlySeq(size, loops)
    System.gc()
    val asReadOnlyTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Package factory time: " + packageFactoryTime + ".")
    println("mkDataSeq time: " + mkDataSeqTime + ".")
    println("asReadOnly time: " + asReadOnlyTime + ".")
  }

  final def testPackageFactory(size: Int, loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      val da = DataArray[RFloat, RFloat](size)
      a += da.size

      l += 1
    }

    println(a)
  }

  final def testMkDataSeq(size: Int, loops: Int) {
    var a = 0
    val factory = DataArray[RFloat, RFloat](0)

    var l = 0; while (l < loops) {
      val da = factory.mkDataArray(size)
      a += da.size

      l += 1
    }

    println(a)
  }

  final def testAsReadOnlySeq(size: Int, loops: Int) {
    var a = 0
    val da = DataArray[RFloat, RFloat](size)

    var l = 0; while (l < loops) {
      a += da.asReadOnly().size

      l += 1
    }

    println(a)
  }
}
