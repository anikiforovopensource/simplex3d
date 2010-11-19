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

package bench.scala

import scala.reflect._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ManifestBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  final val iterations = 1*1000*1000
  final val opIterations = 300

  
  def test() {
    println("\nTesting...")
    var start = 0L

   
    start = System.currentTimeMillis
    testValManifest(iterations)
    System.gc()
    val testValManifestTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testClassManifest(iterations)
    System.gc()
    val testClassManifestTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testClassManifestMatch(iterations)
    System.gc()
    val testClassManifestMatchTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testManifest(iterations)
    System.gc()
    val testManifestTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testManifestMatch(iterations)
    System.gc()
    val testManifestMatchTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testFlops(iterations)
    System.gc()
    val testFlopsTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Val Manifest time: " + testValManifestTime + ".")
    println("Class Manifest time: " + testClassManifestTime + ".")
    println("ClassMaifest match time: " + testClassManifestMatchTime + ".")
    println("Manifest time: " + testManifestTime + ".")
    println("Manifest match time: " + testManifestMatchTime + ".")
    println("Flops time: " + testFlopsTime +
            ". Floating point operations per iteration: " + opIterations*3 + ".")
  }

  class Class1
  class Class2 extends Class1
  class Class3 extends Class2
  class Class4 extends Class3

  final val ClassManifest1 = ClassManifest.classType[Class1](classOf[Class1])
  final val ClassManifest2 = ClassManifest.classType[Class2](classOf[Class2])
  final val ClassManifest3 = ClassManifest.classType[Class3](classOf[Class3])
  final val ClassManifest4 = ClassManifest.classType[Class4](classOf[Class4])
  final val Manifest1 = Manifest.classType[Class1](classOf[Class1])
  final val Manifest2 = Manifest.classType[Class2](classOf[Class2])
  final val Manifest3 = Manifest.classType[Class3](classOf[Class3])
  final val Manifest4 = Manifest.classType[Class4](classOf[Class4])


  private final def getManifest(i: Int) :Manifest[_] = {
    (i & 0x3) match {
      case 0 => Manifest1
      case 1 => Manifest2
      case 2 => Manifest3
      case 3 => Manifest4
    }
  }
  private final def getClassManifest(i: Int) :ClassManifest[_] = {
    (i & 0x3) match {
      case 0 => ClassManifest1
      case 1 => ClassManifest2
      case 2 => ClassManifest3
      case 3 => ClassManifest4
    }
  }

  final def testValManifest(iterations: Int) {
    var a = 0
    val m = Manifest.Int

    var i = 0; while (i < iterations) {
      if (m == getManifest(i)) a += 1

      i += 1
    }

    println(a)
  }

  final def testClassManifest(iterations: Int) {
    var a = 0
    val m = ClassManifest1

    var i = 0; while (i < iterations) {
      if (m == getClassManifest(i)) a += 1

      i += 1
    }

    println(a)
  }

  final def testClassManifestMatch(iterations: Int) {
    var a = 0

    var i = 0; while (i < iterations) {
      getClassManifest(i) match {
        case ClassManifest1 => a += 1
        case ClassManifest2 => a += 2
        case ClassManifest3 => a += 3
        case ClassManifest4 => a += 4
      }

      i += 1
    }

    println(a)
  }

  final def testManifest(iterations: Int) {
    var a = 0
    val m = Manifest1

    var i = 0; while (i < iterations) {
      if (m == getManifest(i)) a += 1

      i += 1
    }

    println(a)
  }

  final def testManifestMatch(iterations: Int) {
    var a = 0

    var i = 0; while (i < iterations) {
      getManifest(i) match {
        case Manifest1 => a += 1
        case Manifest2 => a += 2
        case Manifest3 => a += 3
        case Manifest4 => a += 4
      }

      i += 1
    }

    println(a)
  }

  final def testFlops(iterations: Int) {
    var a = 0

    var i = 0; while (i < iterations) {
      a += doSomeFloatingOps(i)

      i += 1
    }

    println(a)
  }

  final def doSomeFloatingOps(x: Int) :Int = {
    var a = 0.0

    // Floating point operations: 2 muls 1 add per iteration
    var i = 0; while (i < opIterations) {
      a += i*1.1*x

      i += 1
    }

    a.toInt
  }
}
