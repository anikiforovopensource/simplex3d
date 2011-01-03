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

import scala.reflect._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ManifestBench {
  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  final val iterations = 2*1000*1000
  val seed = 1234567890
  val random = new java.util.Random(seed)
  
  def test() {
    println("\nTesting...")
    var start = 0L
   
    start = System.currentTimeMillis
    testClassManifestMatch(iterations)
    System.gc()
    val testClassManifestMatchTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testManifestMatch(iterations)
    System.gc()
    val testManifestMatchTime = System.currentTimeMillis - start

    println("\nResults:")
    println("ClassMaifest match time: " + testClassManifestMatchTime + " ms.")
    println("Manifest match time: " + testManifestMatchTime + " ms.")
  }

  // Manifest.equals gets slower with larger inheritance tree.
  class S1
  class S2 extends S1
  class S3 extends S2
  class S4 extends S3
  class S5 extends S4
  class S6 extends S5
  class S7 extends S6
  class S8 extends S7

  class Class1 extends S8
  class Class2 extends S8
  class Class3 extends S8
  class Class4 extends S8

  final val ClassManifest1 = ClassManifest.classType[Class1](classOf[Class1])
  final val ClassManifest2 = ClassManifest.classType[Class2](classOf[Class2])
  final val ClassManifest3 = ClassManifest.classType[Class3](classOf[Class3])
  final val Manifest1 = Manifest.classType[Class1](classOf[Class1])
  final val Manifest2 = Manifest.classType[Class2](classOf[Class2])
  final val Manifest3 = Manifest.classType[Class3](classOf[Class3])


  private final def getManifest(i: Int) :Manifest[_] = {
    (random.nextInt(3)) match {
      case 0 => Manifest1
      case 1 => Manifest2
      case 2 => Manifest3
    }
  }
  private final def getClassManifest(i: Int) :ClassManifest[_] = {
    (random.nextInt(3)) match {
      case 0 => ClassManifest1
      case 1 => ClassManifest2
      case 2 => ClassManifest3
    }
  }

  final def testClassManifestMatch(iterations: Int) {
    random.setSeed(seed)
    var a = 0

    var i = 0; while (i < iterations) {
      getClassManifest(i) match {
        case ClassManifest1 => a += 1
        case ClassManifest2 => a += 2
        case ClassManifest3 => a += 3
      }

      i += 1
    }

    println(a)
  }

  final def testManifestMatch(iterations: Int) {
    random.setSeed(seed)
    var a = 0

    var i = 0; while (i < iterations) {
      getManifest(i) match {
        case Manifest1 => a += 1
        case Manifest2 => a += 2
        case Manifest3 => a += 3
      }

      i += 1
    }

    println(a)
  }
}
