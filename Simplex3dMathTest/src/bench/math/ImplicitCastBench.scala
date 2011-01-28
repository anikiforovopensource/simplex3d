/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package bench.math

import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ImplicitCastBench {

  // Implicit cast is eliminated by escape analysis.
  // Performance impact is small to moderate, depending on the size of the objects.
  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  val len = 100*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVecNoPromotion(len)
    System.gc()
    val vecNoPromotionTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVecPromotion(len)
    System.gc()
    val vecPromotionTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMatNoPromotion(len)
    System.gc()
    val matNoPromotionTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMatPromotion(len)
    System.gc()
    val matPromotionTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Vec no promotions time: " + vecNoPromotionTime + ".")
    println("Vec promotions time: " + vecPromotionTime + ".")
    println("Mat no promotions time: " + matNoPromotionTime + ".")
    println("Mat promotions time: " + matPromotionTime + ".")
  }

  final def testVecNoPromotion(length: Int) {
    var p = 0
    val v = Vec3d(4, 5, 6)

    var i = 0; while (i < length) {

      // Bench code
      val u = Vec3d(i + 0, i + 1, i + 2)
      val t = u + v
      p ^= Int(t.x + t.y + t.z)

      i += 1
    }

    println(p)
  }

  final def testVecPromotion(length: Int) {
    var p = 0
    val v = Vec3d(4, 5, 6)

    var i = 0; while (i < length) {

      // Bench code
      val u = Vec3i(i + 0, i + 1, i + 2)
      val t = u + v
      p ^= Int(t.x + t.y + t.z)

      i += 1
    }

    println(p)
  }

  final def testMatNoPromotion(length: Int) {
    var p = 0
    val v = Vec3d(4, 5, 6)

    var i = 0; while (i < length) {

      // Bench code
      val m = Mat3x4d(
        i + 0, i + 1, i + 2,
        i + 3, i + 4, i + 5,
        i + 6, i + 7, i + 8,
        i + 9, i + 10, i +11
      )
      val t = m.transformPoint(v)
      p ^= Int(t.x + t.y + t.z)

      i += 1
    }

    println(p)
  }
  
  final def testMatPromotion(length: Int) {
    var p = 0
    val v = Vec3d(4, 5, 6)

    var i = 0; while (i < length) {

      // Bench code
      val m = Mat3x4f(
        i + 0, i + 1, i + 2,
        i + 3, i + 4, i + 5,
        i + 6, i + 7, i + 8,
        i + 9, i + 10, i +11
      )
      val t = m.transformPoint(v)
      p ^= Int(t.x + t.y + t.z)

      i += 1
    }

    println(p)
  }
}
