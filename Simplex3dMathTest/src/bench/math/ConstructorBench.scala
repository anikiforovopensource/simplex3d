/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
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
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ConstructorBench {

  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val len = 100*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testSimpleVec4d(len)
    System.gc()
    val testSimpleVec4dTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedVec4d(len)
    System.gc()
    val testImplementedVec4dTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSimpleMat3d(len)
    System.gc()
    val testSimpleMat3dTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplementedMat3d(len)
    System.gc()
    val testImplementedMat3dTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Simple Vec4d time: " + testSimpleVec4dTime + ".")
    println("Implemented Vec4d time: " + testImplementedVec4dTime + ".")
    println("Simple Mat3d time: " + testSimpleMat3dTime + ".")
    println("Implemented Mat3d time: " + testImplementedMat3dTime + ".")
  }

  final def testSimpleVec4d(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val u = new SimpleVec4d(i, i + 1, i + 2, i + 3)
      val r = u*9
      a += int(r.x + r.y + r.z + r.w)

      i += 1
    }

    println(a)
  }

  final def testImplementedVec4d(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val u = Vec4d(i, i + 1, i + 2, i + 3)
      val r = u*9
      a += int(r.x + r.y + r.z + r.w)

      i += 1
    }

    println(a)
  }

  final def testSimpleMat3d(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val u = new SimpleMat3d(
        i, i + 1, i + 2,
        i + 3, i + 4, i + 5,
        i + 6, i + 7, i + 8
      )
      val r = u*9
      a += int(r.m00 + r.m11 + r.m22)

      i += 1
    }

    println(a)
  }

  final def testImplementedMat3d(length: Int) {
    var a = 0

    var i = 0; while (i < length) {

      // Bench code
      val u = Mat3d(
        i, i + 1, i + 2,
        i + 3, i + 4, i + 5,
        i + 6, i + 7, i + 8
      )
      val r = u*9
      a += int(r.m00 + r.m11 + r.m22)

      i += 1
    }

    println(a)
  }
}

class SimpleVec4d(var x: Double, var y: Double, var z: Double, var w: Double) {
  def *(c: Double) = new SimpleVec4d(x*c, y*c, z*c, w*c)
}

class SimpleMat3d(
  var m00: Double, var m10: Double, var m20: Double,
  var m01: Double, var m11: Double, var m21: Double,
  var m02: Double, var m12: Double, var m22: Double
) {
  def *(c: Double) = new SimpleMat3d(
    m00*c, m10*c, m20*c,
    m01*c, m11*c, m21*c,
    m02*c, m12*c, m22*c
  )
}
