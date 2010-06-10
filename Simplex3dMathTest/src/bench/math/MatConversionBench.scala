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
import simplex3d.math.floatm._
import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object MatConversionBench {
  def main(args: Array[String]) {
    new MatConversionCase().run()
  }
}

class MatConversionCase {
  val length = 10000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    preTest(length, loops)
    val preTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    implementedTest(length, loops)
    val implemented = System.currentTimeMillis - start

    start = System.currentTimeMillis
    factoryTest(length, loops)
    val factoryTime = System.currentTimeMillis - start

    println("Pretest time: " + preTime)
    println("Implemented time: " + implemented)
    println("Factory time: " + factoryTime)
  }

  final def makeMat4(m: Mat2) = Mat4(
    m.m00, m.m10, 0, 0,
    m.m01, m.m11, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  )

  final def makeMat3(m: Mat4) = Mat3(
    m.m00, m.m10, m.m20,
    m.m01, m.m11, m.m21,
    m.m02, m.m12, m.m22
  )

  def preTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops/10) {
      var i = 0; while (i < length/10) {

        // Bench code
        val m = Mat4(
          i, i + 1, i + 2, i + 3,
          i + 4, i + 5, i + 6, i + 7,
          i + 8, i + 9, i + 10, i + 11,
          i + 12, i + 13, i + 14, i + 15
        )
        val b = Mat3(1)
        b += Mat3(Mat2x2(m))
        b += Mat3(Mat2x3(m))
        b += Mat3(Mat2x4(m))
        b += Mat3(Mat3x2(m))
        b += Mat3(Mat3x3(m))
        b += Mat3(Mat3x4(m))
        b += Mat3(Mat4x2(m))
        b += Mat3(Mat4x3(m))
        b += Mat3(Mat4x4(m))
        b += Mat3(Mat2x2f(m))
        b += Mat3(Mat2x3f(m))
        b += Mat3(Mat2x4f(m))
        b += Mat3(Mat3x2f(m))
        b += Mat3(Mat3x3f(m))
        b += Mat3(Mat3x4f(m))
        b += Mat3(Mat4x2f(m))
        b += Mat3(Mat4x3f(m))
        b += Mat3(Mat4x4f(m))
        answer += int(determinant(b))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def implementedTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat4(
          i, i + 1, i + 2, i + 3,
          i + 4, i + 5, i + 6, i + 7,
          i + 8, i + 9, i + 10, i + 11,
          i + 12, i + 13, i + 14, i + 15
        )
        a += 1
        val b = Mat3(a)
        answer += int(determinant(b))

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def factoryTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat4(
          i, i + 1, i + 2, i + 3,
          i + 4, i + 5, i + 6, i + 7,
          i + 8, i + 9, i + 10, i + 11,
          i + 12, i + 13, i + 14, i + 15
        )
        a += 1
        val b = makeMat3(a)
        answer += int(determinant(b))

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
