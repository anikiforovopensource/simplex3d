/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
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
  val loops = 50000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    implementedTest(length, loops)
    val implemented = System.currentTimeMillis - start

    start = System.currentTimeMillis
    factoryTest(length, loops)
    val factoryTime = System.currentTimeMillis - start

    println("Factory time: " + factoryTime)
    println("Implemented time: " + implemented)
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

  def implementedTest(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val m2 = Mat2(i, i + 1, i + 2, i + 3)
        m2 += 1
        val m4 = Mat4(m2)
        m4 += 2
        val m3 = Mat3(m4)
        answer += int(determinant(m3))

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
        val m2 = Mat2(i, i + 1, i + 2, i + 3)
        m2 += 1
        val m4 = makeMat4(m2)
        m4 += 2
        val m3 = makeMat3(m4)
        answer += int(determinant(m3))

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
