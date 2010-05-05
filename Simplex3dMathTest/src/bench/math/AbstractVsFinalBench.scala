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
import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AbstractVsFinalBench {
  def main(args: Array[String]) {
    new AbstractVsFinal().run()
  }
}

class AbstractVsFinal {
  val length = 100000
  val loops = 5000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testAbstract(length, loops)
    val abstractTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testFinal(length, loops)
    val finalTime = System.currentTimeMillis - start

    println("Abstract time: " + abstractTime + ".")
    println("Final time: " + finalTime + ".")
  }

  final def abstractArgs(v1: AnyVec4, v2: AnyVec4, v3: AnyVec4) = {
    val a = (v1 + v2) * v3
    int(a.x + a.y + a.z + a.w)
  }

  final def finalArgs(v1: Vec4, v2: Vec4, v3: Vec4) = {
    val a = (v1 + v2) * v3
    int(a.x + a.y + a.z + a.w)
  }

  def testAbstract(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val v1 = Vec4(i, i + 1, i + 2, i + 3)
        val v2 = Vec4(i + 4, i + 5, i + 6, i + 7)
        val v3 = Vec4(i + 8, i + 9, i, i + 1)
        answer += abstractArgs(v1, v2, v3)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testFinal(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val v1 = Vec4(i, i + 1, i + 2, i + 3)
        val v2 = Vec4(i + 4, i + 5, i + 6, i + 7)
        val v3 = Vec4(i + 8, i + 9, i, i + 1)
        answer += finalArgs(v1, v2, v3)

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
