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
import simplex3d.math.float._
import simplex3d.math.floatx.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AbstractVsFinalBench {
  def main(args: Array[String]) {
    val tc = new AbstractVsFinal()
    tc.run()
    tc.run()
    tc.run()
    tc.run()
  }
}

class AbstractVsFinal {
  val length = 10000
  val loops = 5000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testAbstract(length, loops, Vec4(1, 2, 3, 4))
    val abstractMutableTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstract(length, loops, ConstVec4(1, 2, 3, 4))
    val abstractConstTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testFinal(length, loops, Vec4(1, 2, 3, 4))
    val finalTime = System.currentTimeMillis - start

    println("Abstract mutable time: " + abstractMutableTime + ".")
    println("Abstract const time: " + abstractConstTime + ".")
    println("Final time: " + finalTime + ".")
  }

  final def abstractArgs(v1: ReadVec4, v2: ReadVec4) = {
    val a = (v1 + v2)
    toInt(a.x*v2.x + a.y*v2.y + a.z*v2.z + a.w*v2.w)
  }

  final def finalArgs(v1: Vec4, v2: Vec4) = {
    val a = (v1 + v2)
    toInt(a.x*v2.x + a.y*v2.y + a.z*v2.z + a.w*v2.w)
  }

  def testAbstract(length: Int, loops: Int, a: ReadVec4) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val v1 = Vec4(i, i + 1, i + 2, i + 3)
        answer += abstractArgs(v1, a)

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  def testFinal(length: Int, loops: Int, a: Vec4) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val v1 = Vec4(i, i + 1, i + 2, i + 3)
        answer += finalArgs(v1, a)

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
