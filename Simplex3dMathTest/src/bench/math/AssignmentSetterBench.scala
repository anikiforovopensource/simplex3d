/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AssignmentSetterBench {
  def main(args: Array[String]) {
    val tc = new AssignmentSetterBench()
    tc.run()
    tc.run()
    tc.run()
    tc.run()
  }
}

class AssignmentSetterBench {
  val length = 10000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testAssignment(length, loops)
    System.gc
    val assignmentTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testConstAssignment(length, loops)
    System.gc
    val constAssignmentTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSetter(length, loops)
    System.gc
    val setterTime = System.currentTimeMillis - start

    println("Assignment time: " + assignmentTime + ".")
    println("Const assignment time: " + constAssignmentTime + ".")
    println("Setter time: " + setterTime + ".")
  }

  def testAssignment(length: Int, loops: Int) {
    var res = Mat3d(1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res = res + a

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testConstAssignment(length: Int, loops: Int) {
    var res = ConstMat3d(1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res = res + a

        i += 1
      }

      l += 1
    }

    println(res)
  }

  def testSetter(length: Int, loops: Int) {
    val res = Mat3d(1)

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val a = Mat3d(i, i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8)
        res := res + a

        i += 1
      }

      l += 1
    }

    println(res)
  }
}
