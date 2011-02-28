/*
 * Simplex3d, MathTest package
 * Copyright (C) 2011, Aleksey Nikiforov
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

import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FunctionDispatchBench {

  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  val loops = 100*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testSimple(loops)
    System.gc()
    val testSimpleTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testDispatch(loops)
    System.gc()
    val testDispatchTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbsDispatch(loops)
    System.gc()
    val testAbsDispatchTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Inlined time: " + testSimpleTime + ".")
    println("Interface Dispatch time: " + testDispatchTime + ".")
    println("Unknown Interface Dispatch time: " + testAbsDispatchTime + ".")
  }


  final def testSimple(loops: Int) {
    var a = 0

    var i = 0; while (i < loops) {
      a += length(new Vec2x(i, i + 1)).toInt
      a += length(new Vec3x(i, i + 1, i + 2)).toInt

      i += 1
    }

    println(a)
  }
  
  final def testDispatch(loops: Int) {
    var a = 0

    var i = 0; while (i < loops) {
      a += lengthDisp(new Vec2x(i, i + 1)).toInt
      a += lengthDisp(new Vec3x(i, i + 1, i + 2)).toInt

      i += 1
    }

    println(a)
  }

  final def testAbsDispatch(loops: Int) {
    var a = 0

    var i = 0; while (i < loops) {
      a += lengthDisp(makeVec(i)).toInt
      a += lengthDisp(makeVec(i + 1)).toInt

      i += 1
    }

    println(a)
  }

  final def makeVec(i: Int) :VecX = {
    if ((i & 0x1) == 0) new Vec2x(i, i + 1)
    else new Vec3x(i, i + 1, i + 2)
  }

  final def length(u: Vec2x) = sqrt(u.x*u.x + u.y*u.y)
  final def length(u: Vec3x) = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)
  final def lengthDisp(u: VecX) = u.length
}

trait VecX { def length() :Double }
class Vec2x(var x: Double, var y: Double) extends VecX { def length = sqrt(x*x + y*y) }
class Vec3x(var x: Double, var y: Double, var z: Double) extends VecX { def length = sqrt(x*x + y*y + z*z) }
