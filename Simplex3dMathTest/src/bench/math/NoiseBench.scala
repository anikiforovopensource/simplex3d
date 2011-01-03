/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2011, Simplex3d Team
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
object NoiseBench {

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
    test1d(loops)
    System.gc()
    val time1d = System.currentTimeMillis - start

    start = System.currentTimeMillis
    test2d(loops)
    System.gc()
    val time2d = System.currentTimeMillis - start

    start = System.currentTimeMillis
    test3d(loops)
    System.gc()
    val time3d = System.currentTimeMillis - start

    start = System.currentTimeMillis
    test4d(loops)
    System.gc()
    val time4d = System.currentTimeMillis - start


    println("\nResults:")
    println("1d time: " + time1d + ".")
    println("2d time: " + time2d + ".")
    println("3d time: " + time3d + ".")
    println("4d time: " + time4d + ".")
  }

  final def test1d(loops: Int) {
    var a = 0.0

    var i = 0; while (i < loops) {
      a += noise1(i)

      i += 1
    }

    println(a)
  }

  final def test2d(loops: Int) {
    var a = 0.0

    var i = 0; while (i < loops) {
      a += noise1(Vec2(i, i + 1))

      i += 1
    }

    println(a)
  }

  final def test3d(loops: Int) {
    var a = 0.0

    var i = 0; while (i < loops) {
      a += noise1(Vec3(i, i + 1, i + 2))

      i += 1
    }

    println(a)
  }

  final def test4d(loops: Int) {
    var a = 0.0

    var i = 0; while (i < loops) {
      a += noise1(Vec4(i, i + 1, i + 2, i + 3))

      i += 1
    }

    println(a)
  }
}
