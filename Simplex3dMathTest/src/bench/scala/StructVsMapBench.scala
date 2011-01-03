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

package bench.scala

import simplex3d.math._
import simplex3d.math.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object StructVsMapBench {

  final class TestVar(val u: Vec2 = Vec2(0))
  type TestStruct = {
    val u: Vec2
  }
  final class TestMap {
    private val map = new java.util.HashMap[String, Vec2](1)
    map.put("u", Vec2(0))
    def apply(s: String) = map.get(s)
  }

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
    testVar(len)
    System.gc()
    val testVarTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testStruct(len)
    System.gc()
    val testStructTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMap(len)
    System.gc()
    val testMapTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Var time: " + testVarTime + ".")
    println("Struct time: " + testStructTime + ".")
    println("Map time: " + testMapTime + ".")
  }

  final def testVar(length: Int) {
    val t = new TestVar()

    var i = 0; while (i < length) {

      // Bench code
      t.u := t.u + 1

      i += 1
    }

    println(t.u)
  }

  final def testStruct(length: Int) {
    val t :TestStruct = new TestVar()

    var i = 0; while (i < length) {

      // Bench code
      t.u := t.u + 1

      i += 1
    }

    println(t.u)
  }

  final def testMap(length: Int) {
    val t = new TestMap()

    var i = 0; while (i < length) {

      // Bench code
      t("u") := t("u") + 1

      i += 1
    }

    println(t("u"))
  }
}
