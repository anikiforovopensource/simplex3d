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

package bench.scala

import simplex3d.math._
import simplex3d.math.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object EqualsBench {

  def main(args: Array[String]) {
    test()
    test()
    test()
    test()
  }

  val len = 400*1000*1000

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testScalaVec2(len, Vec2(123))
    System.gc()
    val testScalaVec2Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testJavaVec2(len, Vec2(123))
    System.gc()
    val testJavaVec2Time = System.currentTimeMillis - start


    start = System.currentTimeMillis
    testAbstractScala(len, Vec2(123))
    System.gc()
    val testAbsScalaVec2Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstractScala(len, Vec3(123))
    System.gc()
    val testAbsScalaVec3Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstractJava(len, Vec2(123))
    System.gc()
    val testAbsJavaVec2Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstractJava(len, Vec3(123))
    System.gc()
    val testAbsJavaVec3Time = System.currentTimeMillis - start

    println("\nResults:")
    println("Scala Vec2 time: " + testScalaVec2Time + ".")
    println("Java Vec2 time: " + testJavaVec2Time + ".")
    println("Abstract Scala Vec2 time: " + testAbsScalaVec2Time + ".")
    println("Abstract Scala Vec3 time: " + testAbsScalaVec3Time + ".")
    println("Abstract Java Vec2 time: " + testAbsJavaVec2Time + ".")
    println("Abstract Java Vec3 time: " + testAbsJavaVec3Time + ".")
  }

  final def scalaAbsEquals[T](a: T, b: T) = {
    a == b
  }
  final def javaAbsEquals[T](a: T, b: T) = {
    a.equals(b)
  }

  final def testScalaVec2(length: Int, a: inVec2) {
    var answer = false
    var i = 0; while (i < length) {

      // Bench code
      if (a == Vec2i(i)) answer = !answer

      i += 1
    }

    println(answer)
  }

  final def testJavaVec2(length: Int, a: inVec2) {
    var answer = false
    var i = 0; while (i < length) {

      // Bench code
      if (a.equals(Vec2i(i))) answer = !answer

      i += 1
    }

    println(answer)
  }

  final def testAbstractScala[T](length: Int, a: T) {
    var answer = false
    var i = 0; while (i < length) {

      // Bench code
      if (scalaAbsEquals(a, Vec2i(i))) answer = !answer

      i += 1
    }

    println(answer)
  }

  final def testAbstractJava[T](length: Int, a: T) {
    var answer = false
    var i = 0; while (i < length) {

      // Bench code
      if (javaAbsEquals(a, Vec2i(i))) answer = !answer

      i += 1
    }

    println(answer)
  }
}
