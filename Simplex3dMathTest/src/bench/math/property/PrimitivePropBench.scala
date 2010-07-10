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

package bench.math.property

import simplex3d.math.property._
import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object PrimitivePropBench {

  def main(args: Array[String]) {
    testReadWrite()
    testReadWrite()
    testReadWrite()

    testUpdate()
    testUpdate()
    testUpdate()
  }

  val len = 400*1000*1000

  def testReadWrite() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVarReadWrite(len)
    System.gc()
    val varReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSpecializedReadWrite(len)
    System.gc()
    val specReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testGenericReadWrite(len)
    System.gc()
    val genericReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSpecializedReadWriteD(len)
    System.gc()
    val specReadWriteDTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testGenericReadWriteD(len)
    System.gc()
    val genericReadWriteDTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Var Int r/w time: " + varReadWriteTime + ".")
    println("Spec Int r/w time: " + specReadWriteTime + ".")
    println("Generic Int r/w time: " + genericReadWriteTime + ".")
    println("Spec Double r/w time: " + specReadWriteDTime + ".")
    println("Generic Double r/w time: " + genericReadWriteDTime + ".")
  }

  final def testVarReadWrite(length: Int) {
    var p = 0

    var i = 0; while (i < length) {

      // Bench code
      p = p + i

      i += 1
    }

    println(p)
  }

  final def testSpecializedReadWrite(length: Int) {
    val p: Property[Int] = new SpecializedProperty(
      0,
      (x: Int) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }

  final def testGenericReadWrite(length: Int) {
    val p: Property[Int] = new GenericProperty(
      0,
      (x: Int) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }

  final def testSpecializedReadWriteD(length: Int) {
    val p: Property[Double] = new SpecializedProperty(
      0.0,
      (x: Double) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }

  final def testGenericReadWriteD(length: Int) {
    val p: Property[Double] = new GenericProperty(
      0.0,
      (x: Double) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }


  def testUpdate() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVarUpdate(len)
    System.gc()
    val varUpdateTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSpecializedUpdate(len)
    System.gc()
    val specUpdateTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testGenericUpdate(len)
    System.gc()
    val genericUpdateTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testSpecializedDUpdate(len)
    System.gc()
    val specUpdateDTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testGenericDUpdate(len)
    System.gc()
    val genericUpdateDTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Var Int function time: " + varUpdateTime + ".")
    println("Spec Int function time: " + specUpdateTime + ".")
    println("Generic Int function time: " + genericUpdateTime + ".")
    println("Spec Double function time: " + specUpdateDTime + ".")
    println("Generic Double function time: " + genericUpdateDTime + ".")
  }

  final var count = 0
  final val function = (x: Int) => { count += 1; x + count }
  final val functionD = (x: Double) => { count += 1; x + count }
  
  final def testVarUpdate(length: Int) {
    count = 0
    var p = 0

    var i = 0; while (i < length) {

      // Bench code
      p = function(p)

      i += 1
    }

    println(p)
  }

  final def testSpecializedUpdate(length: Int) {
    count = 0
    val p = new SpecializedProperty(0, function)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }

  final def testGenericUpdate(length: Int) {
    count = 0
    val p = new GenericProperty(0, function)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }

  final def testSpecializedDUpdate(length: Int) {
    count = 0
    val p = new SpecializedProperty(0.0, functionD)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }

  final def testGenericDUpdate(length: Int) {
    count = 0
    val p = new GenericProperty(0.0, functionD)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }
}

final class GenericProperty[T](
  initialValue: PropertyValue[T], function: (T) => T
) extends Property[T] {
  protected val value = initialValue.copyAsMutable()
  def update() {
    updateWith(function)
  }
}

final class SpecializedProperty[@specialized(Boolean, Int, Float, Double) T](
  initialValue: PropertyValue[T], function: (T) => T
) extends Property[T] {
  protected val value = initialValue.copyAsMutable()

  def update() {
    updateWith(function)
  }
}
