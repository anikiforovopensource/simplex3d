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

package bench.property

import simplex3d.math._
import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object ClassVsTraitPropBench {

  def main(args: Array[String]) {
    testReadWrite()
    testReadWrite()
    testReadWrite()
    testReadWrite()
    testReadWrite()
  }

  val len = 100*1000*1000

  def testReadWrite() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testBaseClass(len, new CProp1(Vec2(0)))
    System.gc()
    val testBaseClass1Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testBaseClass(len, new CProp2(Vec2(0)))
    System.gc()
    val testBaseClass2Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testBaseTrait(len, new TProp1(Vec2(0)))
    System.gc()
    val testBaseTrait1Time = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testBaseTrait(len, new TProp2(Vec2(0)))
    System.gc()
    val testBaseTrait2Time = System.currentTimeMillis - start

    println("\nResults:")
    println("BaseClass1 time: " + testBaseClass1Time + ".")
    println("BaseClass2 time: " + testBaseClass2Time + ".")
    println("BaseTrait1 time: " + testBaseTrait1Time + ".")
    println("BaseTrait2 time: " + testBaseTrait2Time + ".")
  }

  final def testBaseClass(length: Int, p: BaseClass[AnyVec2]) {
    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }

  final def testBaseTrait(length: Int, p: Property[AnyVec2]) {
    var i = 0; while (i < length) {

      // Bench code
      p := p() + i

      i += 1
    }

    println(p)
  }
}

abstract class BaseClass[T](init: PropertyValue[T]) extends Property[T] {
  protected val value = init.copyAsMutable()
}
final class CProp1[T](init: PropertyValue[T]) extends BaseClass(init)
final class CProp2[T](init: PropertyValue[T]) extends BaseClass(init)


final class TProp1[T](init: PropertyValue[T]) extends Property[T] {
  protected val value = init.copyAsMutable()
}
final class TProp2[T](init: PropertyValue[T]) extends Property[T] {
  protected val value = init.copyAsMutable()
}
