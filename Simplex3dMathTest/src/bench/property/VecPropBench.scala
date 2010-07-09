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
object VecPropBench {

  def main(args: Array[String]) {
    testReadWrite()
    testReadWrite()
    testReadWrite()

    testUpdate()
    testUpdate()
    testUpdate()
  }

  val len = 100*1000*1000

  def testReadWrite() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVec2ReadWrite(len)
    System.gc()
    val vec2ReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4ReadWrite(len)
    System.gc()
    val vec4ReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec2PropReadWrite(len)
    System.gc()
    val vec2PropReadWriteTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4PropReadWrite(len)
    System.gc()
    val vec4PropReadWriteTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Vec2f r/w time: " + vec2ReadWriteTime + ".")
    println("Vec4f r/w time: " + vec4ReadWriteTime + ".")
    println("Vec2fProp r/w time: " + vec2PropReadWriteTime + ".")
    println("Vec4fProp r/w time: " + vec4PropReadWriteTime + ".")
  }

  final def testVec2ReadWrite(length: Int) {
    val p = Vec2(0)

    var i = 0; while (i < length) {

      // Bench code
      p := p + 1

      i += 1
    }

    println(p)
  }

  final def testVec4ReadWrite(length: Int) {
    val p = Vec4(0)

    var i = 0; while (i < length) {

      // Bench code
      p := p + 1

      i += 1
    }

    println(p)
  }

  final def testVec2PropReadWrite(length: Int) {
    val p: Property[AnyVec2] = new SimpleProperty(
      Vec2(0),
      (u: inVec2) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + 1

      i += 1
    }

    println(p)
  }

  final def testVec4PropReadWrite(length: Int) {
    val p: Property[AnyVec4] = new SimpleProperty(
      Vec4(0),
      (u: inVec4) => throw new AssertionError
    )

    var i = 0; while (i < length) {

      // Bench code
      p := p() + 1

      i += 1
    }

    println(p)
  }


  def testUpdate() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testVec2Update(len)
    System.gc()
    val vec2UpdateDTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4Update(len)
    System.gc()
    val vec4UpdateDTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec2PropUpdate(len)
    System.gc()
    val vec2PropUpdateDTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testVec4PropUpdate(len)
    System.gc()
    val vec4PropUpdateDTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Vec2f function time: " + vec2UpdateDTime + ".")
    println("Vec4f function time: " + vec4UpdateDTime + ".")
    println("Vec2fProp function time: " + vec2PropUpdateDTime + ".")
    println("Vec4fProp function time: " + vec4PropUpdateDTime + ".")
  }

  final var count = 0
  final val functionVec2 = (u: inVec2) => { count += 1; u + count }
  final val functionVec4 = (u: inVec4) => { count += 1; u + count }

  final def testVec2Update(length: Int) {
    count = 0
    val p = Vec2(0)

    var i = 0; while (i < length) {

      // Bench code
      p := functionVec2(p)

      i += 1
    }

    println(p)
  }

  final def testVec4Update(length: Int) {
    count = 0
    val p = Vec4(0)

    var i = 0; while (i < length) {

      // Bench code
      p := functionVec4(p)

      i += 1
    }

    println(p)
  }

  final def testVec2PropUpdate(length: Int) {
    count = 0
    val p = new SimpleProperty(Vec2(0), functionVec2)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }

  final def testVec4PropUpdate(length: Int) {
    count = 0
    val p = new SimpleProperty(Vec4(0), functionVec4)

    var i = 0; while (i < length) {

      // Bench code
      p.update()

      i += 1
    }

    println(p)
  }
}

final class SimpleProperty[@specialized(Boolean, Int, Float, Double) T](
  initialValue: PropertyValue[T], function: (T) => T
) extends Property[T] {
  protected val value = initialValue.copyAsMutable()

  def update() {
    updateWith(function)
  }
}
