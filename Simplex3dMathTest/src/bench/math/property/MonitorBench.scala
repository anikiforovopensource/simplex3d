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
object MonitorBench {

  def main(args: Array[String]) {
    testUpdate()
    testUpdate()
    testUpdate()
    testUpdate()
  }

  val len = 400*1000*1000

  def testUpdate() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testInlinedUpdate(len)
    System.gc()
    val vec2InlinedUpdateTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testMonitorUpdate(len)
    System.gc()
    val vec2MonitorUpdateTime = System.currentTimeMillis - start

    println("\nResults:")
    println("Vec2f inlined update time: " + vec2InlinedUpdateTime + ".")
    println("Vec2f monitor update time: " + vec2MonitorUpdateTime + ".")

  }

  var count = 0
  
  class PropertyImpl(val value: Vec2) extends Property[ReadVec2]

  class ChangeMonitorImpl[T](init: PropertyValue[T]) extends ChangeMonitor[T] {
    protected val value = init.copyAsMutable()
    override def onChange() {
      count += 1
    }
  }


  final def testInlinedUpdate(length: Int) {
    count = 0
    val t = Vec2(0)
    val p = new PropertyImpl(Vec2(-1))

    var i = 0; while (i < length) {

      // Bench code
      t.x = i
      t.y = i

      if (p.value != t) {
        count += 1
        p := t
      }

      i += 1
    }

    println(count)
  }

  final def testMonitorUpdate(length: Int) {
    count = 0
    val t = Vec2(0)
    val p = new ChangeMonitorImpl(Vec2(-1))

    var i = 0; while (i < length) {

      // Bench code
      t.x = i
      t.y = i
      
      p := t

      i += 1
    }

    println(count)
  }
}
