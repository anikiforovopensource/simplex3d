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
import simplex3d.math.intm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AbsFactoryBench {
  def main(args: Array[String]) {
    new AbsFactoryCase().run()
  }
}

class AbsFactoryCase {
  val length = 100000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testImplemented(length, loops)
    val implementedTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstract(length, loops)
    val abstractTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplemented(length, loops)
    val implementedTime2 = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstract(length, loops)
    val abstractTime2 = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplemented(length, loops)
    val implementedTime3 = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAbstract(length, loops)
    val abstractTime3 = System.currentTimeMillis - start

    println("Implemented time: " + implementedTime + ".")
    println("Abstract time: " + abstractTime + ".")
    println("Implemented time: " + implementedTime2 + ".")
    println("Abstract time: " + abstractTime2 + ".")
    println("Implemented time: " + implementedTime3 + ".")
    println("Abstract time: " + abstractTime3 + ".")
  }

  def testImplemented(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val u = Vec2i(i)
        val v = ConstVec2i(i + 1)
        val r = (u + v)
        answer += (r.x*r.y)

        i += 1
      }

      l += 1
    }

    println(answer)
  }

  def testAbstract(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val u = MVec2i(i)
        val v = MConstVec2i(i + 1)
        val r = (u + v)
        answer += (r.x*r.y)

        i += 1
      }

      l += 1
    }

    println(answer)
  }
}

abstract class MAnyVec2i {
  def x: Int
  def y: Int

  def +(u: MAnyVec2i) = new MVec2i(x + u.x, y + u.y)
}
class MConstVec2i(val x: Int, val y: Int) extends MAnyVec2i
class MVec2i(var x: Int, var y: Int) extends MAnyVec2i

abstract class AbsFactoryMVec2i[T <: MAnyVec2i] {
  def apply(x: Int, y: Int) :T
  def apply(x: Int) :T = apply(x, x)
}
object MConstVec2i extends AbsFactoryMVec2i[MConstVec2i] {
  def apply(x: Int, y: Int) = new MConstVec2i(x, y)
}
object MVec2i extends AbsFactoryMVec2i[MVec2i] {
  def apply(x: Int, y: Int) = new MVec2i(x, y)
}
