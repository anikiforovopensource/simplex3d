/*
 * Simplex3dData - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.bench.data


import java.nio._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.data._
import simplex3d.data.float._
import simplex3d.data.extension._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AdapterVImplementedBench {

  trait V3 extends CompositeFormat {
    type Accessor = Vec3
    type Component = Vec3#Component
  }
  implicit object V3Adapter extends DataAdapter[V3, TangibleFloat](components = 3) {
    def apply(primitives: inContiguous[V3#Component, Raw], j: Int) :V3#Accessor#Const = {
      ConstVec3(
        primitives(j),
        primitives(j + 1),
        primitives(j + 2)
      )
    }
    def update(primitives: Contiguous[V3#Component, Raw], j: Int, value: V3#Accessor#Read) {
      primitives(j) = value.x
      primitives(j + 1) = value.y
      primitives(j + 2) = value.z
    }
  }

  trait V4 extends CompositeFormat {
    type Accessor = Vec4
    type Component = Vec4#Component
  }
  implicit object V4Adapter extends DataAdapter[V4, TangibleFloat](components = 4) {
    def apply(primitives: inContiguous[V4#Component, Raw], j: Int) :V4#Accessor#Const = {
      ConstVec4(
        primitives(j),
        primitives(j + 1),
        primitives(j + 2),
        primitives(j + 3)
      )
    }
    def update(primitives: Contiguous[V4#Component, Raw], j: Int, value: V4#Accessor#Read) {
      primitives(j) = value.x
      primitives(j + 1) = value.y
      primitives(j + 2) = value.z
      primitives(j + 3) = value.w
    }
  }


  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val size = 1000
  val loops = 200*1000

  val primitives = DataArray[RFloat, RFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitives.size) {
      primitives(i) = random.nextFloat
      i += 1
    }
  }

  val impl3 = DataArray[Vec3, RFloat](primitives)
  val impl4 = DataArray[Vec4, RFloat](primitives)
  val adp3 = DataArray[V3, RFloat](primitives)
  val adp4 = DataArray[V4, RFloat](primitives)

  def test() {
    println("\nTesting...")
    var start = 0L

    start = System.currentTimeMillis
    testImplemented3(impl3, loops)
    testImplemented4(impl4, loops)
    System.gc()
    val timeImplemented = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testAdapter3(adp3, loops)
    testAdapter4(adp4, loops)
    System.gc()
    val timeAdapter = System.currentTimeMillis - start


    println("\nResults:")
    println("Implemented time: " + timeImplemented + ".")
    println("Adapter time: " + timeAdapter + ".")
  }

  final def testImplemented3(seq: inDataSeq[Vec3, Raw], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }
  final def testImplemented4(seq: inDataSeq[Vec4, Raw], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z + u.w).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }

  final def testAdapter3(seq: inDataSeq[V3, Raw], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }
  final def testAdapter4(seq: inDataSeq[V4, Raw], loops: Int) {
    var a = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < seq.size) {
        val u = seq(i)
        a += (u.x + u.y + u.z + u.w).toInt

        i += 1
      }
      l += 1
    }

    println(a)
  }
}
