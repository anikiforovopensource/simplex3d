/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

package bench.buffer


import java.nio._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.data._
import simplex3d.data.float._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AdapterVImplementedBench {

  trait V3 extends Composite {
    type Read = Vec3#Read
    type Const = Vec3#Const
    type Component = Vec3#Component
  }
  implicit object V3Adapter extends DataAdapter[V3, DefinedFloat](components = 3) {
    def apply(backing: inContiguous[V3#Component, Raw], j: Int) :V3#Const = {
      ConstVec3(
        backing(j),
        backing(j + 1),
        backing(j + 2)
      )
    }
    def update(backing: outContiguous[V3#Component, Raw], j: Int, value: V3#Read) {
      backing(j) = value.x
      backing(j + 1) = value.y
      backing(j + 2) = value.z
    }
  }

  trait V4 extends Composite {
    type Read = Vec4#Read
    type Const = Vec4#Const
    type Component = Vec4#Component
  }
  implicit object V4Adapter extends DataAdapter[V4, DefinedFloat](components = 4) {
    def apply(backing: inContiguous[V4#Component, Raw], j: Int) :V4#Const = {
      ConstVec4(
        backing(j),
        backing(j + 1),
        backing(j + 2),
        backing(j + 3)
      )
    }
    def update(backing: outContiguous[V4#Component, Raw], j: Int, value: V4#Read) {
      backing(j) = value.x
      backing(j + 1) = value.y
      backing(j + 2) = value.z
      backing(j + 3) = value.w
    }
  }


  def main(args: Array[String]) {
    test()
    test()
    test()
  }

  val size = 1000
  val loops = 200*1000

  val primitive = DataArray[RFloat, RFloat](size);
  {
    val random = new java.util.Random(1)
    var i = 0; while( i < primitive.size) {
      primitive(i) = random.nextFloat
      i += 1
    }
  }

  val impl3 = DataArray[Vec3, RFloat](primitive)
  val impl4 = DataArray[Vec4, RFloat](primitive)
  val adp3 = DataArray[V3, RFloat](primitive)
  val adp4 = DataArray[V4, RFloat](primitive)

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

  final def testImplemented3(seq: inData[Vec3], loops: Int) {
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
  final def testImplemented4(seq: inData[Vec4], loops: Int) {
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

  final def testAdapter3(seq: inData[V3], loops: Int) {
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
  final def testAdapter4(seq: inData[V4], loops: Int) {
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
