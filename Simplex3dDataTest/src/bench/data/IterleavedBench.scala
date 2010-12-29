/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010, Simplex3d Team
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
import simplex3d.math.floatx.FloatMath._
import simplex3d.data._
import simplex3d.data.floatm._


/**
 * @author Aleksey Nikiforov (lex)
 */
object IterleavedBench {
  def main(args: Array[String]) {
    for (i <- 0 until 6) { test() }
  }

  val length = 2000000
  val loops = 10

  val vertices = DataBuffer[Vec3, RFloat](length)
  val normals = DataBuffer[Vec3, RFloat](length)
  val colors = DataBuffer[Vec4, UByte](length);
  {
    val random = new java.util.Random(1)
    def r = random.nextFloat
    var i = 0; while( i < length) {
      vertices(i) = Vec3(r, r, r)
      normals(i) = normalize(Vec3(r, r, r))
      colors(i) = Vec4(r, r, r, r)
      i += 1
    }
  }

  val (iv, in, ic) = interleave(vertices, normals, colors)(vertices.size)


  def test() {
    println("\nTesting...")
    var start = 0L
    
    start = System.currentTimeMillis
    testContigious(vertices, normals, colors, loops)
    System.gc()
    val contigiousTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInterleaved(iv, in, ic, loops)
    System.gc()
    val interleavedTime = System.currentTimeMillis - start


    println("\nResults:")
    println("Contigious time: " + contigiousTime + ".")
    println("Interleaved time: " + interleavedTime + ".")
  }

  final def testContigious(
    v: DataSeq[Vec3, Raw],
    n: DataSeq[Vec3, Raw],
    c: DataSeq[Vec4, Raw],
    loops: Int
  ) {
    val (av, an, ac) = (Vec3(0), Vec3(0), Vec4(0))

    var l = 0; while (l < loops) {
      var i = 0; while (i < vertices.size) {

        av += v(i)
        an += n(i)
        ac += c(i)

        i += 1
      }
      l += 1
    }

    av /= vertices.size
    an /= vertices.size
    ac /= vertices.size
    println(av, an, ac)
  }

  final def testInterleaved(
    v: DataView[Vec3, Raw],
    n: DataView[Vec3, Raw],
    c: DataView[Vec4, Raw],
    loops: Int
  ) {
    val (av, an, ac) = (Vec3(0), Vec3(0), Vec4(0))

    var l = 0; while (l < loops) {
      var i = 0; while (i < vertices.size) {

        av += v(i)
        an += n(i)
        ac += c(i)

        i += 1
      }
      l += 1
    }

    av /= vertices.size
    an /= vertices.size
    ac /= vertices.size
    println(av, an, ac)
  }
}
