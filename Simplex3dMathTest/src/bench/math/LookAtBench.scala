/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010 Simplex3d Team
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
import simplex3d.math.doublem.DoubleMath._
import simplex3d.math.doublem.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
object LookAtBench {
  def main(args: Array[String]) {
    new LookAtTC().run()
  }
}

class LookAtTC {
  val len = 10000
  val loops = 10000

  def run() {
    var start = 0L

    start = System.currentTimeMillis
    testRegular(len, loops)
    val regularTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testInlined(len, loops)
    val inlinedTime = System.currentTimeMillis - start

    start = System.currentTimeMillis
    testImplemented(len, loops)
    val implementedTime = System.currentTimeMillis - start

    println("Regular time: " + regularTime + ".")
    println("Inlined time: " + inlinedTime + ".")
    println("Implemented time: " + implementedTime + ".")
  }

  final def lookAtRegular(direction: inVec3, up: inVec3) :Mat3 = {
    val zaxis = normalize(direction)
    val xaxis = normalize(cross(up, zaxis))
    val yaxis = cross(zaxis, xaxis)
    Mat3(
      xaxis.x, xaxis.y, xaxis.z,
      yaxis.x, yaxis.y, yaxis.z,
      zaxis.x, zaxis.y, zaxis.z
    )
  }

  final def lookAtInlined(direction: inVec3, up: inVec3) :Mat3 = {
    val dirinvlen = inversesqrt(lengthSquare(direction))

    val zax = direction.x*dirinvlen
    val zay = direction.y*dirinvlen
    val zaz = direction.z*dirinvlen

    var xax = up.y*zaz - zay*up.z
    var xay = up.z*zax - zaz*up.x
    var xaz = up.x*zay - zax*up.y

    val invlen = inversesqrt(xax*xax + xay*xay + xaz*xaz)
    xax *= invlen
    xay *= invlen
    xaz *= invlen

    val yax = zay*xaz - xay*zaz
    val yay = zaz*xax - xaz*zax
    val yaz = zax*xay - xax*zay

    Mat3(
      xax, xay, xaz,
      yax, yay, yaz,
      zax, zay, zaz
    )
  }

  final def testImplemented(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val dir = Vec3(i, i + 1, i + 2)
        val m = lookAt(dir, Vec3.UnitY)
        answer += int(
          m.m00 + m.m01 + m.m02 +
          m.m10 + m.m11 + m.m12 +
          m.m20 + m.m21 + m.m22
        )

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testRegular(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val dir = Vec3(i, i + 1, i + 2)
        val m = lookAtRegular(dir, Vec3.UnitY)
        answer += int(
          m.m00 + m.m01 + m.m02 +
          m.m10 + m.m11 + m.m12 +
          m.m20 + m.m21 + m.m22
        )

        i += 1
      }
      l += 1
    }

    println(answer)
  }

  final def testInlined(length: Int, loops: Int) {
    var answer = 0

    var l = 0; while (l < loops) {
      var i = 0; while (i < length) {

        // Bench code
        val dir = Vec3(i, i + 1, i + 2)
        val m = lookAtInlined(dir, Vec3.UnitY)
        answer += int(
          m.m00 + m.m01 + m.m02 +
          m.m10 + m.m11 + m.m12 +
          m.m20 + m.m21 + m.m22
        )

        i += 1
      }
      l += 1
    }

    println(answer)
  }
}
