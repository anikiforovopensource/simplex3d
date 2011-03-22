/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package visual.math

import visual.math.draw._

import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TurbulenceTest {

  def main(args: Array[String]) {
    val scale = 1.0/200
    val scrollSpeed = 5
    val noiseSpeed = 1.0/30

    val octaves = 4
    val lacunarity = 2
    val amplitude = 1.5

    val s1 = (for (i <- 0 until octaves) yield pow(lacunarity, i)).toArray
    val s2 = (for (i <- 0 until octaves) yield pow(amplitude, -i)).toArray

    def noiseSum(p: inVec3) = {
      def octave(i: Int, p: inVec3) = {
        abs(noise1(p*s1(i))*s2(i))
      }

      var sum = 0.0; var i = 0; while (i < octaves) {
        sum += octave(i, p + i)
        i += 1
      }
      sum
    }

    Launcher.launch(new Function {
    final def apply(pixel: ReadVec2, time: Double) = {
      val p = pixel + time*scrollSpeed
      Vec3(noiseSum(Vec3(p*scale, time*noiseSpeed))*0.65)
    }})
  }
}
