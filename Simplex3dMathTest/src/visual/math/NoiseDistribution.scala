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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object NoiseDistribution {

  def main(args: Array[String]) {
    val scale = 1.0/50
    val noiseSpeed1 = 1.0/3
    val noiseSpeed2 = 1.0/5
    val scrollSpeed = 10

    Launcher.launch(new Function {
    final def apply(pixel: ReadVec2, t: Double) = {
      val p = pixel + t*scrollSpeed

      val timeSlot = (Int(t)/10)%4
      val noise =
        if (timeSlot == 0) noise1(p.x*scale)
        else if (timeSlot == 1) noise1(p*scale)
        else if (timeSlot ==2) noise1(Vec3(p*scale, t*noiseSpeed1))
        else noise1(Vec4(p*scale, t*noiseSpeed1, t*noiseSpeed2))

          val scaledNoise = (noise + 1)/2
          if (scaledNoise > 0.75) color(Vec3(1, 0, 0), (scaledNoise - 0.75)/0.25)
          else if (scaledNoise > 0.5) color(Vec3(1, 0, 1), (scaledNoise - 0.5)/0.25)
          else if (scaledNoise > 0.25) color(Vec3(0, 0, 1), (scaledNoise - 0.25)/0.25)
          else color(Vec3(0, 1, 1), (scaledNoise)/0.25)
    }})
  }

  private def color(shadeColor: inVec3, intencity: Double) :Vec3 = {
    mix(shadeColor/2, shadeColor, intencity)
  }
}
