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

package visual.math

import visual.math.draw._

import simplex3d.math.double._
import simplex3d.math.doublex.functions._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SmoothStepTest {

  def main(args: Array[String]) {
    Launcher.launch(new Function {
    final def apply(pixel: ReadVec2, time: Double)
    :ReadVec3 =
    {
      val lineWidth = 2.5
      val axisWidth = 1.5
      val white = Vec3(1)
      val background = Vec3(1)
      val axisColor = Vec3(0)

      val mid = dimensions/2
      val u = pixel - mid

      val color = background

      color *= {
        val shade = clamp(abs(u.x)/axisWidth, 0, 1)
        mix(axisColor, white, shade)
      }
      color *= {
        val shade = clamp(abs(u.y)/axisWidth, 0, 1)
        mix(axisColor, white, shade)
      }

      color *= {
        val scale = 2/mid.x

        val x = u.x*scale
        val y = u.y*scale

        val f = smoothstep(-1, 1, x)
        val shade = clamp(abs(f - y)/(scale*lineWidth), 0, 1)
        mix(Vec3(1, 0, 0), Vec3(1), shade)
      }
      
      color
    }})
  }
}
