/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
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

import simplex3d.math.BaseMath._
import simplex3d.math.floatm._
import simplex3d.math.doublem.renamed._
import simplex3d.math.doublem.DoubleMath._



/**
 * @author Aleksey Nikiforov (lex)
 */
object MandelbrotTest {

    def main(args: Array[String]) {
      FunFrame.launch(new Fun {
        def binSearch(low: Double, high: Double)
        (f: (Double) => Double) :Double =
        {
            val mid = low*0.5 + high*0.5
            if (mid == low || mid == high) mid
            else if (f(mid) < 0) binSearch(mid, high)(f)
            else binSearch(low, mid)(f)
        }
        def zoomPointY(x: Double) = {
            binSearch(0, 2) { (y: Double) =>
                val x4 = (x - 0.25)
                val p = sqrt(x4*x4 + y*y)
                x - (p - 2*p*p + 0.25)
            }
        }

        val scale: Double = 200d
        val speed: Double = 1.1d

        val offset = Vec2(0.29505737159927603, -0.018132000868057857)

        val blue = 10
        val blueGreen = blue + 50
        val greenBlue = blueGreen + 75
        val blueRed = greenBlue + 255
        val redBlack = blueRed + 255
        val maxi = redBlack

        val colors = new Array[ConstVec3f](maxi)

        {
            for (i <- 0 until maxi) {
                colors(i) = color(i)
            }
        }
        
        val setColor = colors(maxi - 1)

        final def color(i :Int) = {
            def intencity(x: Float, zoneSize: Float) = {
                FloatMath.mix(0.2f, 1f, x/zoneSize)
            }

            if (i < blue) {
                val j = i
                val s = blue
                Vec3f(0, 0, intencity(i, s))
            }
            else if (i < blueGreen) {
                val j = i - blue
                val s = blueGreen - blue
                Vec3f(0, intencity(j, s), intencity(s - j, s))
            }
            else if (i < greenBlue) {
                val j = i - blueGreen
                val s = greenBlue - blueGreen
                Vec3f(0, intencity(s - j, s), intencity(j, s))
            }
            else if (i < blueRed) {
                val j = i - greenBlue
                val s = blueRed - greenBlue
                Vec3f(intencity(j, s), 0, intencity(s - j, s))
            }
            else {
                val j = i - blueRed
                val s = maxi - blueRed
                Vec3f(intencity(s - j, s), 0, 0)
            }
        }

        final def apply(pixel: AnyVec2f, time: Float, dim: AnyVec2f)
        :ConstVec3f =
        {
            val mid = dim/2
            if (FloatMath.approxEqual(pixel, mid, 2f)) {
                return Vec3f(1, 0, 0)
            }

            val c = (pixel - mid)/(scale + pow(speed, 1 + time)) + offset
            
            // quick elimination
            val x4 = (c.x - 0.25)
            val p = sqrt(x4*x4 + c.y*c.y)
            if (c.x < p - 2*p*p + 0.25) return setColor
            if ((c.x + 1)*(c.x + 1) + c.y*c.y < 0.0625) return setColor

            var x = 0d
            var y = 0d

            var i = 0; while (x*x + y*y <= 4 && i < maxi) {
                val xt = x*x - y*y + c.x

                y = 2*x*y + c.y
                x = xt

                i += 1
            }

            if (i == maxi) setColor
            else colors(i)
        }})
    }
}
