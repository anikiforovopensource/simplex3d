/*
 * Simplex3d, DoubleMath module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.doublex

import simplex3d.math._
import simplex3d.math.doublex.functions.{abs, pow}


/** Noise sum.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class NoiseSum(
  val octaves: Int,
  val lacunarity: Double = 2.0,
  val persistence: Double = 0.5,
  val noise: NoiseSource = NoiseDefaults.DefaultSource
) {

  private[this] val frequencyFactors = {
    val array = new Array[Double](octaves)

    var i = 0; while (i < octaves) {
      array(i) = pow(lacunarity, i)
      i += 1
    }

    array
  }
  private[this] val amplitudeFactors = {
    val array = new Array[Double](octaves)

    var i = 0; while (i < octaves) {
      array(i) = pow(persistence, i)
      i += 1
    }

    array
  }

  
  def apply(x: Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(x*f + (i << 4))*a

      i += 1
    }

    sum
  }
  def apply(u: inVec2d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(u.x*f + (i << 4), u.y*f)*a

      i += 1
    }

    sum
  }
  def apply(u: inVec3d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(u.x*f + (i << 4), u.y*f, u.z*f)*a

      i += 1
    }

    sum
  }
  def apply(u: inVec4d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(u.x*f + (i << 4), u.y*f, u.z*f, u.w*f)*a

      i += 1
    }

    sum
  }
}
