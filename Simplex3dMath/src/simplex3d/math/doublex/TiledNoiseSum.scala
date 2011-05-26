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
import simplex3d.math.doublex.functions.{abs, pow, round}


/** Noise sum.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class TiledNoiseSum(
  val tile: ConstVec4d,
  val octaves: Int,
  val lacunarity: Double = 2.0,
  val persistence: Double = 0.5,
  val noise: TiledNoiseSource = NoiseDefaults.DefaultTiledSource
) {

  private[this] val tiles = {
    val array = new Array[ConstVec4i](octaves)

    var i = 0; while (i < octaves) {
      val octaveFreq = pow(lacunarity, i)

      array(i) = ConstVec4i(
        round(tile.x*octaveFreq/noise.tileSizeX).toInt,
        round(tile.y*octaveFreq/noise.tileSizeY).toInt,
        round(tile.z*octaveFreq/noise.tileSizeZ).toInt,
        round(tile.w*octaveFreq/noise.tileSizeW).toInt
      )

      i += 1
    }

    array
  }
  private[this] val frequencyFactors = {
    val array = new Array[ConstVec4d](octaves)

    var i = 0; while (i < octaves) {
      val t = tiles(i)
      
      array(i) = ConstVec4d(
        (t.x*noise.tileSizeX)/tile.x,
        (t.y*noise.tileSizeY)/tile.y,
        (t.z*noise.tileSizeZ)/tile.z,
        (t.w*noise.tileSizeW)/tile.w
      )

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
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(
        t.x,
        x*f.x + (i << 4)
      )*a

      i += 1
    }

    sum
  }
  def apply(u: inVec2d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(
        t.x, t.y,
        u.x*f.x + (i << 4), u.y*f.y
      )*a

      i += 1
    }

    sum
  }
  def apply(u: inVec3d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(
        t.x, t.y, t.z,
        u.x*f.x + (i << 4), u.y*f.y, u.z*f.z
      )*a

      i += 1
    }

    sum
  }
  def apply(u: inVec4d) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += noise(
        t.x, t.y, t.z, t.w,
        u.x*f.x + (i << 4), u.y*f.y, u.z*f.z, u.w*f.w
      )*a

      i += 1
    }

    sum
  }
}
