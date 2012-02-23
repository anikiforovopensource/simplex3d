/*
 * Simplex3dAlgorithm - Noise Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.algorithm.noise

import java.io._
import simplex3d.math._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions.{abs, pow, round}


/** Noise sum.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
final class TiledNoiseSum(
  val source: TiledNoiseSource,
  val tile: ConstVec4d,
  val frequency: Double,
  val octaves: Int,
  val lacunarity: Double = 2.0,
  val persistence: Double = 0.5
) extends NoiseGen with Serializable {

  final def seed: Long = source.seed
  def reseed(seed: Long) = new TiledNoiseSum(source.reseed(seed), tile, frequency, octaves, lacunarity, persistence)
  
  @transient private[this] var tiles: Array[ConstVec4i] = _
  @transient private[this] var frequencyFactors: Array[ConstVec4d] = _
  @transient private[this] var amplitudeFactors: Array[Double] = _
  initTransient()

  private[this] def initTransient() {
    tiles = new Array[ConstVec4i](octaves)

    var i = 0; while (i < octaves) {
      val octaveFreq = pow(lacunarity, i)*frequency

      tiles(i) = ConstVec4i(
        round(tile.x*octaveFreq/source.tileSizeX).toInt,
        round(tile.y*octaveFreq/source.tileSizeY).toInt,
        round(tile.z*octaveFreq/source.tileSizeZ).toInt,
        round(tile.w*octaveFreq/source.tileSizeW).toInt
      )

      i += 1
    }


    frequencyFactors = new Array[ConstVec4d](octaves)

    i = 0; while (i < octaves) {
      val t = tiles(i)

      frequencyFactors(i) = ConstVec4d(
        (t.x*source.tileSizeX)/tile.x,
        (t.y*source.tileSizeY)/tile.y,
        (t.z*source.tileSizeZ)/tile.z,
        (t.w*source.tileSizeW)/tile.w
      )

      i += 1
    }


    amplitudeFactors = new Array[Double](octaves)

    i = 0; while (i < octaves) {
      amplitudeFactors(i) = pow(persistence, i)
      i += 1
    }
  }

  
  def apply(x: Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += source(
        t.x,
        x*f.x + (i << 4)
      )*a

      i += 1
    }

    sum
  }
  def apply(x: Double, y: Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += source(
        t.x, t.y,
        x*f.x + (i << 4), y*f.y
      )*a

      i += 1
    }

    sum
  }
  def apply(x: Double, y: Double, z:Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += source(
        t.x, t.y, t.z,
        x*f.x + (i << 4), y*f.y, z*f.z
      )*a

      i += 1
    }

    sum
  }
  def apply(x: Double, y: Double, z:Double, w:Double) :Double = {
    var sum = 0.0; var i = 0; while (i < octaves) {
      val t = tiles(i)
      val f = frequencyFactors(i)
      val a = amplitudeFactors(i)

      sum += source(
        t.x, t.y, t.z, t.w,
        x*f.x + (i << 4), y*f.y, z*f.z, w*f.w
      )*a

      i += 1
    }

    sum
  }


  @throws(classOf[IOException])
  private[this] def writeObject(out: ObjectOutputStream) {
    out.defaultWriteObject()
  }

  @throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
  private[this] def readObject(in: ObjectInputStream) {
    in.defaultReadObject()
    initTransient()
  }
}
