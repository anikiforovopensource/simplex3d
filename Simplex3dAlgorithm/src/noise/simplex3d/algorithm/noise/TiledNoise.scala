/*
 * Simplex3dAlgorithm - Noise Module
 * Copyright (C) 2012, Aleksey Nikiforov
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
import simplex3d.math.doublex.functions.{abs, pow, round, greaterThan}


/** Tiled noise.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
final class TiledNoise(
  val source: TiledNoiseSource,
  val tile: ConstVec4d,
  val frequency: Double
) extends NoiseGen with Serializable {

  final def seed: Long = source.seed
  def reseed(seed: Long) = new TiledNoise(source.reseed(seed), tile, frequency)
  
  @transient private[this] var discreteTile: ConstVec4i = _
  @transient private[this] var frequencyFactor: ConstVec4d = _
  initTransient()

  private[this] def initTransient() {
    require(frequency > 0, "Frequency must be greater than 0.")
    
    discreteTile = ConstVec4i(
      round(tile.x*frequency/source.tileSizeX).toInt,
      round(tile.y*frequency/source.tileSizeY).toInt,
      round(tile.z*frequency/source.tileSizeZ).toInt,
      round(tile.w*frequency/source.tileSizeW).toInt
    )
    
    require(all(greaterThan(discreteTile, Vec4i.Zero)), "Bad tile-frequency combination.")

    frequencyFactor = ConstVec4d(
      (discreteTile.x*source.tileSizeX)/tile.x,
      (discreteTile.y*source.tileSizeY)/tile.y,
      (discreteTile.z*source.tileSizeZ)/tile.z,
      (discreteTile.w*source.tileSizeW)/tile.w
    )
  }

  
  def apply(x: Double) :Double = {
    source(discreteTile.x, x*frequencyFactor.x)
  }
  def apply(x: Double, y: Double) :Double = {
    source(discreteTile.x, discreteTile.y, x*frequencyFactor.x, y*frequencyFactor.y)
  }
  def apply(x: Double, y: Double, z:Double) :Double = {
    source(
      discreteTile.x, discreteTile.y, discreteTile.z,
      x*frequencyFactor.x, y*frequencyFactor.y, z*frequencyFactor.z
    )
  }
  def apply(x: Double, y: Double, z:Double, w:Double) :Double = {
    source(
      discreteTile.x, discreteTile.y, discreteTile.z, discreteTile.w,
      x*frequencyFactor.x, y*frequencyFactor.y, z*frequencyFactor.z, w*frequencyFactor.w
    )
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
