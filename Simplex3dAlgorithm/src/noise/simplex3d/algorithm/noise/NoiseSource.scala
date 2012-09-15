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


/** Low level unaltered noise source.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
abstract class NoiseSource extends NoiseGen with Serializable {
  def reseed(seed: Long) :NoiseSource
}

@SerialVersionUID(8104346712419693669L)
abstract class TiledNoiseSource extends NoiseSource with Serializable {
  def reseed(seed: Long) :TiledNoiseSource
  
  val tileSizeX :Double
  val tileSizeY :Double
  val tileSizeZ :Double
  val tileSizeW :Double

  def apply(
    tile: Int,
    x: Double
  ) :Double

  def apply(
    tilex: Int, tiley: Int,
    x: Double, y: Double
  ) :Double

  def apply(
    tilex: Int, tiley: Int, tilez: Int,
    x: Double, y: Double, z:Double
  ) :Double

  def apply(
    tilex: Int, tiley: Int, tilez: Int, tilew: Int,
    x: Double, y: Double, z:Double, w:Double
  ) :Double
}
