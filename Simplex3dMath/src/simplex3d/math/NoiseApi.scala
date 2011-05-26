/*
 * Simplex3d, CoreMath module
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

package simplex3d.math


/** 
 *
 * @author Aleksey Nikiforov (lex)
 */
trait NoiseFactory {
  def apply(seed: Long) :NoiseSource
}

trait TiledNoiseFactory extends NoiseFactory {
  override def apply(seed: Long) :TiledNoiseSource
}


abstract class NoiseSource(val seed: Long) {
  def apply(x: Double) :Double
  def apply(x: Double, y: Double) :Double
  def apply(x: Double, y: Double, z:Double) :Double
  def apply(x: Double, y: Double, z:Double, w:Double) :Double

  def apply(x: Float) :Float = apply(x.toDouble).toFloat
  def apply(x: Float, y: Float) :Float = apply(x.toDouble, y.toDouble).toFloat
  def apply(x: Float, y: Float, z:Float) :Float = apply(x.toDouble, y.toDouble, z.toDouble).toFloat
  def apply(x: Float, y: Float, z:Float, w:Float) :Float = apply(x.toDouble, y.toDouble, z.toDouble, w.toDouble).toFloat
}

abstract class TiledNoiseSource(seed: Long) extends NoiseSource(seed) {
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


  val tileSizeXf :Float = tileSizeX.toFloat
  val tileSizeYf :Float = tileSizeY.toFloat
  val tileSizeZf :Float = tileSizeZ.toFloat
  val tileSizeWf :Float = tileSizeW.toFloat

  def apply(
    tile: Int,
    x: Float
  ) :Float = apply(tile, x.toDouble).toFloat

  def apply(
    tilex: Int, tiley: Int,
    x: Float, y: Float
  ) :Float = apply(tilex, tiley, x.toDouble, y.toDouble).toFloat

  def apply(
    tilex: Int, tiley: Int, tilez: Int,
    x: Float, y: Float, z:Float
  ) :Float = apply(tilex, tiley, tilez, x.toDouble, y.toDouble, z.toDouble).toFloat

  def apply(
    tilex: Int, tiley: Int, tilez: Int, tilew: Int,
    x: Float, y: Float, z:Float, w:Float
  ) :Float = apply(tilex, tiley, tilez, tilew, x.toDouble, y.toDouble, z.toDouble, w.toDouble).toFloat
}

object NoiseDefaults {
  final val DefaultTiledFactory: TiledNoiseFactory = ClassicalGradientNoise
  final val DefaultFactory: NoiseFactory = DefaultTiledFactory
  final val DefaultTiledSource: TiledNoiseSource = ClassicalGradientNoise(0)
  final val DefaultSource: NoiseSource = DefaultTiledSource
}
