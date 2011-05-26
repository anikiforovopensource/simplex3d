/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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

package simplex3d.math.floatx

import simplex3d.math._
import simplex3d.math.floatx.functions._


/** Noise source.
 *
 * @author Aleksey Nikiforov (lex)
 */
class NoiseSource(val seed: Long, val noiseFactory: NoiseFactory = ClassicalGradientNoise) {

  private[math] final val nx = noiseFactory(seed & ((1L << 46) - 1))
  private[math] final val ny = noiseFactory(seed & ((1L << 46) - 1) | (1L << 46))
  private[math] final val nz = noiseFactory(seed & ((1L << 46) - 1) | (2L << 46))
  private[math] final val nw = noiseFactory(seed & ((1L << 46) - 1) | (3L << 46))

  
  /** Computes a value of a 1-dimensional noise function.
   * The noise function is C^2^ continuous (the first and the second derivatives are continuous).
   *
   * The return values of the noise function have the following properties:
   *   - They are in the range [-1, 1].
   *   - They have Gaussian dirstibution.
   *   - The overall average is zero.
   *   - A particular argument will always result in the same return value.
   *   - Statistical properties do not change with rotation and translation of the domain.
   *
   * @param x a double argument.
   * @return a value of a noise function.
   */
  final def noise1(x: Float) :Float = nx.noise(x).toFloat
  final def noise1(u: inVec2f) :Float = nx.noise(u.x, u.y).toFloat
  final def noise1(u: inVec3f) :Float = nx.noise(u.x, u.y, u.z).toFloat
  final def noise1(u: inVec4f) :Float = nx.noise(u.x, u.y, u.z, u.w).toFloat

  /** Computes __two__ independent values of a 1-dimensional noise function.
   * @param x a double argument.
   * @return two values of a noise function packed as Vec2.
   */
  final def noise2(x: Float) :Vec2f = new Vec2f(
    nx.noise(x).toFloat,
    ny.noise(x).toFloat
  )
  final def noise2(u: inVec2f) :Vec2f = new Vec2f(
    nx.noise(u.x, u.y).toFloat,
    ny.noise(u.x, u.y).toFloat
  )
  final def noise2(u: inVec3f) :Vec2f = new Vec2f(
    nx.noise(u.x, u.y, u.z).toFloat,
    ny.noise(u.x, u.y, u.z).toFloat
  )
  final def noise2(u: inVec4f) :Vec2f = new Vec2f(
    nx.noise(u.x, u.y, u.z, u.w).toFloat,
    ny.noise(u.x, u.y, u.z, u.w).toFloat
  )

  final def noise3(x: Float) :Vec3f = new Vec3f(
    nx.noise(x).toFloat,
    ny.noise(x).toFloat,
    nz.noise(x).toFloat
  )
  final def noise3(u: inVec2f) :Vec3f = new Vec3f(
    nx.noise(u.x, u.y).toFloat,
    ny.noise(u.x, u.y).toFloat,
    nz.noise(u.x, u.y).toFloat
  )
  final def noise3(u: inVec3f) :Vec3f = new Vec3f(
    nx.noise(u.x, u.y, u.z).toFloat,
    ny.noise(u.x, u.y, u.z).toFloat,
    nz.noise(u.x, u.y, u.z).toFloat
  )
  final def noise3(u: inVec4f) :Vec3f = new Vec3f(
    nx.noise(u.x, u.y, u.z, u.w).toFloat,
    ny.noise(u.x, u.y, u.z, u.w).toFloat,
    nz.noise(u.x, u.y, u.z, u.w).toFloat
  )

  final def noise4(x: Float) :Vec4f = new Vec4f(
    nx.noise(x).toFloat,
    ny.noise(x).toFloat,
    nz.noise(x).toFloat,
    nw.noise(x).toFloat
  )
  final def noise4(u: inVec2f) :Vec4f = new Vec4f(
    nx.noise(u.x, u.y).toFloat,
    ny.noise(u.x, u.y).toFloat,
    nz.noise(u.x, u.y).toFloat,
    nw.noise(u.x, u.y).toFloat
  )
  final def noise4(u: inVec3f) :Vec4f = new Vec4f(
    nx.noise(u.x, u.y, u.z).toFloat,
    ny.noise(u.x, u.y, u.z).toFloat,
    nz.noise(u.x, u.y, u.z).toFloat,
    nw.noise(u.x, u.y, u.z).toFloat
  )
  final def noise4(u: inVec4f) :Vec4f = new Vec4f(
    nx.noise(u.x, u.y, u.z, u.w).toFloat,
    ny.noise(u.x, u.y, u.z, u.w).toFloat,
    nz.noise(u.x, u.y, u.z, u.w).toFloat,
    nw.noise(u.x, u.y, u.z, u.w).toFloat
  )
}
