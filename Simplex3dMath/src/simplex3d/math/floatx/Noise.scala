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
class Noise(val seed: Long, val sourceFactory: NoiseFactory = ClassicalGradientNoise) {

  private[math] final val channel1 = sourceFactory(seed & ((1L << 46) - 1))
  private[math] final val channel2 = sourceFactory(seed & ((1L << 46) - 1) | (1L << 46))
  private[math] final val channel3 = sourceFactory(seed & ((1L << 46) - 1) | (2L << 46))
  private[math] final val channel4 = sourceFactory(seed & ((1L << 46) - 1) | (3L << 46))

  
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
  final def noise1(x: Float) :Float = channel1(x)
  final def noise1(u: inVec2f) :Float = channel1(u.x, u.y)
  final def noise1(u: inVec3f) :Float = channel1(u.x, u.y, u.z)
  final def noise1(u: inVec4f) :Float = channel1(u.x, u.y, u.z, u.w)

  /** Computes __two__ independent values of a 1-dimensional noise function.
   * @param x a double argument.
   * @return two values of a noise function packed as Vec2.
   */
  final def noise2(x: Float) :Vec2f = new Vec2f(
    channel1(x),
    channel2(x)
  )
  final def noise2(u: inVec2f) :Vec2f = new Vec2f(
    channel1(u.x, u.y),
    channel2(u.x, u.y)
  )
  final def noise2(u: inVec3f) :Vec2f = new Vec2f(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z)
  )
  final def noise2(u: inVec4f) :Vec2f = new Vec2f(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w)
  )

  final def noise3(x: Float) :Vec3f = new Vec3f(
    channel1(x),
    channel2(x),
    channel3(x)
  )
  final def noise3(u: inVec2f) :Vec3f = new Vec3f(
    channel1(u.x, u.y),
    channel2(u.x, u.y),
    channel3(u.x, u.y)
  )
  final def noise3(u: inVec3f) :Vec3f = new Vec3f(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z),
    channel3(u.x, u.y, u.z)
  )
  final def noise3(u: inVec4f) :Vec3f = new Vec3f(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w),
    channel3(u.x, u.y, u.z, u.w)
  )

  final def noise4(x: Float) :Vec4f = new Vec4f(
    channel1(x),
    channel2(x),
    channel3(x),
    channel4(x)
  )
  final def noise4(u: inVec2f) :Vec4f = new Vec4f(
    channel1(u.x, u.y),
    channel2(u.x, u.y),
    channel3(u.x, u.y),
    channel4(u.x, u.y)
  )
  final def noise4(u: inVec3f) :Vec4f = new Vec4f(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z),
    channel3(u.x, u.y, u.z),
    channel4(u.x, u.y, u.z)
  )
  final def noise4(u: inVec4f) :Vec4f = new Vec4f(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w),
    channel3(u.x, u.y, u.z, u.w),
    channel4(u.x, u.y, u.z, u.w)
  )
}
