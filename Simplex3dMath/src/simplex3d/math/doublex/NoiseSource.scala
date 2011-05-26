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
  final def noise1(x: Double) :Double = nx.noise(x)
  final def noise1(u: inVec2d) :Double = nx.noise(u.x, u.y)
  final def noise1(u: inVec3d) :Double = nx.noise(u.x, u.y, u.z)
  final def noise1(u: inVec4d) :Double = nx.noise(u.x, u.y, u.z, u.w)

  /** Computes __two__ independent values of a 1-dimensional noise function.
   * @param x a double argument.
   * @return two values of a noise function packed as Vec2.
   */
  final def noise2(x: Double) :Vec2d = new Vec2d(
    nx.noise(x),
    ny.noise(x)
  )
  final def noise2(u: inVec2d) :Vec2d = new Vec2d(
    nx.noise(u.x, u.y),
    ny.noise(u.x, u.y)
  )
  final def noise2(u: inVec3d) :Vec2d = new Vec2d(
    nx.noise(u.x, u.y, u.z),
    ny.noise(u.x, u.y, u.z)
  )
  final def noise2(u: inVec4d) :Vec2d = new Vec2d(
    nx.noise(u.x, u.y, u.z, u.w),
    ny.noise(u.x, u.y, u.z, u.w)
  )

  final def noise3(x: Double) :Vec3d = new Vec3d(
    nx.noise(x),
    ny.noise(x),
    nz.noise(x)
  )
  final def noise3(u: inVec2d) :Vec3d = new Vec3d(
    nx.noise(u.x, u.y),
    ny.noise(u.x, u.y),
    nz.noise(u.x, u.y)
  )
  final def noise3(u: inVec3d) :Vec3d = new Vec3d(
    nx.noise(u.x, u.y, u.z),
    ny.noise(u.x, u.y, u.z),
    nz.noise(u.x, u.y, u.z)
  )
  final def noise3(u: inVec4d) :Vec3d = new Vec3d(
    nx.noise(u.x, u.y, u.z, u.w),
    ny.noise(u.x, u.y, u.z, u.w),
    nz.noise(u.x, u.y, u.z, u.w)
  )

  final def noise4(x: Double) :Vec4d = new Vec4d(
    nx.noise(x),
    ny.noise(x),
    nz.noise(x),
    nw.noise(x)
  )
  final def noise4(u: inVec2d) :Vec4d = new Vec4d(
    nx.noise(u.x, u.y),
    ny.noise(u.x, u.y),
    nz.noise(u.x, u.y),
    nw.noise(u.x, u.y)
  )
  final def noise4(u: inVec3d) :Vec4d = new Vec4d(
    nx.noise(u.x, u.y, u.z),
    ny.noise(u.x, u.y, u.z),
    nz.noise(u.x, u.y, u.z),
    nw.noise(u.x, u.y, u.z)
  )
  final def noise4(u: inVec4d) :Vec4d = new Vec4d(
    nx.noise(u.x, u.y, u.z, u.w),
    ny.noise(u.x, u.y, u.z, u.w),
    nz.noise(u.x, u.y, u.z, u.w),
    nw.noise(u.x, u.y, u.z, u.w)
  )
}
