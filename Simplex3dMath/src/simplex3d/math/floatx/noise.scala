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


/** Various functions to work with noise.
 *
 * @author Aleksey Nikiforov (lex)
 */
object noise {

  // 1/wavelength = 1/sqrt(2)
  final val BaseFrequency = 0.7071067811865475244f

  // Simplex side length * 2
  final val BaseWavelength = 1.4142135623730950488f

  
  /** This class uses 46-bit seed given by the formula: usedSeed = (seed & ((1L << 46) - 1)).
   *
   */
  class SimplexNoise(val seed: Long) {

    private[this] val nx = new SimplexNoiseImpl(seed & ((1L << 46) - 1))
    private[this] val ny = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (1L << 46))
    private[this] val nz = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (2L << 46))
    private[this] val nw = new SimplexNoiseImpl(seed & ((1L << 46) - 1) | (3L << 46))

    /** Computes the value of the 1-dimensional simplex noise function.
     * The simplex noise function is C^2^ continuous (the first and the second derivatives are continuous).
     *
     * The return values of the simplex noise function have the following properties:
     *   - They are in the range [-1, 1].
     *   - They have Gaussian dirstibution.
     *   - The overall average is zero.
     *   - A particular argument will always result in the same return value.
     *   - Statistical properties do not change with rotation and translation of the domain.
     *
     * '''noise1''' function returns zero when the arguments are coordinates of simplex vertices.
     * For 1-dimensional case this means `noise1(N*simplexSide) == 0` for all integral N.
     * Simplex side is the same for all dimensions and is equal to `1/sqrt(2) = 0.7071067811865475244`.
     *
     * @param x a double argument.
     * @return the value of the simplex noise function.
     */
    def noise1(x: Float) :Float = nx.noise(x).toFloat
    def noise1(u: inVec2f) :Float = nx.noise(u.x, u.y).toFloat
    def noise1(u: inVec3f) :Float = nx.noise(u.x, u.y, u.z).toFloat
    def noise1(u: inVec4f) :Float = nx.noise(u.x, u.y, u.z, u.w).toFloat

    /** Computes __two__ independent values of the 1-dimensional simplex noise function.
     * @param x a double argument.
     * @return two values of the simplex noise function packed as Vec2.
     * @see [[simplex3d.math.noise.SimplexNoise.noise1(Float)]]
     */
    def noise2(x: Float) :Vec2f = new Vec2f(
      nx.noise(x).toFloat,
      ny.noise(x).toFloat
    )
    def noise2(u: inVec2f) :Vec2f = new Vec2f(
      nx.noise(u.x, u.y).toFloat,
      ny.noise(u.x, u.y).toFloat
    )
    def noise2(u: inVec3f) :Vec2f = new Vec2f(
      nx.noise(u.x, u.y, u.z).toFloat,
      ny.noise(u.x, u.y, u.z).toFloat
    )
    def noise2(u: inVec4f) :Vec2f = new Vec2f(
      nx.noise(u.x, u.y, u.z, u.w).toFloat,
      ny.noise(u.x, u.y, u.z, u.w).toFloat
    )

    def noise3(x: Float) :Vec3f = new Vec3f(
      nx.noise(x).toFloat,
      ny.noise(x).toFloat,
      nz.noise(x).toFloat
    )
    def noise3(u: inVec2f) :Vec3f = new Vec3f(
      nx.noise(u.x, u.y).toFloat,
      ny.noise(u.x, u.y).toFloat,
      nz.noise(u.x, u.y).toFloat
    )
    def noise3(u: inVec3f) :Vec3f = new Vec3f(
      nx.noise(u.x, u.y, u.z).toFloat,
      ny.noise(u.x, u.y, u.z).toFloat,
      nz.noise(u.x, u.y, u.z).toFloat
    )
    def noise3(u: inVec4f) :Vec3f = new Vec3f(
      nx.noise(u.x, u.y, u.z, u.w).toFloat,
      ny.noise(u.x, u.y, u.z, u.w).toFloat,
      nz.noise(u.x, u.y, u.z, u.w).toFloat
    )

    def noise4(x: Float) :Vec4f = new Vec4f(
      nx.noise(x).toFloat,
      ny.noise(x).toFloat,
      nz.noise(x).toFloat,
      nw.noise(x).toFloat
    )
    def noise4(u: inVec2f) :Vec4f = new Vec4f(
      nx.noise(u.x, u.y).toFloat,
      ny.noise(u.x, u.y).toFloat,
      nz.noise(u.x, u.y).toFloat,
      nw.noise(u.x, u.y).toFloat
    )
    def noise4(u: inVec3f) :Vec4f = new Vec4f(
      nx.noise(u.x, u.y, u.z).toFloat,
      ny.noise(u.x, u.y, u.z).toFloat,
      nz.noise(u.x, u.y, u.z).toFloat,
      nw.noise(u.x, u.y, u.z).toFloat
    )
    def noise4(u: inVec4f) :Vec4f = new Vec4f(
      nx.noise(u.x, u.y, u.z, u.w).toFloat,
      ny.noise(u.x, u.y, u.z, u.w).toFloat,
      nz.noise(u.x, u.y, u.z, u.w).toFloat,
      nw.noise(u.x, u.y, u.z, u.w).toFloat
    )
  }
}
