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

import simplex3d.math.doublex._


/** Noise source.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
class Noise(val source: NoiseSource) extends Serializable {

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
  final def apply(x: Double) :Double = source(x)
  final def apply(u: inVec2d) :Double = source(u.x, u.y)
  final def apply(u: inVec3d) :Double = source(u.x, u.y, u.z)
  final def apply(u: inVec4d) :Double = source(u.x, u.y, u.z, u.w)
}
