/*
 * Simplex3dAlgorithm - Noise Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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


/** Noise Adapter for Vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
@SerialVersionUID(8104346712419693669L)
class Noise1(val gen: NoiseGen) extends Serializable {

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
  final def apply(x: Double) :Double = gen(x)
  final def apply(u: inVec2d) :Double = gen(u.x, u.y)
  final def apply(u: inVec3d) :Double = gen(u.x, u.y, u.z)
  final def apply(u: inVec4d) :Double = gen(u.x, u.y, u.z, u.w)
}

@SerialVersionUID(8104346712419693669L)
class Noise2(val gen: NoiseGen) extends Serializable {
  protected val channel1 = gen
  protected val channel2 = gen.reseed(gen.seed ^ -4959463499243013640L)

  final def apply(x: Double) :Vec2d = Vec2d(
    channel1(x),
    channel2(x)
  )
  final def apply(u: inVec2d) :Vec2d = Vec2d(
    channel1(u.x, u.y),
    channel2(u.x, u.y)
  )
  final def apply(u: inVec3d) :Vec2d = Vec2d(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z)
  )
  final def apply(u: inVec4d) :Vec2d = Vec2d(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w)
  )
}

@SerialVersionUID(8104346712419693669L)
class Noise3(val gen: NoiseGen) extends Serializable {
  protected val channel1 = gen
  protected val channel2 = gen.reseed(gen.seed ^ -4959463499243013640L)
  protected val channel3 = gen.reseed(gen.seed ^ -1817970389778669801L)

  final def apply(x: Double) :Vec3d = Vec3d(
    channel1(x),
    channel2(x),
    channel3(x)
  )
  final def apply(u: inVec2d) :Vec3d = Vec3d(
    channel1(u.x, u.y),
    channel2(u.x, u.y),
    channel3(u.x, u.y)
  )
  final def apply(u: inVec3d) :Vec3d = Vec3d(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z),
    channel3(u.x, u.y, u.z)
  )
  final def apply(u: inVec4d) :Vec3d = Vec3d(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w),
    channel3(u.x, u.y, u.z, u.w)
  )
}

@SerialVersionUID(8104346712419693669L)
class Noise4(val gen: NoiseGen) extends Serializable {
  protected val channel1 = gen
  protected val channel2 = gen.reseed(gen.seed ^ -4959463499243013640L)
  protected val channel3 = gen.reseed(gen.seed ^ -1817970389778669801L)
  protected val channel4 = gen.reseed(gen.seed ^ 9164759175887871693L)

  final def apply(x: Double) :Vec4d = Vec4d(
    channel1(x),
    channel2(x),
    channel3(x),
    channel4(x)
  )
  final def apply(u: inVec2d) :Vec4d = Vec4d(
    channel1(u.x, u.y),
    channel2(u.x, u.y),
    channel3(u.x, u.y),
    channel4(u.x, u.y)
  )
  final def apply(u: inVec3d) :Vec4d = Vec4d(
    channel1(u.x, u.y, u.z),
    channel2(u.x, u.y, u.z),
    channel3(u.x, u.y, u.z),
    channel4(u.x, u.y, u.z)
  )
  final def apply(u: inVec4d) :Vec4d = Vec4d(
    channel1(u.x, u.y, u.z, u.w),
    channel2(u.x, u.y, u.z, u.w),
    channel3(u.x, u.y, u.z, u.w),
    channel4(u.x, u.y, u.z, u.w)
  )
}
