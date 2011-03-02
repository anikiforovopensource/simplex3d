/*
 * Simplex3d, CoreMath module
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

package simplex3d.math


/** The <code>ExtendedInt</code> class encapsulates glue code to make Ints
 * interact with Int vectors.
 * <p>
 *   Instances of this class are produces via implicit conversions from Int
 *   when required.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {

  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec2i) = u*value

  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec3i) = u*value

  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec4i) = u*value

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x and s/u.y.
   */
  def /(u: inVec2i) = u.divByComp(value)

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x, s/u.y, and s/u.z.
   */
  def /(u: inVec3i) = u.divByComp(value)

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x, s/u.y, s/u.z, and s/u.w.
   */
  def /(u: inVec4i) = u.divByComp(value)

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x and s + u.y.
   */
  def +(u: inVec2i) = u + value

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x, s + u.y, and s + u.z.
   */
  def +(u: inVec3i) = u + value

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x, s + u.y, s + u.z, and s + u.w.
   */
  def +(u: inVec4i) = u + value

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x and s - u.y.
   */
  def -(u: inVec2i) =
    new Vec2i(value - u.x, value - u.y)

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x, s - u.y, and s - u.z.
   */
  def -(u: inVec3i) =
    new Vec3i(value - u.x, value - u.y, value - u.z)

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x, s - u.y, s - u.z, and s - u.w.
   */
  def -(u: inVec4i) =
    new Vec4i(value - u.x, value - u.y, value - u.z, value - u.w)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x and s % u.y.
   */
  def %(u: inVec2i) = u.remByComp(value)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x, s % u.y, and s % u.z.
   */
  def %(u: inVec3i) = u.remByComp(value)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x, s % u.y, s % u.z, and s % u.w.
   */
  def %(u: inVec4i) = u.remByComp(value)

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x and s & u.y.
   */
  def &(u: inVec2i) = u & value

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x, s & u.y, and s & u.z.
   */
  def &(u: inVec3i) = u & value

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x, s & u.y, s & u.z, and s & u.w.
   */
  def &(u: inVec4i) = u & value

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x and s | u.y.
   */
  def |(u: inVec2i) = u | value

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x, s | u.y, and s | u.z.
   */
  def |(u: inVec3i) = u | value

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x, s | u.y, s | u.z, and s | u.w.
   */
  def |(u: inVec4i) = u | value

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x and s ^ u.y.
   */
  def ^(u: inVec2i) = u ^ value

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x, s ^ u.y, and s ^ u.z.
   */
  def ^(u: inVec3i) = u ^ value

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x, s ^ u.y, s ^ u.z, and s ^ u.w.
   */
  def ^(u: inVec4i) = u ^ value
}
