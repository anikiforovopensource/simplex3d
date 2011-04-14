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

import simplex3d.math.CommonMath._


/** The <code>ExtendedInt</code> class encapsulates glue code to make Ints
 * interact with Int vectors.
 * <p>
 *   Instances of this class are produces via implicit conversions from Int
 *   when required.
 * </p>
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class ReadIntRef(protected var x: Int) extends PrimitiveRef[Int] {
  type Read = ReadIntRef
  type Const = Int
  def toConst() :Int = x

  def apply(i: Int) :Int = {
    if (i == 0) x
    else throw new IndexOutOfBoundsException("Expected from 0 to 0, got " + i + ".")
  }

  private[math] def bx: Boolean = Boolean(x)
  private[math] def ix: Int = x
  private[math] def fx: Float = x
  private[math] def dx: Double = x


  final override def equals(other: Any) :Boolean = {
    other match {
      case r: ReadIntRef => x == r.toConst
      case r: ReadBooleanRef => false
      case r: PrimitiveRef[_] => dx == r.dx
      case a => x == a
    }
  }

  final override def hashCode() :Int = {
    x.hashCode
  }

  final override def toString() :String = {
    "IntRef" + "(" + x + ")"
  }


  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec2i) = u*x

  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec3i) = u*x

  /** Multiplies this scalar by a vector.
   * @param u a vector to multiply by.
   * @return u*scalar.
   */
  def *(u: inVec4i) = u*x

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x and s/u.y.
   */
  def /(u: inVec2i) = u.divByComp(x)

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x, s/u.y, and s/u.z.
   */
  def /(u: inVec3i) = u.divByComp(x)

  /** Divides this scalar by a vector.
   * @param u a vector to divide by.
   * @return a vector with components s/u.x, s/u.y, s/u.z, and s/u.w.
   */
  def /(u: inVec4i) = u.divByComp(x)

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x and s + u.y.
   */
  def +(u: inVec2i) = u + x

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x, s + u.y, and s + u.z.
   */
  def +(u: inVec3i) = u + x

  /** Adds this scalar to each component of a vector.
   * @param u a vector to add to.
   * @return a vector with components s + u.x, s + u.y, s + u.z, and s + u.w.
   */
  def +(u: inVec4i) = u + x

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x and s - u.y.
   */
  def -(u: inVec2i) = new Vec2i(x - u.x, x - u.y)

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x, s - u.y, and s - u.z.
   */
  def -(u: inVec3i) = new Vec3i(x - u.x, x - u.y, x - u.z)

  /** Subtracts each component of a vector from this scalar.
   * @param u a vector to subtract.
   * @return a vector with components s - u.x, s - u.y, s - u.z, and s - u.w.
   */
  def -(u: inVec4i) = new Vec4i(x - u.x, x - u.y, x - u.z, x - u.w)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x and s % u.y.
   */
  def %(u: inVec2i) = u.remByComp(x)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x, s % u.y, and s % u.z.
   */
  def %(u: inVec3i) = u.remByComp(x)

  /** Computes remainders of divisions of this scalar
   * by each component of a vector.
   *
   * @param u a vector to divide by.
   * @return a vector with components s % u.x, s % u.y, s % u.z, and s % u.w.
   */
  def %(u: inVec4i) = u.remByComp(x)

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x and s & u.y.
   */
  def &(u: inVec2i) = u & x

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x, s & u.y, and s & u.z.
   */
  def &(u: inVec3i) = u & x

  /** Computes bitwise AND of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s & u.x, s & u.y, s & u.z, and s & u.w.
   */
  def &(u: inVec4i) = u & x

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x and s | u.y.
   */
  def |(u: inVec2i) = u | x

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x, s | u.y, and s | u.z.
   */
  def |(u: inVec3i) = u | x

  /** Computes bitwise OR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s | u.x, s | u.y, s | u.z, and s | u.w.
   */
  def |(u: inVec4i) = u | x

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x and s ^ u.y.
   */
  def ^(u: inVec2i) = u ^ x

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x, s ^ u.y, and s ^ u.z.
   */
  def ^(u: inVec3i) = u ^ x

  /** Computes bitwise XOR of this scalar with each component of a vector.
   * @param u a vector.
   * @return a vector with components s ^ u.x, s ^ u.y, s ^ u.z, and s ^ u.w.
   */
  def ^(u: inVec4i) = u ^ x
}

final class IntRef(cx: Int) extends ReadIntRef(cx) with PropertyRef {
  type Clone = IntRef
  override def clone() = new IntRef(x)
  
  def :=(s: Int) { x = s }
  def :=(r: ReadIntRef) { x = r.toConst }

  
  def *=(s: Int) { x *= s }
  def /=(s: Int) { x /= s }
  def +=(s: Int) { x += s }
  def -=(s: Int) { x -= s }

  def %=(s: Int) { x %= s }
  def >>=(s: Int) { x >>= s }
  def >>>=(s: Int) { x >>>= s }
  def <<=(s: Int) { x <<= s }
  def &=(s: Int) { x &= s }
  def |=(s: Int) { x |= s }
  def ^=(s: Int) { x ^= s }
}

object IntRef {
  def unapply(r: ReadIntRef) = Some(r.toConst)
  implicit def toMutable(r: ReadIntRef) = new IntRef(r.toConst)
  implicit def toMutable(x: Int) = new IntRef(x)
}
