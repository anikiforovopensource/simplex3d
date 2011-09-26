/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d

import simplex3d.math.types._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object math {

  // Implicits
  implicit def toBooleanToRef(s: Boolean) :ReadBooleanRef = new BooleanRef(s)
  implicit def refToBoolean(r: ReadBooleanRef) = r.toConst

  implicit def toIntToRef(s: Int) :ReadIntRef = new IntRef(s)
  implicit def refToInt(r: ReadIntRef) = r.toConst
  

  // In aliases

  /** <code>in</code> prefix for Vec2b.
   * Use the prefix when declaring functions.
   */
  type inVec2b = ReadVec2b

  /** <code>in</code> prefix for Vec3b.
   * Use the prefix when declaring functions.
   */
  type inVec3b = ReadVec3b

  /** <code>in</code> prefix for Vec4b.
   * Use the prefix when declaring functions.
   */
  type inVec4b = ReadVec4b


  type inVec2i = ReadVec2i
  type inVec3i = ReadVec3i
  type inVec4i = ReadVec4i


  // Matrix aliases
  type AnyMat2x2[P] = AnyMat2[P]
  type AnyMat3x3[P] = AnyMat3[P]
  type AnyMat4x4[P] = AnyMat4[P]

  
  // Casting.

  /** Casts a Boolean to a Boolean. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  @inline final def toBoolean(x: Boolean) :Boolean = x

  /** Casts an Int to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0, true otherwise.
   */
  @inline final def toBoolean(x: Int) :Boolean = (x != 0)

  /** Casts a Float to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  @inline final def toBoolean(x: Float) :Boolean = (x != 0)

  /** Casts a Double to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  @inline final def toBoolean(x: Double) :Boolean = (x != 0)

  @inline final def toBoolean(u: AnyVec[_]) :Boolean = u.bx

  /** Casts a Boolean to an Int.
   * @param x a value to cast.
   * @return 1 if x is true, 0 otherwise.
   */
  @inline final def toInt(x: Boolean) :Int = if (x) 1 else 0

  /** Casts an Int to an Int. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  @inline final def toInt(x: Int) :Int = x

  /** Casts a Float to an Int.
   * @param x a value to cast.
   * @return integer part of x,
   *           Int.MinValue if x <= Int.MinValue,
   *           Int.MaxValue if x >= Int.MaxValue,
   *           possible loss of precision.
   */
  @inline final def toInt(x: Float) :Int = x.asInstanceOf[Int]

  /** Casts a Double to an Int.
   * @param x a value to cast.
   * @return integer part of x,
   *           Int.MinValue if x <= Int.MinValue,
   *           Int.MaxValue if x >= Int.MaxValue,
   *           possible loss of precision.
   */
  @inline final def toInt(x: Double) :Int = x.asInstanceOf[Int]

  @inline final def toInt(u: AnyVec[_]) :Int = u.ix

  /** Casts a Boolean to a Float.
   * @param x a value to cast.
   * @return 1.0 if x is true, 0.0 otherwise.
   */
  @inline final def toFloat(x: Boolean) :Float = if (x) 1 else 0

  /** Casts an Int to a Float.
   * @param x a value to cast.
   * @return x as Float, possible loss of precision.
   */
  @inline final def toFloat(x: Int) :Float = x.asInstanceOf[Float]

  /** Casts a Float to a Float. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  @inline final def toFloat(x: Float) :Float = x

  /** Casts a Double to a Float.
   * @param x a value to cast.
   * @return x as Float, possible loss of precision.
   */
  @inline final def toFloat(x: Double) :Float = x.asInstanceOf[Float]

  @inline final def toFloat(u: AnyVec[_]) :Float = u.fx

  /** Casts a Boolean to a Douoble.
   * @param x a value to cast.
   * @return 1.0 if x is true, 0.0 otherwise.
   */
  @inline final def toDouble(x: Boolean) :Double = if (x) 1 else 0

  /** Casts an Int to a Douoble.
   * @param x a value to cast.
   * @return x as Double.
   */
  @inline final def toDouble(x: Int) :Double = x.asInstanceOf[Double]

  /** Casts a Float to a Douoble.
   * @param x a value to cast.
   * @return x as Double.
   */
  @inline final def toDouble(x: Float) :Double = x.asInstanceOf[Double]

  /** Casts a Double to a Douoble. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  @inline final def toDouble(x: Double) :Double = x

  @inline final def toDouble(u: AnyVec[_]) :Double = u.dx


  // Boolean functions.

  // Vec2b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  final def any(u: inVec2b) :Boolean = {
    u.x || u.y
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  final def all(u: inVec2b) :Boolean = {
    u.x && u.y
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  final def not(u: inVec2b) :Vec2b = new Vec2b(!u.x, !u.y)


  // Vec3b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  final def any(u: inVec3b) :Boolean = {
    u.x || u.y || u.z
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  final def all(u: inVec3b) :Boolean = {
    u.x && u.y && u.z
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  final def not(u: inVec3b) :Vec3b = new Vec3b(!u.x, !u.y, !u.z)


  // Vec4b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  final def any(u: inVec4b) :Boolean = {
    u.x || u.y || u.z || u.w
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  final def all(u: inVec4b) :Boolean = {
    u.x && u.y && u.z && u.w
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  final def not(u: inVec4b) :Vec4b = new Vec4b(!u.x, !u.y, !u.z, !u.w)
}
