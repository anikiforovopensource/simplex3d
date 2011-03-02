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

import java.nio._


private[math] object CommonMath extends CommonMath


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] class CommonMath {

  // Casting.

  /** Casts a Boolean to a Boolean. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  final def Boolean(x: Boolean) :Boolean = x

  /** Casts an Int to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0, true otherwise.
   */
  final def Boolean(x: Int) :Boolean = (x != 0)

  /** Casts a Float to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  final def Boolean(x: Float) :Boolean = (x != 0)

  /** Casts a Double to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  final def Boolean(x: Double) :Boolean = (x != 0)
  
  final def Boolean(u: AnyVec[_]) :Boolean = u.bx

  /** Casts a Boolean to an Int.
   * @param x a value to cast.
   * @return 1 if x is true, 0 otherwise.
   */
  final def Int(x: Boolean) :Int = if (x) 1 else 0

  /** Casts an Int to an Int. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  final def Int(x: Int) :Int = x

  /** Casts a Float to an Int.
   * @param x a value to cast.
   * @return integer part of x,
   *           Int.MinValue if x <= Int.MinValue,
   *           Int.MaxValue if x >= Int.MaxValue,
   *           possible loss of precision.
   */
  final def Int(x: Float) :Int = x.asInstanceOf[Int]

  /** Casts a Double to an Int.
   * @param x a value to cast.
   * @return integer part of x,
   *           Int.MinValue if x <= Int.MinValue,
   *           Int.MaxValue if x >= Int.MaxValue,
   *           possible loss of precision.
   */
  final def Int(x: Double) :Int = x.asInstanceOf[Int]
  
  final def Int(u: AnyVec[_]) :Int = u.ix

  /** Casts a Boolean to a Float.
   * @param x a value to cast.
   * @return 1.0 if x is true, 0.0 otherwise.
   */
  final def Float(x: Boolean) :Float = if (x) 1 else 0

  /** Casts an Int to a Float.
   * @param x a value to cast.
   * @return x as Float, possible loss of precision.
   */
  final def Float(x: Int) :Float = x.asInstanceOf[Float]

  /** Casts a Float to a Float. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  final def Float(x: Float) :Float = x

  /** Casts a Double to a Float.
   * @param x a value to cast.
   * @return x as Float, possible loss of precision.
   */
  final def Float(x: Double) :Float = x.asInstanceOf[Float]
  
  final def Float(u: AnyVec[_]) :Float = u.fx

  /** Casts a Boolean to a Douoble.
   * @param x a value to cast.
   * @return 1.0 if x is true, 0.0 otherwise.
   */
  final def Double(x: Boolean) :Double = if (x) 1 else 0

  /** Casts an Int to a Douoble.
   * @param x a value to cast.
   * @return x as Double.
   */
  final def Double(x: Int) :Double = x.asInstanceOf[Double]

  /** Casts a Float to a Douoble.
   * @param x a value to cast.
   * @return x as Double.
   */
  final def Double(x: Float) :Double = x.asInstanceOf[Double]

  /** Casts a Double to a Douoble. This method is here for completeness.
   * @param x a value to cast.
   * @return x.
   */
  final def Double(x: Double) :Double = x
  
  final def Double(u: AnyVec[_]) :Double = u.dx


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
  final def not(u: inVec2b) :Vec2b = Vec2b(!u.x, !u.y)


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
  final def not(u: inVec3b) :Vec3b = Vec3b(!u.x, !u.y, !u.z)


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
  final def not(u: inVec4b) :Vec4b = Vec4b(!u.x, !u.y, !u.z, !u.w)
}
