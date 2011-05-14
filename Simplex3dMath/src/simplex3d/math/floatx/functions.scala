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
import simplex3d.math.SimplexNoise._
import java.lang.{Math => JMath}


// An empty class to make -Xno-forwarders work
private[math] class functions


/**
 * @author Aleksey Nikiforov (lex)
 */
object functions extends CommonMath {

  // Constants
  /** Machine float epsilon.
   */
  final val Epsilon = 1.19209e-7f
  
  /** Constant ''pi''.
   */
  final val Pi = 3.14159265358979323846f
  
  /** Constant ''e''.
   */
  final val E = 2.71828182845904523536f

  private final val DegToRad = 0.01745329251994329577f
  private final val RadToDeg = 57.2957795130823208768f
  private final val InvLog2 = 1.44269504088896340736

  private final val MaxIntegralFp = 8388608f
  private final val MinIntegralFp = -8388608f
  

  // Copied here until the scala compiler can resolve inherited overloaded functions.
  // Int functions

  /** Returns an absolute value of the argument.
   * @param x an integer argument.
   * @return an absolute value of the agument.
   */
  final def abs(x: Int) :Int = if (x < 0) -x else x

  /** Returns the sign of the argument.
   * @param x an integer argument.
   * @return one of the following:
   *   - -1 if ''x'' < 0.
   *   - 1 if ''x'' > 0.
   *   - 0 otherwise.
   */
  final def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0

  /** Returns the smaller one of two arguments.
   * @param x an integer argument.
   * @param y an integer argument.
   * @return the smallest argument.
   */
  final def min(x: Int, y: Int) :Int = if (x < y) x else y

  /** Returns the larger one of two arguments.
   * @param x an integer argument.
   * @param y an integer argument.
   * @return the largest argument.
   */
  final def max(x: Int, y: Int) :Int = if (x > y) x else y

  /** Clamps a value to a given range.
   * __Note:__ the result is undefined if ''minVal'' > ''maxVal''.
   * @param x an integer value to clamp.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return one of the following:
   *   - ''minVal'' if ''x'' < ''minVal''.
   *   - ''maxVal'' if ''x'' > ''maxVal''.
   *   - ''x'' otherwise.
   */
  final def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
    if (x <= minVal) minVal
    else if (x >= maxVal) maxVal
    else x
  }

  // Vec2i functions
  /** Applies '''abs''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of ''u''.
   */
  final def abs(u: inVec2i) :Vec2i = new Vec2i(abs(u.x), abs(u.y))
  
  /** Applies '''sign''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `sign(u.c)` for each component ''c''.
   */
  final def sign(u: inVec2i) :Vec2i = new Vec2i(sign(u.x), sign(u.y))
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `min(c, s)` for each component ''c''.
   */
  final def min(u: inVec2i, s: Int) :Vec2i = new Vec2i(min(u.x, s), min(u.y, s))
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `min(u.c, v.c)` for each component ''c''.
   */
  final def min(u: inVec2i, v: inVec2i) :Vec2i = new Vec2i(min(u.x, v.x), min(u.y, v.y))
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `max(c, s)` for each component ''c''.
   */
  final def max(u: inVec2i, s: Int) :Vec2i = new Vec2i(max(u.x, s), max(u.y, s))
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `max(u.c, v.c)` for each component ''c''.
   */
  final def max(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(max(u.x, v.x), max(u.y, v.y))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [''minValue'', ''maxValue''].
   */
  final def clamp(u: inVec2i, minVal: Int, maxVal: Int) :Vec2i = {
    new Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  
  /** Applies '''clamp''' function to each component.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to `clamp(u.c, minVal.c, maxVal.c)` for each component ''c''.
   */
  final def clamp(u: inVec2i, minVal: inVec2i, maxVal: inVec2i) :Vec2i = {
    new Vec2i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }
  
  final def length(u: inVec2i) = sqrt(u.x*u.x + u.y*u.y)

  /** Performs component-wise '''less than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c < v.c)` for each component ''c''.
   */
  final def lessThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  
  /** Performs component-wise '''less than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c <= v.c)` for each component ''c''.
   */
  final def lessThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  
  /** Performs component-wise '''greater than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c > v.c)` for each component ''c''.
   */
  final def greaterThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  
  /** Performs component-wise '''greater than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c >= v.c)` for each component ''c''.
   */
  final def greaterThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  
  /** Performs component-wise '''equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c == v.c)` for each component ''c''.
   */
  final def equal(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  
  /** Performs component-wise '''not equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c != v.c)` for each component ''c''.
   */
  final def notEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  // Vec3i functions
  /** Applies '''abs''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of ''u''.
   */
  final def abs(u: inVec3i) :Vec3i = new Vec3i(abs(u.x), abs(u.y), abs(u.z))
  
  /** Applies '''sign''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `sign(u.c)` for each component ''c''.
   */
  final def sign(u: inVec3i) :Vec3i = new Vec3i(sign(u.x), sign(u.y), sign(u.z))
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `min(c, s)` for each component ''c''.
   */
  final def min(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `min(u.c, v.c)` for each component ''c''.
   */
  final def min(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `max(c, s)` for each component ''c''.
   */
  final def max(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `max(u.c, v.c)` for each component ''c''.
   */
  final def max(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [''minValue'', ''maxValue''].
   */
  final def clamp(u: inVec3i, minVal: Int, maxVal: Int) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  
  /** Applies '''clamp''' function to each component.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to `clamp(u.c, minVal.c, maxVal.c)` for each component ''c''.
   */
  final def clamp(u: inVec3i, minVal: inVec3i, maxVal: inVec3i) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }
  
  final def length(u: inVec3i) = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)

    /** Performs component-wise '''less than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c < v.c)` for each component ''c''.
   */
  final def lessThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  
  /** Performs component-wise '''less than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c <= v.c)` for each component ''c''.
   */
  final def lessThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  
  /** Performs component-wise '''greater than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c > v.c)` for each component ''c''.
   */
  final def greaterThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  
  /** Performs component-wise '''greater than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c >= v.c)` for each component ''c''.
   */
  final def greaterThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  
  /** Performs component-wise '''equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c == v.c)` for each component ''c''.
   */
  final def equal(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  
  /** Performs component-wise '''not equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c != v.c)` for each component ''c''.
   */
  final def notEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  // Vec4i functions
  /** Applies '''abs''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of ''u''.
   */
  final def abs(u: inVec4i) :Vec4i = {
    new Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  
  /** Applies '''sign''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `sign(u.c)` for each component ''c''.
   */
  final def sign(u: inVec4i) :Vec4i = {
    new Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `min(c, s)` for each component ''c''.
   */
  final def min(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  
  /** Applies '''min''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `min(u.c, v.c)` for each component ''c''.
   */
  final def min(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @return a vector with components set to `max(c, s)` for each component ''c''.
   */
  final def max(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  
  /** Applies '''max''' function to each component.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to `max(u.c, v.c)` for each component ''c''.
   */
  final def max(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [''minValue'', ''maxValue''].
   */
  final def clamp(u: inVec4i, minVal: Int, maxVal: Int) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  
  /** Applies '''clamp''' function to each component.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to `clamp(u.c, minVal.c, maxVal.c)` for each component ''c''.
   */
  final def clamp(u: inVec4i, minVal: inVec4i, maxVal: inVec4i) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }
  
  final def length(u: inVec4i) = sqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)

  /** Performs component-wise '''less than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c < v.c)` for each component ''c''.
   */
  final def lessThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  
  /** Performs component-wise '''less than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c <= v.c)` for each component ''c''.
   */
  final def lessThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  
  /** Performs component-wise '''greater than''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c > v.c)` for each component ''c''.
   */
  final def greaterThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  
  /** Performs component-wise '''greater than or equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c >= v.c)` for each component ''c''.
   */
  final def greaterThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  
  /** Performs component-wise '''equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c == v.c)` for each component ''c''.
   */
  final def equal(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  
  /** Performs component-wise '''not equal''' comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to `(u.c != v.c)` for each component ''c''.
   */
  final def notEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }


  // Float functions
  /** Converts degress to radians.
   * @param x an angle (in degrees).
   * @return the argument converted to radians.
   */
  def radians(x: Float) :Float = x*DegToRad
  
  /** Converts radians to degrees.
   * @param x an angle (in radians).
   * @return the argument converted to degrees.
   */
  def degrees(x: Float) :Float = x*RadToDeg

  /** Computes the value of the trigonometric sine function.
   * @param x an angle (in radians).
   * @return the sine of the argument.
   */
  def sin(w: Float) :Float = JMath.sin(w).toFloat
  
  /** Computes the value of the trigonometric cosine function.
   * @param x an angle (in radians).
   * @return the cosine of the argument.
   */
  def cos(w: Float) :Float = JMath.cos(w).toFloat
  
  /** Computes the value of the trigonometric tangent function.
   * @param x an angle (in radians).
   * @return the tangent of ''x''.
   */
  def tan(w: Float) :Float = JMath.tan(w).toFloat

  /** Computes the value of the inverse sine function. Sine function has a range of [-1, 1],
   * so only the values from this range have meaningful results.
   * @param x a float argument.
   * @return one of the following:
   *   - an angle (in radians) whose sine is ''x'' if ''x'' is in [-1, 1].
   *   - NaN otherwise.
   */
  def asin(w: Float) :Float = JMath.asin(w).toFloat
  
  /** Computes the value of the inverse cosine function. Cosine function has a range of [-1, 1],
   * so only the values from this range have meaningful results.
   * @param x a float argument.
   * @return one of the following:
   *   - an angle (in radians) whose cosine is ''x'' if ''x'' is in [-1, 1].
   *   - NaN otherwise.
   */
  def acos(w: Float) :Float = JMath.acos(w).toFloat
  
  /** Computes the value of the inverse tangent function.
   * The resulting value ''theta'' is in range [-''pi'', ''pi'']. ''Theta'' is
   * the angular component of polar coordinates (''r'', ''theta'') which correspond to
   * the point (''x'', ''y'') in Caretesian coordinates.
   * @param y coordinate.
   * @param x coordinate.
   * @return one of the following:
   *   - an angle (in radians) whose tangent is `y/x` if both ''x'' and ''y'' are non-zero.
   *   - 0 otherwise.
   */
  def atan(y: Float, x: Float) :Float = JMath.atan2(y, x).toFloat
  
  /** Computes the value of the inverse tangent function.
   * The resulting value is in range [-''pi''/2, ''pi''/2].
   * @param w a float argument.
   * @return an angle (in radians) whose tangent is ''w''.
   */
  def atan(w: Float) :Float = JMath.atan(w).toFloat

  /** Computes the value of the hyperbolic sine function.
   * @param x a float argument.
   * @return the hyperbolic sine of the argument.
   */
  def sinh(x: Float) :Float = JMath.sinh(x).toFloat
  
  /** Computes the value of the hyperbolic cosine function.
   * @param x a float argument.
   * @return the hyperbolic cosine of the argument.
   */
  def cosh(x: Float) :Float = JMath.cosh(x).toFloat
  
  /** Computes the value of the hyperbolic tangent function.
   * @param x a float argument.
   * @return the hyperbolic tangent of the argument.
   */
  def tanh(x: Float) :Float = JMath.tanh(x).toFloat

  /** Computes the value of the inverse hyperbolic sine function.
   * @param x a float argument.
   * @return a value whose hyperbolic sine is ''x''.
   */
  def asinh(x: Float) :Float = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) (-JMath.log(-x + JMath.sqrt(x*x + 1))).toFloat
    else (JMath.log(x + JMath.sqrt(x*x + 1))).toFloat
  }
  
  /** Computes the value of the inverse hyperbolic cosine function. This function is defined
   * from 1 (inclusinve) to `+`''infinity''.
   * @param x a float argument.
   * @return one of the following:
   *   - a value whose hyperbolic cosine is ''x'' if ''x'' >= 1.
   *   - NaN otherwise.
   */
  def acosh(x: Float) :Float = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) scala.Float.NaN
    else (JMath.log(x + JMath.sqrt(x*x - 1))).toFloat
  }
  
  /** Computes the value of the inverse hyperbolic tangent function. This function is defined
   * from -1 (inclusinve) to 1 (inclusive).
   * @param x a float argument.
   * @return one of the following:
   *   - a value whose hyperbolic tangent is ''x'' if ''x'' is in range (-1, 1).
   *   - `-`''infinity'' if ''x'' is -1.
   *   - `+`''infinity'' if ''x'' is 1.
   *   - NaN otherwise.
   */
  def atanh(x: Float) :Float = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) -atanh(-x)
    else if (x < 0.2) (0.5*JMath.log1p(2*x/(1 - x))).toFloat
    else (0.5*JMath.log((1 + x)/(1 - x))).toFloat
  }

  /** Computes the value of the first argument raised to the power of the second.
   * @param x the value of the base.
   * @param y the value of the exponent.
   * @return one of the following:
   *   - NaN if ''x'' < 0 and ''y'' is not an integer.
   *   - `x`^y^ otherwise.
   */
  def pow(x: Float, y: Float) :Float = JMath.pow(x, y).toFloat
  
  /** Computes the value of the exponential function.
   * @param x the value of the exponent.
   * @return `e`^x^, where ''e'' is the base of the natural logarithm.
   */
  def exp(x: Float) :Float = JMath.exp(x).toFloat
  
  /** Computes the value of the natural logarithm.
   * @param x a float argument.
   * @return one of the following:
   *   - natural logarithm of ''x'' if ''x'' > 0.
   *   - `-`''infinity'' if ''x'' == 0.
   *   - NaN otherwise.
   */
  def log(x: Float) :Float = JMath.log(x).toFloat

  /** Computes the value of 2 raised to the value of the argument.
   * @param x the value of the exponent.
   * @return `2`^x^.
   */
  def exp2(x: Float) :Float = JMath.pow(2, x).toFloat
  
  /** Computes the value of base 2 logarithm.
   * @param x a float argument.
   * @return one of the following:
   *   - base 2 logarithm of ''x'' if ''x'' > 0.
   *   - `-`''infinity'' if ''x'' == 0.
   *   - NaN otherwise.
   */
  def log2(x: Float) :Float = (JMath.log(x)*InvLog2).toFloat

  /** Computes the value of the square root.
   * @param x a float argument.
   * @return one of the following:
   *   - `√x` if ''x'' >= 0.
   *   - NaN otherwise.
   */
  def sqrt(s: Float) :Float = JMath.sqrt(s).toFloat
  
  /** Computes the value of the multiplicative inverse of the square root.
   * @param x a float argument.
   * @return one of the following:
   *   - `1/√x` if ''x'' > 0.
   *   - `-`''infinity'' if ''x'' is -0.
   *   - `+`''infinity'' if ''x'' is +0.
   *   - NaN otherwise.
   */
  def inversesqrt(s: Float) :Float = (1/JMath.sqrt(s)).toFloat

  /** Returns the absolute value of the argument.
   * @param x a float argument.
   * @return an absolute value of the agument.
   */
  def abs(x: Float) :Float = { if (x > 0) x else if (x == 0) 0 else -x }
  
  /** Returns the sign of the argument.
   * @param x a float argument.
   * @return one of the following:
   *   - -1 if ''x'' < 0.
   *   - 1 if ''x'' > 0.
   *   - 0 otherwise.
   */
  def sign(x: Float) :Float = {
    if (x > 0) 1
    else if (x < 0) -1
    else x // preserves nan and -0
  }
  
  /** Returns the nearest integral number less than the argument.
   * @param x a float argument.
   * @return float representation of the nearest integral number less than the argument.
   */
  def floor(x: Float) :Float = {
    if (x > 0) {
      if (x > MaxIntegralFp) x
      else {
        x.toInt
      }
    }
    else if (x < 0) {
      if (x < MinIntegralFp) x
      else {
        val i = x.toInt
        if (x == i) x else i - 1
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  
  /** Returns the integral part of the argument, discarding the fractional part.
   * @param x a float argument.
   * @return float representation of the integral part of the argument.
   */
  def trunc(x: Float) :Float = {
    if (x > 0) {
      if (x > MaxIntegralFp) x
      else if (x >= 1) x.toInt
      else 0
    }
    else if (x < 0) {
      if (x < MinIntegralFp) x
      else if (x <= -1) x.toInt
      else -0f
    }
    else {
      x // preserves nan and -0
    }
  }
  
  /** Rounds the argument to the nearest integral number. If the fractional part of the argument is 0.5 then
   * the result is rounded towards the larger integer.
   * @param x a float argument.
   * @return float representation of an integral number nearest to the argument.
   */
  def round(x: Float) :Float = {
    if (x > 0) {
      if (x > MaxIntegralFp) x
      else {
        (x + 0.5f).toInt
      }
    }
    else if (x < 0) {
      if (x < MinIntegralFp) x
      else if (x >= -0.5f) -0f
      else {
        val f = x + 0.5f
        val i = f.toInt
        if (f == i) f else i - 1
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  
  /** Rounds the argument to the nearest integral number. If the fractional part of the argument is 0.5 then
   * the result is rounded towards the even integer.
   * @param x a float argument.
   * @return float representation of an integral number nearest to the argument.
   */
  def roundEven(x: Float) :Float = JMath.rint(x).toFloat
  def ceil(x: Float) :Float = {
    if (x > 0) {
      if (x > MaxIntegralFp) x
      else {
        val i = x.toInt
        if (x == i) x else i + 1
      }
    }
    else if (x < 0) {
      if (x < MinIntegralFp) x
      else if (x > -1) -0f
      else {
        x.toInt
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  
  /** Returns the fractional part dicarded by the floor function.
   * @param x a float argument
   * @return `x - floor(x)`.
   */
  def fract(x: Float) :Float = {
    if (isnegzero(x)) -0f
    else if (isinf(x)) 0
    else x - floor(x)
  }
  
  /** Returns `x mod y`. Results of this function are different from scala % operator
   * when only one of the arguments is negative.
   * @param x a float argument
   * @return `x - y*floor(x/y)`.
   */
  def mod(x: Float, y: Float) :Float = {
    if (isinf(x)) scala.Float.NaN
    else x - y*floor(x/y)
  }

  /** Returns the smaller one of two arguments.
   * @param x a float argument.
   * @param y a float argument.
   * @return the smallest argument.
   */
  def min(x: Float, y: Float) :Float = {
    if (y == 0 && isnegzero(x)) x
    else if (x < y || isnan(x)) x
    else y
  }
  
  /** Returns the larger one of two arguments.
   * @param x a float argument.
   * @param y a float argument.
   * @return the largest argument.
   */
  def max(x: Float, y: Float) :Float = {
    if (x == 0 && isnegzero(y)) x
    else if (x > y || isnan(x)) x
    else y
  }
  
  /** Clamps a value to a given range.
   * @param x a float value to clamp.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return one of the following:
   *   - NaN if ''minVal'' > ''maxVal''.
   *   - ''minVal'' if ''x'' < ''minVal''.
   *   - ''maxVal'' if ''x'' > ''maxVal''.
   *   - ''x'' otherwise.
   */
  def clamp(x: Float, minVal: Float, maxVal: Float) :Float = {
    if (!(minVal <= maxVal)) scala.Float.NaN
    else if (x > maxVal) maxVal
    else if (x < minVal) minVal
    else x
  }

  /** Returns the linear interpolation between ''x'' and ''y'' with a factor ''a''.
   * @param x interpolation value for a factor 0.
   * @param y interpolation value for a factor 1.
   * @param a interpolation factor, must be in range [0, 1] to achieve the desired result.
   * @return `x*(1 - a) + y*a`.
   */
  def mix(x: Float, y: Float, a: Float) :Float = x*(1 - a) + y*a
  
  /** Returns the value of the step function.
   * @param edge a float argument.
   * @param x a float argument.
   * @return one of the following:
   *   - 0 if ''x'' < ''edge''.
   *   - 1 otherwise.
   */
  def step(edge: Float, x: Float) :Float = {
    if (x < edge) 0
    else if (x >= edge) 1
    else scala.Float.NaN
  }
  
  /** Returns 0 if ''x'' < ''edge0'' and 1 if ''x'' > ''edge1'',
   * otherwise performs Hermite interpolation from 0 to 1 with
   * 1st derivate 0 at x = edge0 and x = edge1.
   * @param edge0 the first edge.
   * @param edge1 the second edge.
   * @param x a float argument.
   * @return one of the following:
   *   - NaN if ''edge0'' > ''edge1''.
   *   - 0 if ''x'' < ''edge0''.
   *   - 1 if ''x'' > ''edge1''.
   *   - smooth interpolation from 0 to 1 otherwise.
   */
  def smoothstep(edge0: Float, edge1: Float, x: Float) :Float = {
    if (!(edge0 <= edge1)) scala.Float.NaN
    else if (x >= edge1) 1
    else if (x <= edge0) 0
    else {
      val t = (x - edge0)/(edge1 - edge0)
      t*t*(3 - 2*t)
    }
  }

  /** Returns 0 if ''x'' < ''edge0'' and 1 if ''x'' > ''edge1'',
   * otherwise performs smooth interpolation from 0 to 1 with
   * 1st and 2nd derivaties 0 at x = edge0 and x = edge1.
   * @param edge0 the first edge.
   * @param edge1 the second edge.
   * @param x a float argument.
   * @return one of the following:
   *   - NaN if ''edge0'' > ''edge1''.
   *   - 0 if ''x'' < ''edge0''.
   *   - 1 if ''x'' > ''edge1''.
   *   - smooth interpolation from 0 to 1 otherwise.
   */
  def smootherstep(edge0: Float, edge1: Float, x: Float) :Float = {
    if (!(edge0 <= edge1)) scala.Float.NaN
    else if (x >= edge1) 1
    else if (x <= edge0) 0
    else {
      val t = (x - edge0)/(edge1 - edge0)
      t*t*t*(t*(t*6 - 15) + 10)
    }
  }

  def pulse(edge0: Float, edge1: Float, x: Float): Float = {
    if (!(edge0 <= edge1)) scala.Float.NaN
    else if (x >= edge1) 0
    else if (x <= edge0) 0
    else {
      val t = (2*x - (edge0 + edge1))/(edge1 - edge0)
      t*(t*t*(t*t*((-2048/432.0f)*t*t + (6144/432.0f)) + (-6144/432.0f)) + (2048/432.0f))
    }
  }

  def saturate(edge0: Float, edge1: Float, x: Float) :Float = {
    val s = (edge1 - edge0)*0.1f
    x + pulse(edge0 - s, edge1 + s, x)*s
  }

  /** Checks if the argument is NaN.
   * @param x a float argument.
   * @return one of the following:
   *   - ''true'' if ''x'' is NaN.
   *   - ''false'' otherwise.
   */
  def isnan(x: Float) :Boolean = java.lang.Float.isNaN(x)
  
  /** Checks if the argument is ''infinity''.
   * @param x a float argument.
   * @return one of the following:
   *   - ''true'' if ''x'' is `-`''infinity'' or `+`''infinity''.
   *   - ''false'' otherwise.
   */
  def isinf(x: Float) :Boolean = java.lang.Float.isInfinite(x)

  /** Computes the length of the argument.
   * For scalar components `length(x)` is equivalent to `abs(x)`.
   * @param x a float argument.
   * @return the distance between the argument and the origin.
   */
  def length(x: Float) :Float = abs(x)
  
  /** Computes the distance between the arguments.
   * @param x a float argument.
   * @param y a float argument.
   * @return the distance between the arguments.
   */
  def distance(x: Float, y: Float) :Float = abs(x - y)
  
  /** Computes the dot product of the arguments.
   * For scalar components `dot(x, y)` is equivalent to `x*y`.
   * @param x a float argument.
   * @param y a float argument.
   * @return the dot product of the arguments.
   */
  def dot(x: Float, y: Float) :Float = x*y
  
  /** Returns the normalized value of the argument.
   * @param x a float argument.
   * @return one of the following:
   *   - -1 if ''x'' < 0.
   *   - 1 if ''x'' > 0.
   *   - NaN otherwise.
   */
  def normalize(x: Float) :Float = {
    if (x > 0) 1
    else if (x < 0) -1
    else scala.Float.NaN
  }

  /** Flips the normal vector ''n'' to face the direction opposite to the incident vector ''i''.
   * @param n the normal.
   * @param i the incident vector.
   * @param nref the reference normal.
   * @return one of the following:
   *   - ''n'' if dot(''i'', ''nref'') < 0.
   *   - ''-n'' otherwise.
   */
  def faceforward(n: Float, i: Float, nref: Float) :Float = {
    val dot = i*nref
    if (dot < 0) n else if (isnan(dot)) scala.Float.NaN else -n
  }

  /** Reflects the incident vector ''i'' with respect to the normal vector ''n''.
   * This function is equivalent to `i - 2*dot(n, i)*n`.
   * @param n the normal, must be normalized to achieve the desired result.
   * @param i the incident vector.
   * @return the reflection vector.
   */
  def reflect(i: Float, n: Float) :Float = {
    i - 2*(n*i)*n
  }

  /** Refracts the incident vector ''i'' with respect to the normal vector ''n''
   * using the ratio of indices of refraction ''eta''.
   * @param n the normal, must be normalized to achieve the desired result.
   * @param i the incident vector, must be normalized to achieve the desired result.
   * @param eta the ratio of indices of refration.
   * @return the refraction vector.
   */
  def refract(i: Float, n: Float, eta: Float) :Float = {
    val ni = n*i
    val k = 1 - eta*eta*(1 - ni*ni)
    if (k < 0) 0 else eta*i - (eta*ni + sqrt(k))*n
  }

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
   * @param x a float argument.
   * @return the value of the simplex noise function.
   */
  def noise1(x: Float) :Float = noise(x).toFloat
  
  /** Computes __two__ values of the 1-dimensional simplex noise function.
   * The first return value is computed using the argument.
   * The second is computed by adding a constant offset to the argument.
   * @param x a float argument.
   * @return two values of the simplex noise function packed as Vec2.
   * @see [[simplex3d.math.floatx.functions.noise1(Float)]]
   */
  def noise2(x: Float) :Vec2f = {
    new Vec2f(
      noise(x).toFloat,
      noise(x + offset1).toFloat
    )
  }
  
  /** Computes __three__ values of the 1-dimensional simplex noise function.
   * The first return value is computed using the argument.
   * The others are computed by adding a constant offset to the argument.
   * @param x a float argument.
   * @return three values of the simplex noise function packed as Vec3.
   * @see [[simplex3d.math.floatx.functions.noise1(Float)]]
   */
  def noise3(x: Float) :Vec3f = {
    new Vec3f(
      noise(x).toFloat,
      noise(x + offset1).toFloat,
      noise(x + offset2).toFloat
    )
  }
  
  /** Computes __four__ values of the 1-dimensional simplex noise function.
   * The first return value is computed using the argument.
   * The others are computed by adding a constant offset to the argument.
   * @param x a float argument.
   * @return four values of the simplex noise function packed as Vec4.
   * @see [[simplex3d.math.floatx.functions.noise1(Float)]]
   */
  def noise4(x: Float) :Vec4f = {
    new Vec4f(
      noise(x).toFloat,
      noise(x + offset1).toFloat,
      noise(x + offset2).toFloat,
      noise(x + offset3).toFloat
    )
  }

  // Vec2f functions
  /** Performs component-wise conversion to radians.
   * @param u a float vector argument.
   * @return a vector with argument components converted to radians.
   */
  def radians(u: inVec2f) :Vec2f = new Vec2f(radians(u.x), radians(u.y))
  
  /** Performs component-wise conversion to degrees.
   * @param u a float vector argument.
   * @return a vector with argument components converted to degrees.
   */
  def degrees(u: inVec2f) :Vec2f = new Vec2f(degrees(u.x), degrees(u.y))

  /** Applies '''sin''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `sin(c)` for each component ''c''.
   */
  def sin(u: inVec2f) :Vec2f = new Vec2f(sin(u.x), sin(u.y))
  
  /** Applies '''cos''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `cos(c)` for each component ''c''.
   */
  def cos(u: inVec2f) :Vec2f = new Vec2f(cos(u.x), cos(u.y))
  
  /** Applies '''tan''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `tan(c)` for each component ''c''.
   */
  def tan(u: inVec2f) :Vec2f = new Vec2f(tan(u.x), tan(u.y))

  /** Applies '''asin''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `asin(c)` for each component ''c''.
   */
  def asin(u: inVec2f) :Vec2f = new Vec2f(asin(u.x), asin(u.y))
  
  /** Applies '''acos''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `acos(c)` for each component ''c''.
   */
  def acos(u: inVec2f) :Vec2f = new Vec2f(acos(u.x), acos(u.y))
  
  /** Applies '''atan''' function to each component.
   * @param uy a float vector argument.
   * @param ux a float vector argument.
   * @return a vector with with components set to `atan(uy.c, ux.c)` for each component ''c''.
   */
  def atan(uy: inVec2f, ux: inVec2f) :Vec2f = new Vec2f(atan(uy.x, ux.x), atan(uy.y, ux.y))

  /** Applies '''atan''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `atan(c)` for each component ''c''.
   */
  def atan(u: inVec2f) :Vec2f = new Vec2f(atan(u.x), atan(u.y))

  /** Applies '''sinh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `sinh(c)` for each component ''c''.
   */
  def sinh(u: inVec2f) :Vec2f = new Vec2f(sinh(u.x), sinh(u.y))
  
  /** Applies '''cosh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `cosh(c)` for each component ''c''.
   */
  def cosh(u: inVec2f) :Vec2f = new Vec2f(cosh(u.x), cosh(u.y))
  
  /** Applies '''tanh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `tanh(c)` for each component ''c''.
   */
  def tanh(u: inVec2f) :Vec2f = new Vec2f(tanh(u.x), tanh(u.y))

  /** Applies '''asinh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `asinh(c)` for each component ''c''.
   */
  def asinh(u: inVec2f) :Vec2f = new Vec2f(asinh(u.x), asinh(u.y))
  
  /** Applies '''acosh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `acosh(c)` for each component ''c''.
   */
  def acosh(u: inVec2f) :Vec2f = new Vec2f(acosh(u.x), acosh(u.y))
  
  /** Applies '''atanh''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `atanh(c)` for each component ''c''.
   */
  def atanh(u: inVec2f) :Vec2f = new Vec2f(atanh(u.x), atanh(u.y))

  /** Applies '''pow''' function to each component.
   * @param u a float vector argument.
   * @param v a float vector argument.
   * @return a vector with with components set to `pow(u.c, v.c)` for each component ''c''.
   */
  def pow(u: inVec2f, v: inVec2f) :Vec2f = new Vec2f(pow(u.x, v.x), pow(u.y, v.y))
  
  /** Applies exponential function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `e`^c^ for each component ''c''.
   */
  def exp(u: inVec2f) :Vec2f = new Vec2f(exp(u.x), exp(u.y))
  
  /** Applies natural logarith function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `log(c)` for each component ''c''.
   */
  def log(u: inVec2f) :Vec2f = new Vec2f(log(u.x), log(u.y))

  /** Applies '''exp2''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `2`^c^ for each component ''c''.
   */
  def exp2(u: inVec2f) :Vec2f = new Vec2f(exp2(u.x), exp2(u.y))
  
  /** Applies '''log2''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `log2(c)` for each component ''c''.
   */
  def log2(u: inVec2f) :Vec2f = new Vec2f(log2(u.x), log2(u.y))

  /** Applies '''sqrt''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `√c` for each component ''c''.
   */
  def sqrt(u: inVec2f) :Vec2f = new Vec2f(sqrt(u.x), sqrt(u.y))
  
  /** Applies '''inversesqrt''' function to each component.
   * @param u a float vector argument.
   * @return a vector with with components set to `1/√c` for each component ''c''.
   */
  def inversesqrt(u: inVec2f) :Vec2f = {
    new Vec2f(inversesqrt(u.x), inversesqrt(u.y))
  }

  def abs(u: inVec2f) :Vec2f = new Vec2f(abs(u.x), abs(u.y))
  def sign(u: inVec2f) :Vec2f = new Vec2f(sign(u.x), sign(u.y))
  def floor(u: inVec2f) :Vec2f = new Vec2f(floor(u.x), floor(u.y))
  def trunc(u: inVec2f) :Vec2f = new Vec2f(trunc(u.x), trunc(u.y))
  def round(u: inVec2f) :Vec2f = new Vec2f(round(u.x), round(u.y))
  def roundEven(u: inVec2f) :Vec2f = {
    new Vec2f(roundEven(u.x), roundEven(u.y))
  }
  def ceil(u: inVec2f) :Vec2f = new Vec2f(ceil(u.x), ceil(u.y))
  def fract(u: inVec2f) :Vec2f = new Vec2f(fract(u.x), fract(u.y))
  def mod(u: inVec2f, s: Float) :Vec2f = new Vec2f(mod(u.x, s), mod(u.y, s))
  def mod(u: inVec2f, v: inVec2f) :Vec2f = {
    new Vec2f(mod(u.x, v.x), mod(u.y, v.y))
  }
  
  /** Separates each component of a given vector into fractional and integer
   * parts, both parts will have the same sign as the component.
   *
   * @param u the vector to be separated into fractional and integer parts.
   * @param i a result vector to store the integer parts of components of u.
   * @return a vector with fractional parts of components of u.
   */
  def modf(u: inVec2f, i: outVec2f) :Vec2f = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    u - i
  }

  def min(u: inVec2f, s: Float) :Vec2f = new Vec2f(min(u.x, s), min(u.y, s))
  def min(u: inVec2f, v: inVec2f) :Vec2f = {
    new Vec2f(min(u.x, v.x), min(u.y, v.y))
  }
  def max(u: inVec2f, s: Float) :Vec2f = new Vec2f(max(u.x, s), max(u.y, s))
  def max(u: inVec2f, v: inVec2f) :Vec2f = {
    new Vec2f(max(u.x, v.x), max(u.y, v.y))
  }
  def clamp(u: inVec2f, minVal: Float, maxVal: Float) :Vec2f = {
    new Vec2f(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  def clamp(u: inVec2f, minVal: inVec2f, maxVal: inVec2f) :Vec2f = {
    new Vec2f(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }

  def mix(u: inVec2f, v: inVec2f, a: Float) :Vec2f = {
    val b = 1 - a
    new Vec2f(b*u.x + a*v.x, b*u.y + a*v.y)
  }
  def mix(u: inVec2f, v: inVec2f, a: inVec2f) :Vec2f = {
    new Vec2f(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y))
  }
  def mix(u: inVec2f, v: inVec2f, a: inVec2b) :Vec2f = {
    new Vec2f(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y
    )
  }

  def step(edge: Float, u: inVec2f) :Vec2f = {
    new Vec2f(step(edge, u.x), step(edge, u.y))
  }
  def step(edge: inVec2f, u: inVec2f) :Vec2f = {
    new Vec2f(step(edge.x, u.x), step(edge.y, u.y))
  }
  def smoothstep(edge0: Float, edge1: Float, u: inVec2f) :Vec2f = {
    new Vec2f(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y)
    )
  }
  def smoothstep(edge0: inVec2f, edge1: inVec2f, u: inVec2f) :Vec2f = {
    new Vec2f(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y)
    )
  }

  def isnan(u: inVec2f) :Vec2b = new Vec2b(isnan(u.x), isnan(u.y))
  def isinf(u: inVec2f) :Vec2b = new Vec2b(isinf(u.x), isinf(u.y))

  def length(u: inVec2f) :Float = sqrt(u.x*u.x + u.y*u.y)
  def distance(u: inVec2f, v: inVec2f) :Float = {
    val x = u.x - v.x
    val y = u.y - v.y
    sqrt(x*x + y*y)
  }
  def dot(u: inVec2f, v: inVec2f) :Float = u.x*v.x + u.y*v.y
  def normalize(u: inVec2f) :Vec2f = u*inversesqrt(u.x*u.x + u.y*u.y)

  def faceforward(n: inVec2f, i: inVec2f, nref: inVec2f) :Vec2f = {
    val d = dot(nref, i)
    if (d < 0) Vec2f(n) else if (isnan(d)) Vec2f(scala.Float.NaN) else -n
  }

  def reflect(i: inVec2f, n: inVec2f) :Vec2f = {
    val t = -2*dot(n, i)
    new Vec2f(
      i.x + n.x*t,
      i.y + n.y*t
    )
  }
  def refract(i: inVec2f, n: inVec2f, eta: Float) :Vec2f = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec2f(0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec2f(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t
      )
    }
  }

  def lessThan(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  def lessThanEqual(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  def greaterThan(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  def greaterThanEqual(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  def equal(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  def notEqual(u: inVec2f, v: inVec2f) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  def noise1(u: inVec2f) :Float = {
    noise(u.x, u.y).toFloat
  }
  def noise2(u: inVec2f) :Vec2f = {
    new Vec2f(
      noise(u.x, u.y).toFloat,
      noise(u.x + offset1, u.y + offset1).toFloat
    )
  }
  def noise3(u: inVec2f) :Vec3f = {
    new Vec3f(
      noise(u.x, u.y).toFloat,
      noise(u.x + offset1, u.y + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2).toFloat
    )
  }
  def noise4(u: inVec2f) :Vec4f = {
    new Vec4f(
      noise(u.x, u.y).toFloat,
      noise(u.x + offset1, u.y + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2).toFloat,
      noise(u.x + offset3, u.y + offset3).toFloat
    )
  }


  // Vec3f functions
  def radians(u: inVec3f) :Vec3f = {
    new Vec3f(radians(u.x), radians(u.y), radians(u.z))
  }
  def degrees(u: inVec3f) :Vec3f = {
    new Vec3f(degrees(u.x), degrees(u.y), degrees(u.z))
  }

  def sin(u: inVec3f) :Vec3f = new Vec3f(sin(u.x), sin(u.y), sin(u.z))
  def cos(u: inVec3f) :Vec3f = new Vec3f(cos(u.x), cos(u.y), cos(u.z))
  def tan(u: inVec3f) :Vec3f = new Vec3f(tan(u.x), tan(u.y), tan(u.z))

  def asin(u: inVec3f) :Vec3f = new Vec3f(asin(u.x), asin(u.y), asin(u.z))
  def acos(u: inVec3f) :Vec3f = new Vec3f(acos(u.x), acos(u.y), acos(u.z))
  def atan(uy: inVec3f, ux: inVec3f) :Vec3f = {
    new Vec3f(atan(uy.x, ux.x), atan(uy.y, ux.y), atan(uy.z, ux.z))
  }
  def atan(u: inVec3f) :Vec3f = new Vec3f(atan(u.x), atan(u.y), atan(u.z))

  def sinh(u: inVec3f) :Vec3f = new Vec3f(sinh(u.x), sinh(u.y), sinh(u.z))
  def cosh(u: inVec3f) :Vec3f = new Vec3f(cosh(u.x), cosh(u.y), cosh(u.z))
  def tanh(u: inVec3f) :Vec3f = new Vec3f(tanh(u.x), tanh(u.y), tanh(u.z))

  def asinh(u: inVec3f) :Vec3f = {
    new Vec3f(asinh(u.x), asinh(u.y), asinh(u.z))
  }
  def acosh(u: inVec3f) :Vec3f = {
    new Vec3f(acosh(u.x), acosh(u.y), acosh(u.z))
  }
  def atanh(u: inVec3f) :Vec3f = {
    new Vec3f(atanh(u.x), atanh(u.y), atanh(u.z))
  }

  def pow(u: inVec3f, v: inVec3f) :Vec3f = {
    new Vec3f(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z))
  }
  def exp(u: inVec3f) :Vec3f = new Vec3f(exp(u.x), exp(u.y), exp(u.z))
  def log(u: inVec3f) :Vec3f = new Vec3f(log(u.x), log(u.y), log(u.z))

  def exp2(u: inVec3f) :Vec3f = new Vec3f(exp2(u.x), exp2(u.y), exp2(u.z))
  def log2(u: inVec3f) :Vec3f = new Vec3f(log2(u.x), log2(u.y), log2(u.z))

  def sqrt(u: inVec3f) :Vec3f = new Vec3f(sqrt(u.x), sqrt(u.y), sqrt(u.z))
  def inversesqrt(u: inVec3f) :Vec3f = {
    new Vec3f(inversesqrt(u.x), inversesqrt(u.y), inversesqrt(u.z))
  }

  def abs(u: inVec3f) :Vec3f = new Vec3f(abs(u.x), abs(u.y), abs(u.z))
  def sign(u: inVec3f) :Vec3f = new Vec3f(sign(u.x), sign(u.y), sign(u.z))
  def floor(u: inVec3f) :Vec3f = {
    new Vec3f(floor(u.x), floor(u.y), floor(u.z))
  }
  def trunc(u: inVec3f) :Vec3f = {
    new Vec3f(trunc(u.x), trunc(u.y), trunc(u.z))
  }
  def round(u: inVec3f) :Vec3f = {
    new Vec3f(round(u.x), round(u.y), round(u.z))
  }
  def roundEven(u: inVec3f) :Vec3f = {
    new Vec3f(roundEven(u.x), roundEven(u.y), roundEven(u.z))
  }
  def ceil(u: inVec3f) :Vec3f = new Vec3f(ceil(u.x), ceil(u.y), ceil(u.z))
  def fract(u: inVec3f) :Vec3f = {
    new Vec3f(fract(u.x), fract(u.y), fract(u.z))
  }
  def mod(u: inVec3f, s: Float) :Vec3f = {
    new Vec3f(mod(u.x, s), mod(u.y, s), mod(u.z, s))
  }
  def mod(u: inVec3f, v: inVec3f) :Vec3f = {
    new Vec3f(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z))
  }
  def modf(u: inVec3f, i: outVec3f) :Vec3f = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    i.z = trunc(u.z)
    u - i
  }

  def min(u: inVec3f, s: Float) :Vec3f = {
    new Vec3f(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  def min(u: inVec3f, v: inVec3f) :Vec3f = {
    new Vec3f(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  def max(u: inVec3f, s: Float) :Vec3f = {
    new Vec3f(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  def max(u: inVec3f, v: inVec3f) :Vec3f = {
    new Vec3f(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  def clamp(u: inVec3f, minVal: Float, maxVal: Float) :Vec3f = {
    new Vec3f(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  def clamp(u: inVec3f, minVal: inVec3f, maxVal: inVec3f) :Vec3f = {
    new Vec3f(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }

  def mix(u: inVec3f, v: inVec3f, a: Float) :Vec3f = {
    val b = 1 - a
    new Vec3f(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z)
  }
  def mix(u: inVec3f, v: inVec3f, a: inVec3f) :Vec3f = {
    new Vec3f(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y), mix(u.z, v.z, a.z))
  }
  def mix(u: inVec3f, v: inVec3f, a: inVec3b) :Vec3f = {
    new Vec3f(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y,
      if (a.z) v.z else u.z
    )
  }

  def step(edge: Float, u: inVec3f) :Vec3f = {
    new Vec3f(step(edge, u.x), step(edge, u.y), step(edge, u.z))
  }
  def step(edge: inVec3f, u: inVec3f) :Vec3f = {
    new Vec3f(step(edge.x, u.x), step(edge.y, u.y), step(edge.z, u.z))
  }
  def smoothstep(edge0: Float, edge1: Float, u: inVec3f) :Vec3f = {
    new Vec3f(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y),
      smoothstep(edge0, edge1, u.z)
    )
  }
  def smoothstep(edge0: inVec3f, edge1: inVec3f, u: inVec3f) :Vec3f = {
    new Vec3f(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y),
      smoothstep(edge0.z, edge1.z, u.z)
    )
  }

  def isnan(u: inVec3f) :Vec3b = {
    new Vec3b(isnan(u.x), isnan(u.y), isnan(u.z))
  }
  def isinf(u: inVec3f) :Vec3b = {
    new Vec3b(isinf(u.x), isinf(u.y), isinf(u.z))
  }

  def length(u: inVec3f) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)
  def distance(u: inVec3f, v: inVec3f) :Float = {
    val x = u.x - v.x
    val y = u.y - v.y
    val z = u.z - v.z
    sqrt(x*x + y*y + z*z)
  }
  def dot(u: inVec3f, v: inVec3f) :Float = u.x*v.x + u.y*v.y + u.z*v.z
  def cross(u: inVec3f, v: inVec3f) :Vec3f = {
    new Vec3f(
      u.y*v.z-v.y*u.z,
      u.z*v.x-v.z*u.x,
      u.x*v.y-v.x*u.y
    )
  }
  def normalize(u: inVec3f) :Vec3f = {
    u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z)
  }

  def faceforward(n: inVec3f, i: inVec3f, nref: inVec3f) :Vec3f = {
    val d = dot(nref, i)
    if (d < 0) Vec3f(n) else if (isnan(d)) Vec3f(scala.Float.NaN) else -n
  }

  def reflect(i: inVec3f, n: inVec3f) :Vec3f = {
    val t = -2*dot(n, i)
    new Vec3f(
      i.x + n.x*t,
      i.y + n.y*t,
      i.z + n.z*t
    )
  }
  def refract(i: inVec3f, n: inVec3f, eta: Float) :Vec3f = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec3f(0, 0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec3f(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t,
        i.z*eta - n.z*t
      )
    }
  }

  def lessThan(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  def lessThanEqual(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  def greaterThan(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  def greaterThanEqual(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  def equal(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  def notEqual(u: inVec3f, v: inVec3f) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  def noise1(u: inVec3f) :Float = {
    noise(u.x, u.y, u.z).toFloat
  }
  def noise2(u: inVec3f) :Vec2f = {
    new Vec2f(
      noise(u.x, u.y, u.z).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1).toFloat
    )
  }
  def noise3(u: inVec3f) :Vec3f = {
    new Vec3f(
      noise(u.x, u.y, u.z).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2, u.z + offset2).toFloat
    )
  }
  def noise4(u: inVec3f) :Vec4f = {
    new Vec4f(
      noise(u.x, u.y, u.z).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2, u.z + offset2).toFloat,
      noise(u.x + offset3, u.y + offset3, u.z + offset3).toFloat
    )
  }

  // Vec4f functions
  def radians(u: inVec4f) :Vec4f = {
    new Vec4f(radians(u.x), radians(u.y), radians(u.z), radians(u.w))
  }
  def degrees(u: inVec4f) :Vec4f = {
    new Vec4f(degrees(u.x), degrees(u.y), degrees(u.z), degrees(u.w))
  }

  def sin(u: inVec4f) :Vec4f = {
    new Vec4f(sin(u.x), sin(u.y), sin(u.z), sin(u.w))
  }
  def cos(u: inVec4f) :Vec4f = {
    new Vec4f(cos(u.x), cos(u.y), cos(u.z), cos(u.w))
  }
  def tan(u: inVec4f) :Vec4f = {
    new Vec4f(tan(u.x), tan(u.y), tan(u.z), tan(u.w))
  }

  def asin(u: inVec4f) :Vec4f = {
    new Vec4f(asin(u.x), asin(u.y), asin(u.z), asin(u.w))
  }
  def acos(u: inVec4f) :Vec4f = {
    new Vec4f(acos(u.x), acos(u.y), acos(u.z), acos(u.w))
  }
  def atan(uy: inVec4f, ux: inVec4f) :Vec4f = {
    new Vec4f(
      atan(uy.x, ux.x),
      atan(uy.y, ux.y),
      atan(uy.z, ux.z),
      atan(uy.w, ux.w)
    )
  }
  def atan(u: inVec4f) :Vec4f = {
    new Vec4f(atan(u.x), atan(u.y), atan(u.z), atan(u.w))
  }

  def sinh(u: inVec4f) :Vec4f = {
    new Vec4f(sinh(u.x), sinh(u.y), sinh(u.z), sinh(u.w))
  }
  def cosh(u: inVec4f) :Vec4f = {
    new Vec4f(cosh(u.x), cosh(u.y), cosh(u.z), cosh(u.w))
  }
  def tanh(u: inVec4f) :Vec4f = {
    new Vec4f(tanh(u.x), tanh(u.y), tanh(u.z), tanh(u.w))
  }

  def asinh(u: inVec4f) :Vec4f = {
    new Vec4f(asinh(u.x), asinh(u.y), asinh(u.z), asinh(u.w))
  }
  def acosh(u: inVec4f) :Vec4f = {
    new Vec4f(acosh(u.x), acosh(u.y), acosh(u.z), acosh(u.w))
  }
  def atanh(u: inVec4f) :Vec4f = {
    new Vec4f(atanh(u.x), atanh(u.y), atanh(u.z), atanh(u.w))
  }

  def pow(u: inVec4f, v: inVec4f) :Vec4f = {
    new Vec4f(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z), pow(u.w, v.w))
  }
  def exp(u: inVec4f) :Vec4f = {
    new Vec4f(exp(u.x), exp(u.y), exp(u.z), exp(u.w))
  }
  def log(u: inVec4f) :Vec4f = {
    new Vec4f(log(u.x), log(u.y), log(u.z), log(u.w))
  }

  def exp2(u: inVec4f) :Vec4f = {
    new Vec4f(exp2(u.x), exp2(u.y), exp2(u.z), exp2(u.w))
  }
  def log2(u: inVec4f) :Vec4f = {
    new Vec4f(log2(u.x), log2(u.y), log2(u.z), log2(u.w))
  }

  def sqrt(u: inVec4f) :Vec4f = {
    new Vec4f(sqrt(u.x), sqrt(u.y), sqrt(u.z), sqrt(u.w))
  }
  def inversesqrt(u: inVec4f) :Vec4f = {
    new Vec4f(
      inversesqrt(u.x),
      inversesqrt(u.y),
      inversesqrt(u.z),
      inversesqrt(u.w)
    )
  }

  def abs(u: inVec4f) :Vec4f = {
    new Vec4f(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  def sign(u: inVec4f) :Vec4f = {
    new Vec4f(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  def floor(u: inVec4f) :Vec4f = {
    new Vec4f(floor(u.x), floor(u.y), floor(u.z), floor(u.w))
  }
  def trunc(u: inVec4f) :Vec4f = {
    new Vec4f(trunc(u.x), trunc(u.y), trunc(u.z), trunc(u.w))
  }
  def round(u: inVec4f) :Vec4f = {
    new Vec4f(round(u.x), round(u.y), round(u.z), round(u.w))
  }
  def roundEven(u: inVec4f) :Vec4f = {
    new Vec4f(
      roundEven(u.x), roundEven(u.y),
      roundEven(u.z), roundEven(u.w)
    )
  }
  def ceil(u: inVec4f) :Vec4f = {
    new Vec4f(ceil(u.x), ceil(u.y), ceil(u.z), ceil(u.w))
  }
  def fract(u: inVec4f) :Vec4f = {
    new Vec4f(fract(u.x), fract(u.y), fract(u.z), fract(u.w))
  }
  def mod(u: inVec4f, s: Float) :Vec4f = {
    new Vec4f(mod(u.x, s), mod(u.y, s), mod(u.z, s), mod(u.w, s))
  }
  def mod(u: inVec4f, v: inVec4f) :Vec4f = {
    new Vec4f(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z), mod(u.w, v.w))
  }
  def modf(u: inVec4f, i: outVec4f) :Vec4f = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    i.z = trunc(u.z)
    i.w = trunc(u.w)
    u - i
  }

  def min(u: inVec4f, s: Float) :Vec4f = {
    new Vec4f(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  def min(u: inVec4f, v: inVec4f) :Vec4f = {
    new Vec4f(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  def max(u: inVec4f, s: Float) :Vec4f = {
    new Vec4f(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  def max(u: inVec4f, v: inVec4f) :Vec4f = {
    new Vec4f(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  def clamp(u: inVec4f, minVal: Float, maxVal: Float) :Vec4f = {
    new Vec4f(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  def clamp(u: inVec4f, minVal: inVec4f, maxVal: inVec4f) :Vec4f = {
    new Vec4f(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }

  def mix(u: inVec4f, v: inVec4f, a: Float) :Vec4f = {
    val b = 1 - a
    new Vec4f(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z, b*u.w + a*v.w)
  }
  def mix(u: inVec4f, v: inVec4f, a: inVec4f) :Vec4f = {
    new Vec4f(
      mix(u.x, v.x, a.x),
      mix(u.y, v.y, a.y),
      mix(u.z, v.z, a.z),
      mix(u.w, v.w, a.w)
    )
  }
  def mix(u: inVec4f, v: inVec4f, a: inVec4b) :Vec4f = {
    new Vec4f(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y,
      if (a.z) v.z else u.z,
      if (a.w) v.w else u.w
    )
  }

  def step(edge: Float, u: inVec4f) :Vec4f = {
    new Vec4f(step(edge, u.x), step(edge, u.y),
        step(edge, u.z), step(edge, u.w))
  }
  def step(edge: inVec4f, u: inVec4f) :Vec4f = {
    new Vec4f(
      step(edge.x, u.x),
      step(edge.y, u.y),
      step(edge.z, u.z),
      step(edge.w, u.w)
    )
  }
  def smoothstep(edge0: Float, edge1: Float, u: inVec4f) :Vec4f = {
    new Vec4f(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y),
      smoothstep(edge0, edge1, u.z),
      smoothstep(edge0, edge1, u.w)
    )
  }
  def smoothstep(edge0: inVec4f, edge1: inVec4f, u: inVec4f) :Vec4f = {
    new Vec4f(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y),
      smoothstep(edge0.z, edge1.z, u.z),
      smoothstep(edge0.w, edge1.w, u.w)
    )
  }

  def isnan(u: inVec4f) :Vec4b = {
    new Vec4b(isnan(u.x), isnan(u.y), isnan(u.z), isnan(u.w))
  }
  def isinf(u: inVec4f) :Vec4b = {
    new Vec4b(isinf(u.x), isinf(u.y), isinf(u.z), isinf(u.w))
  }

  def length(u: inVec4f) :Float = sqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
  def distance(u: inVec4f, v: inVec4f) :Float = {
    val x = u.x - v.x
    val y = u.y - v.y
    val z = u.z - v.z
    val w = u.w - v.w
    sqrt(x*x + y*y + z*z + w*w)
  }
  def dot(u: inVec4f, v: inVec4f) :Float = {
    u.x*v.x + u.y*v.y + u.z*v.z + u.w*v.w
  }
  def normalize(u: inVec4f) :Vec4f = {
    u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
  }

  def faceforward(n: inVec4f, i: inVec4f, nref: inVec4f) :Vec4f = {
    val d = dot(nref, i)
    if (d < 0) Vec4f(n) else if (isnan(d)) Vec4f(scala.Float.NaN) else -n
  }

  def reflect(i: inVec4f, n: inVec4f) :Vec4f = {
    val t = -2*dot(n, i)
    new Vec4f(
      i.x + n.x*t,
      i.y + n.y*t,
      i.z + n.z*t,
      i.w + n.w*t
    )
  }
  def refract(i: inVec4f, n: inVec4f, eta: Float) :Vec4f = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec4f(0, 0, 0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec4f(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t,
        i.z*eta - n.z*t,
        i.w*eta - n.w*t
      )
    }
  }

  def lessThan(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  def lessThanEqual(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  def greaterThan(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  def greaterThanEqual(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  def equal(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  def notEqual(u: inVec4f, v: inVec4f) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }

  def noise1(u: inVec4f) :Float = {
    noise(u.x, u.y, u.z, u.w).toFloat
  }
  def noise2(u: inVec4f) :Vec2f = {
    new Vec2f(
      noise(u.x, u.y, u.z, u.w).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1).toFloat
    )
  }
  def noise3(u: inVec4f) :Vec3f = {
    new Vec3f(
      noise(u.x, u.y, u.z, u.w).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2, u.z + offset2, u.w + offset2).toFloat
    )
  }
  def noise4(u: inVec4f) :Vec4f = {
    new Vec4f(
      noise(u.x, u.y, u.z, u.w).toFloat,
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1).toFloat,
      noise(u.x + offset2, u.y + offset2, u.z + offset2, u.w + offset2).toFloat,
      noise(u.x + offset3, u.y + offset3, u.z + offset3, u.w + offset3).toFloat
    )
  }

  // Mat functions
  def matrixCompMult(a: inMat2f, b: inMat2f) :Mat2f = {
    new Mat2f(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11
    )
  }
  def matrixCompMult(a: inMat2x3f, b: inMat2x3f) :Mat2x3f = {
    new Mat2x3f(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11,
      a.m02*b.m02, a.m12*b.m12
    )
  }
  def matrixCompMult(a: inMat2x4f, b: inMat2x4f) :Mat2x4f = {
    new Mat2x4f(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11,
      a.m02*b.m02, a.m12*b.m12,
      a.m03*b.m03, a.m13*b.m13
    )
  }
  def matrixCompMult(a: inMat3x2f, b: inMat3x2f) :Mat3x2f = {
    new Mat3x2f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21
    )
  }
  def matrixCompMult(a: inMat3f, b: inMat3f) :Mat3f = {
    new Mat3f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22
    )
  }
  def matrixCompMult(a: inMat3x4f, b: inMat3x4f) :Mat3x4f = {
    new Mat3x4f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22,
      a.m03*b.m03, a.m13*b.m13, a.m23*b.m23
    )
  }
  def matrixCompMult(a: inMat4x2f, b: inMat4x2f) :Mat4x2f = {
    new Mat4x2f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31
    )
  }
  def matrixCompMult(a: inMat4x3f, b: inMat4x3f) :Mat4x3f = {
    new Mat4x3f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32
    )
  }
  def matrixCompMult(a: inMat4f, b: inMat4f) :Mat4f = {
    new Mat4f(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32,
      a.m03*b.m03, a.m13*b.m13, a.m23*b.m23, a.m33*b.m33
    )
  }

  def outerProduct(c: inVec2f, r: inVec2f) :Mat2f = {
    new Mat2f(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y
    )
  }
  def outerProduct(c: inVec2f, r: inVec3f) :Mat2x3f = {
    new Mat2x3f(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y,
      c.x*r.z, c.y*r.z
    )
  }
  def outerProduct(c: inVec2f, r: inVec4f) :Mat2x4f = {
    new Mat2x4f(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y,
      c.x*r.z, c.y*r.z,
      c.x*r.w, c.y*r.w
    )
  }
  def outerProduct(c: inVec3f, r: inVec2f) :Mat3x2f = {
    new Mat3x2f(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y
    )
  }
  def outerProduct(c: inVec3f, r: inVec3f) :Mat3f = {
    new Mat3f(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z
    )
  }
  def outerProduct(c: inVec3f, r: inVec4f) :Mat3x4f = {
    new Mat3x4f(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z,
      c.x*r.w, c.y*r.w, c.z*r.w
    )
  }
  def outerProduct(c: inVec4f, r: inVec2f) :Mat4x2f = {
    new Mat4x2f(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y
    )
  }
  def outerProduct(c: inVec4f, r: inVec3f) :Mat4x3f = {
    new Mat4x3f(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z
    )
  }
  def outerProduct(c: inVec4f, r: inVec4f) :Mat4f = {
    new Mat4f(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z,
      c.x*r.w, c.y*r.w, c.z*r.w, c.w*r.w
    )
  }

  def transpose(a: inMat2f) :Mat2f = {
    new Mat2f(
      a.m00, a.m01,
      a.m10, a.m11
    )
  }
  def transpose(a: inMat3x2f) :Mat2x3f = {
    new Mat2x3f(
      a.m00, a.m01,
      a.m10, a.m11,
      a.m20, a.m21
    )
  }
  def transpose(a: inMat4x2f) :Mat2x4f = {
    new Mat2x4f(
      a.m00, a.m01,
      a.m10, a.m11,
      a.m20, a.m21,
      a.m30, a.m31
    )
  }
  def transpose(a: inMat2x3f) :Mat3x2f = {
    new Mat3x2f(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12
    )
  }
  def transpose(a: inMat3f) :Mat3f = {
    new Mat3f(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12,
      a.m20, a.m21, a.m22
    )
  }
  def transpose(a: inMat4x3f) :Mat3x4f = {
    new Mat3x4f(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12,
      a.m20, a.m21, a.m22,
      a.m30, a.m31, a.m32
    )
  }
  def transpose(a: inMat2x4f) :Mat4x2f = {
    new Mat4x2f(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13
    )
  }
  def transpose(a: inMat3x4f) :Mat4x3f = {
    new Mat4x3f(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13,
      a.m20, a.m21, a.m22, a.m23
    )
  }
  def transpose(a: inMat4f) :Mat4f = {
    new Mat4f(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13,
      a.m20, a.m21, a.m22, a.m23,
      a.m30, a.m31, a.m32, a.m33
    )
  }

  def determinant(m: inMat2f) :Float = m.m00*m.m11 - m.m01*m.m10

  def determinant(m: inMat3f) :Float = {
    import m._

    val c0 = m11*m22 - m12*m21
    val c1 = m12*m20 - m10*m22
    val c2 = m10*m21 - m11*m20

    m00*c0 + m01*c1 + m02*c2
  }

  def determinant(m: inMat4f) :Float = {
    import m._

    val a0 = m00*m11 - m01*m10
    val a1 = m00*m12 - m02*m10
    val a2 = m00*m13 - m03*m10
    val a3 = m01*m12 - m02*m11
    val a4 = m01*m13 - m03*m11
    val a5 = m02*m13 - m03*m12
    val b0 = m20*m31 - m21*m30
    val b1 = m20*m32 - m22*m30
    val b2 = m20*m33 - m23*m30
    val b3 = m21*m32 - m22*m31
    val b4 = m21*m33 - m23*m31
    val b5 = m22*m33 - m23*m32

    a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0
  }

  /**
   * If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat2f) :Mat2f = {
    val invDet = 1/determinant(m)
    new Mat2f(
       m.m11*invDet, -m.m10*invDet,
      -m.m01*invDet,  m.m00*invDet
    )
  }

  /**
   * This is a general matrix inverse. You can invert transofrmations
   * quicker by using Transform3f.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat3f) :Mat3f = {
    import m._

    val c0 = m11*m22 - m12*m21
    val c1 = m12*m20 - m10*m22
    val c2 = m10*m21 - m11*m20

    val invDet = 1/(m00*c0 + m01*c1 + m02*c2)

    new Mat3f(
      c0*invDet,
      c1*invDet,
      c2*invDet,

      (m02*m21 - m01*m22)*invDet,
      (m00*m22 - m02*m20)*invDet,
      (m01*m20 - m00*m21)*invDet,

      (m01*m12 - m02*m11)*invDet,
      (m02*m10 - m00*m12)*invDet,
      (m00*m11 - m01*m10)*invDet
    )
  }

  /**
   * This is a general matrix inverse. You can invert transofrmations
   * quicker by using Transform3f.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat4f) :Mat4f = {
    import m._

    val a0 = m00*m11 - m01*m10
    val a1 = m00*m12 - m02*m10
    val a2 = m00*m13 - m03*m10
    val a3 = m01*m12 - m02*m11
    val a4 = m01*m13 - m03*m11
    val a5 = m02*m13 - m03*m12
    val b0 = m20*m31 - m21*m30
    val b1 = m20*m32 - m22*m30
    val b2 = m20*m33 - m23*m30
    val b3 = m21*m32 - m22*m31
    val b4 = m21*m33 - m23*m31
    val b5 = m22*m33 - m23*m32

    val invDet = 1/(a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0)

    new Mat4f(
      ( m11*b5 - m12*b4 + m13*b3)*invDet,
      (-m10*b5 + m12*b2 - m13*b1)*invDet,
      ( m10*b4 - m11*b2 + m13*b0)*invDet,
      (-m10*b3 + m11*b1 - m12*b0)*invDet,

      (-m01*b5 + m02*b4 - m03*b3)*invDet,
      ( m00*b5 - m02*b2 + m03*b1)*invDet,
      (-m00*b4 + m01*b2 - m03*b0)*invDet,
      ( m00*b3 - m01*b1 + m02*b0)*invDet,

      ( m31*a5 - m32*a4 + m33*a3)*invDet,
      (-m30*a5 + m32*a2 - m33*a1)*invDet,
      ( m30*a4 - m31*a2 + m33*a0)*invDet,
      (-m30*a3 + m31*a1 - m32*a0)*invDet,

      (-m21*a5 + m22*a4 - m23*a3)*invDet,
      ( m20*a5 - m22*a2 + m23*a1)*invDet,
      (-m20*a4 + m21*a2 - m23*a0)*invDet,
      ( m20*a3 - m21*a1 + m22*a0)*invDet
    )
  }

  // *** Extra Math functions ************************************************

  def isnegzero(x: Float) :Boolean = (x == 0 && java.lang.Float.floatToRawIntBits(x) != 0)
  def isposzero(x: Float) :Boolean = (x == 0 && java.lang.Float.floatToRawIntBits(x) == 0)
  
  def isneginf(x: Float) :Boolean = isinf(x) && x < 0
  def isposinf(x: Float) :Boolean = isinf(x) && x > 0


  // Lerp
  def lerp(m: inMat2f, n: inMat2f, a: Float) :Mat2f = {
    val b = 1 - a

    new Mat2f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11
    )
  }
  def lerp(m: inMat2x3f, n: inMat2x3f, a: Float) :Mat2x3f = {
    val b = 1 - a

    new Mat2x3f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12
    )
  }
  def lerp(m: inMat2x4f, n: inMat2x4f, a: Float) :Mat2x4f = {
    val b = 1 - a

    new Mat2x4f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12,
      b*m.m03 + a*n.m03, b*m.m13 + a*n.m13
    )
  }
  def lerp(m: inMat3x2f, n: inMat3x2f, a: Float) :Mat3x2f = {
    val b = 1 - a

    new Mat3x2f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21
    )
  }
  def lerp(m: inMat3f, n: inMat3f, a: Float) :Mat3f = {
    val b = 1 - a

    new Mat3f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22
    )
  }
  def lerp(m: inMat3x4f, n: inMat3x4f, a: Float) :Mat3x4f = {
    val b = 1 - a

    new Mat3x4f(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22,
      b*m.m03 + a*n.m03, b*m.m13 + a*n.m13, b*m.m23 + a*n.m23
    )
  }
  def lerp(m: inMat4x2f, n: inMat4x2f, a: Float) :Mat4x2f = {
    val b = 1 - a

    new Mat4x2f(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31
    )
  }
  def lerp(m: inMat4x3f, n: inMat4x3f, a: Float) :Mat4x3f = {
    val b = 1 - a

    new Mat4x3f(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
      b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32
    )
  }
  def lerp(m: inMat4f, n: inMat4f, a: Float) :Mat4f = {
    val b = 1 - a

    new Mat4f(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
      b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32,
      b*m.m03 +a*n.m03, b*m.m13 +a*n.m13, b*m.m23 +a*n.m23, b*m.m33 +a*n.m33
    )
  }

  def hasErrors(x: Float) :Boolean = isinf(x) || isnan(x)
  def hasErrors(u: inVec2f) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y)
  )
  def hasErrors(u: inVec3f) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y) || hasErrors(u.z)
  )
  def hasErrors(u: inVec4f) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y) || hasErrors(u.z) || hasErrors(u.w)
  )
  def hasErrors(q: inQuat4f) :Boolean = (
    hasErrors(q.a) || hasErrors(q.b) || hasErrors(q.c) || hasErrors(q.d)
  )
  def hasErrors(m: inMat2f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)
  )
  def hasErrors(m: inMat2x3f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)||
    hasErrors(m.m02)|| hasErrors(m.m12)
  )
  def hasErrors(m: inMat2x4f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)||
    hasErrors(m.m02)|| hasErrors(m.m12)||
    hasErrors(m.m03)|| hasErrors(m.m13)
  )
  def hasErrors(m: inMat3x2f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)
  )
  def hasErrors(m: inMat3f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)
  )
  def hasErrors(m: inMat3x4f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)||
    hasErrors(m.m03)|| hasErrors(m.m13)|| hasErrors(m.m23)
  )
  def hasErrors(m: inMat4x2f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)
  )
  def hasErrors(m: inMat4x3f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)|| hasErrors(m.m32)
  )
  def hasErrors(m: inMat4f) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)|| hasErrors(m.m32)||
    hasErrors(m.m03)|| hasErrors(m.m13)|| hasErrors(m.m23)|| hasErrors(m.m33)
  )

  def approxEqual(x: Float, y: Float, absDelta: Float) :Boolean = {
    abs(x - y) < absDelta
  }
  def approxEqual(u: inVec2f, v: inVec2f, absDelta: Float) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta
  }
  def approxEqual(u: inVec3f, v: inVec3f, absDelta: Float) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta &&
    abs(v.z - u.z) < absDelta
  }
  def approxEqual(u: inVec4f, v: inVec4f, absDelta: Float) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta &&
    abs(v.z - u.z) < absDelta &&
    abs(v.w - u.w) < absDelta
  }
  def approxEqual(p: inQuat4f, q: inQuat4f, absDelta: Float) :Boolean = {
    abs(p.a - q.a) < absDelta &&
    abs(p.b - q.b) < absDelta &&
    abs(p.c - q.c) < absDelta &&
    abs(p.d - q.d) < absDelta
  }
  def approxEqual(m: inMat2f, n: inMat2f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta
    )
  }
  def approxEqual(m: inMat2x3f, n: inMat2x3f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta
    )
  }
  def approxEqual(m: inMat2x4f, n: inMat2x4f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta &&

      abs(n.m03 - m.m03) < absDelta &&
      abs(n.m13 - m.m13) < absDelta
    )
  }

  def approxEqual(m: inMat3x2f, n: inMat3x2f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta
    )
  }
  def approxEqual(m: inMat3f, n: inMat3f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta &&
      abs(n.m22 - m.m22) < absDelta
    )
  }
  def approxEqual(m: inMat3x4f, n: inMat3x4f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta &&
      abs(n.m22 - m.m22) < absDelta &&

      abs(n.m03 - m.m03) < absDelta &&
      abs(n.m13 - m.m13) < absDelta &&
      abs(n.m23 - m.m23) < absDelta
    )
  }
  def approxEqual(m: inMat4x2f, n: inMat4x2f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&
      abs(n.m30 - m.m30) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta &&
      abs(n.m31 - m.m31) < absDelta
    )
  }
  def approxEqual(m: inMat4x3f, n: inMat4x3f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&
      abs(n.m30 - m.m30) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta &&
      abs(n.m31 - m.m31) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta &&
      abs(n.m22 - m.m22) < absDelta &&
      abs(n.m32 - m.m32) < absDelta
    )
  }
  def approxEqual(m: inMat4f, n: inMat4f, absDelta: Float) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&
      abs(n.m30 - m.m30) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta &&
      abs(n.m31 - m.m31) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta &&
      abs(n.m22 - m.m22) < absDelta &&
      abs(n.m32 - m.m32) < absDelta &&

      abs(n.m03 - m.m03) < absDelta &&
      abs(n.m13 - m.m13) < absDelta &&
      abs(n.m23 - m.m23) < absDelta &&
      abs(n.m33 - m.m33) < absDelta
    )
  }

  /**
   * This method is equivalent to casting the matrix to 3x3, inverting it
   * and then casting the result back to 2x3.<br/>
   *
   * This is a general matrix inverse. You can invert transofrmations
   * quicker by using Transform2f.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat2x3f) :Mat2x3f = {
    import m._

    val invDet = 1/(m00*m11 - m01*m10)

    new Mat2x3f(
       m11*invDet, -m10*invDet,
      -m01*invDet,  m00*invDet,
      (m01*m12 - m02*m11)*invDet, (m02*m10 - m00*m12)*invDet
    )
  }

  /**
   * This method is equivalent to casting the matrix to 4x4, inverting it
   * and then casting the result back to 3x4.<br/>
   *
   * This is a general matrix inverse. You can invert transofrmations
   * quicker by using Transform3f.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat3x4f) :Mat3x4f = {
    import m._

    val a0 = m00*m11 - m01*m10
    val a1 = m00*m12 - m02*m10
    val a2 = m00*m13 - m03*m10
    val a3 = m01*m12 - m02*m11
    val a4 = m01*m13 - m03*m11
    val a5 = m02*m13 - m03*m12

    val invDet = 1/(a0*m22 - a1*m21 + a3*m20)

    new Mat3x4f(
      ( m11*m22 - m12*m21)*invDet,
      (-m10*m22 + m12*m20)*invDet,
      ( m10*m21 - m11*m20)*invDet,

      (-m01*m22 + m02*m21)*invDet,
      ( m00*m22 - m02*m20)*invDet,
      (-m00*m21 + m01*m20)*invDet,

       a3*invDet,
      -a1*invDet,
       a0*invDet,

      (-m21*a5 + m22*a4 - m23*a3)*invDet,
      ( m20*a5 - m22*a2 + m23*a1)*invDet,
      (-m20*a4 + m21*a2 - m23*a0)*invDet
    )
  }

  // Quaternion
  def normSquare(q: inQuat4f) :Float = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
  def norm(q: inQuat4f) :Float = {
    sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
  }
  def conjugate(q: inQuat4f) :Quat4f = new Quat4f(q.a, -q.b, -q.c, -q.d)

  def normalize(q: inQuat4f) :Quat4f = {
    q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
  }

  /**
   * This method is here for completness. Normally you should work with 
   * unit quaternions (`norm(q) == 1`), and in this case
   * `inverse(q) == conjugate(q)`.
   */
  def inverse(q: inQuat4f) :Quat4f = conjugate(q)/normSquare(q)

  def slerp(p: inQuat4f, q: inQuat4f, a: Float) :Quat4f = {
    if (approxEqual(p, q, 1e-5f)) return Quat4f(q)

    var cosTheta = p.a*q.a + p.b*q.b + p.c*q.c+ p.d*q.d
    var negate = false
    if (cosTheta < 0) {
      cosTheta = -cosTheta
      negate = true
    }

    var t = a
    var s = 1 - t

    if (cosTheta < 0.95f) {
      val theta = acos(cosTheta)
      val invSinTheta = 1/sin(theta)

      t = sin(t*theta)*invSinTheta
      s = sin(s*theta)*invSinTheta
      if (negate) t = -t
    }

    new Quat4f(
      s*p.a + t*q.a,
      s*p.b + t*q.b,
      s*p.c + t*q.c,
      s*p.d + t*q.d
    )
  }

  /**
   * The quaternion must have unit norm to achieve the desired result.
   */
  def rotateVector(u: inVec3f, q: inQuat4f) = {
    import q._

    val t1 = a*b
    val t2 = a*c
    val t3 = a*d
    val t4 = -b*b
    val t5 = b*c
    val t6 = b*d
    val t7 = -c*c
    val t8 = c*d
    val t9 = -d*d

    new Vec3f(
      2*((t7 + t9)*u.x + (t5 - t3)*u.y + (t2 + t6)*u.z) + u.x,
      2*((t3 + t5)*u.x + (t4 + t9)*u.y + (t8 - t1)*u.z) + u.y,
      2*((t6 - t2)*u.x + (t1 + t8)*u.y + (t4 + t7)*u.z) + u.z
    )
  }

  // Rotation
  /**
   * This method creates a 2d transformation matrix that rotates a vector
   * counterclockwise by the specified angle.
   */
  def rotationMat(angle: Float) :Mat2f = {
    val cosA = cos(angle)
    val sinA = sin(angle)

    new Mat2f(
       cosA, sinA,
      -sinA, cosA
    )
  }

  /**
   * The matrix must represent non-scaling rotation to achieve
   * the desired result.
   */
  def rotationAngle(m: inMat2f) :Float = {
    acos((m.m00 + m.m11)*0.5f)
  }

  /**
   * The matrix must represent non-scaling rotation to achieve
   * the desired result.
   */
  def quaternion(m: inMat3f) :Quat4f = {
    import m._

    val trace = m00 + m11 + m22

    if (trace > 0) {
      val t = trace + 1
      val s = inversesqrt(t)*0.5f
      new Quat4f(
        s*t,
        (m21 - m12)*s,
        (m02 - m20)*s,
        (m10 - m01)*s
      )
    }
    else if (m00 > m11 && m00 > m22) {
      val t = m00 - m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      new Quat4f(
        (m21 - m12)*s,
        s*t,
        (m10 + m01)*s,
        (m02 + m20)*s
      )
    }
    else if (m11 > m22) {
      val t = -m00 + m11 - m22 + 1
      val s = inversesqrt(t)*0.5f
      new Quat4f(
        (m02 - m20)*s,
        (m10 + m01)*s,
        s*t,
        (m21 + m12)*s
      )
    }
    else {
      val t = -m00 - m11 + m22 + 1
      val s = inversesqrt(t)*0.5f
      new Quat4f(
        (m10 - m01)*s,
        (m02 + m20)*s,
        (m21 + m12)*s,
        s*t
      )
    }
  }
  /**
   * The axis must have unit length to achieve the desired result.
   */
  def quaternion(angle: Float, axis: inVec3f) :Quat4f = {
    val halfAngle = angle*0.5f
    val s = sin(halfAngle)
    new Quat4f(cos(halfAngle), s*axis.x, s*axis.y, s*axis.z)
  }

  /**
   * The quaternion must have unit norm to achieve the desired result.
   */
  def rotationMat(q: inQuat4f) :Mat3f = {
    import q._

    val tb = 2*b*b
    val tc = 1 - 2*c*c
    val td = 2*d*d
    val bc = 2*b*c
    val da = 2*d*a
    val bd = 2*b*d
    val ca = 2*c*a
    val cd = 2*c*d
    val ba = 2*b*a

    new Mat3f(
      tc - td, bc + da, bd - ca,
      bc - da, 1 - tb - td, cd + ba,
      bd + ca, cd - ba, tc - tb
    )
  }
  /**
   * The axis must have unit length to achieve the desired result.
   */
  def rotationMat(angle: Float, axis: inVec3f) :Mat3f = {
    import axis._

    val sinA = sin(angle)
    val cosA = cos(angle)
    val c = 1 - cosA
    val sx = sinA*x
    val sy = sinA*y
    val sz = sinA*z
    val temp = c*x
    val cxy = temp*y
    val cxz = temp*z
    val cyz = c*y*z

    new Mat3f(
      cosA + c*x*x, cxy + sz, cxz - sy,
      cxy - sz, cosA + c*y*y, cyz + sx,
      cxz + sy, cyz - sx, cosA + c*z*z
    )
  }

  /**
   * The quaternion must have unit norm to achieve the desired result.
   * If quaternion represents 0 degree rotation, then rotation
   * axis is not defined, in this case the UnitX axis is chosen.
   */
  def angleAxis(q: inQuat4f, axis: outVec3f) :Float = {
    import q._

    if (approxEqual(abs(a), 1, 1e-6f)) {
      axis := Vec3f.UnitX
      return 0
    }

    val t = inversesqrt(1 - a*a)
    axis.x = b*t
    axis.y = c*t
    axis.z = d*t

    2*acos(a)
  }
  /**
   * The matrix must represent non-scaling rotation to achieve
   * the desired result. If the matrix represents 0 degree rotation,
   * then rotation axis is undefined, in this case the UnitX axis is chosen.
   */
  def angleAxis(m: inMat3f, axis: outVec3f) :Float = {
    import m._

    val cosAngle = (m00 + m11 + m22 - 1)*0.5f

    if (approxEqual(cosAngle, 1, 1e-5f)) {
      axis := Vec3f.UnitX
      return 0
    }
    else if (approxEqual(cosAngle, -1, 1e-5f)) {
      if (m00 > m11 && m00 > m22) {
        val r = sqrt((m00 + 1)*0.5f)
        val t = 1/(4*r)
        axis.x = r
        axis.y = (m01 + m10)*t
        axis.z = (m02 + m20)*t
      }
      else if (m11 > m22) {
        val r = sqrt((m11 + 1)*0.5f)
        val t = 1/(4*r)
        axis.y = r
        axis.x = (m01 + m10)*t
        axis.z = (m12 + m21)*t
      }
      else {
        val r = sqrt((m22 + 1)*0.5f)
        val t = 1/(4*r)
        axis.z = r
        axis.x = (m02 + m20)*t
        axis.y = (m12 + m21)*t
      }
      return Pi
    }

    val t0 = (m21 - m12)
    val t1 = (m02 - m20)
    val t2 = (m10 - m01)
    val t = inversesqrt(t0*t0 + t1*t1 + t2*t2)
    axis.x = (m21 - m12)*t
    axis.y = (m02 - m20)*t
    axis.z = (m10 - m01)*t

    acos(cosAngle)
  }

  def lookAt(direction: inVec3f, up: inVec3f) :Mat3f = {
    // zaxis = normalize(direction)
    val dirinvlen = inversesqrt(dot(direction, direction))
    val zax = direction.x*dirinvlen
    val zay = direction.y*dirinvlen
    val zaz = direction.z*dirinvlen

    // xaxis = cross(up, zaxis)
    var xax = up.y*zaz - zay*up.z
    var xay = up.z*zax - zaz*up.x
    var xaz = up.x*zay - zax*up.y

    // xaxis = normalize(xaxis)
    val invlen = inversesqrt(xax*xax + xay*xay + xaz*xaz)
    xax *= invlen
    xay *= invlen
    xaz *= invlen

    // yaxis = cross(zaxis, xaxis)
    val yax = zay*xaz - xay*zaz
    val yay = zaz*xax - xaz*zax
    val yaz = zax*xay - xax*zay

    // Mat3x3(xaxis, yaxis, zaxis)
    new Mat3f(
      xax, xay, xaz,
      yax, yay, yaz,
      zax, zay, zaz
    )
  }

  // Projection
  /**
   * @param fieldOfView field of view angle in y direction (in radians).
   * @param aspectRatio width/height aspect ratio.
   * @param near the distance to the near clipping plane, must be positive,
   *   approximately log2(far/near) bits of depth buffer precision are lost.
   * @param far the distance to the far clipping plane, must be positive.
   */
  def perspectiveProj(
    fieldOfView: Float, aspectRatio: Float,
    near: Float, far: Float
  ) :Mat4f = {
    val focus = 1/tan(fieldOfView * 0.5f)
    val inv_nf = 1/(near - far)

    new Mat4f(
      focus/aspectRatio, 0, 0, 0,
      0, focus, 0, 0,
      0, 0, (near + far)*inv_nf, -1,
      0, 0, 2*near*far*inv_nf, 0
    )
  }

  /**
   * @param left the coordinates of the left clipping plane.
   * @param right the coordinates of the right clipping plane.
   * @param bottom the coordinates of the bottom clipping plane.
   * @param top the coordinates of the top clipping plane.
   * @param near the distance to the near clipping plane, must be positive,
   *   approximately log2(far/near) bits of depth buffer precision are lost.
   * @param far the distance to the far clipping plane, must be positive.
   */
  def perspectiveProj(
    left: Float, right: Float,
    bottom: Float, top: Float,
    near: Float, far: Float
  ) :Mat4f = {
    val near2 = near*2
    val inv_rl = 1/(right - left)
    val inv_tb = 1/(top - bottom)
    val inv_nf = 1/(near - far)

    new Mat4f(
      near2*inv_rl, 0, 0, 0,
      0, near2*inv_tb, 0, 0,
      (right + left)*inv_rl, (top + bottom)*inv_tb, (near + far)*inv_nf, -1,
      0, 0, near2*far*inv_nf, 0
    )
  }

  /**
   * @param left the coordinates of the left clipping plane.
   * @param right the coordinates of the right clipping plane.
   * @param bottom the coordinates of the bottom clipping plane.
   * @param top the coordinates of the top clipping plane.
   * @param near the distance to the near clipping plane, negative if the plane is behind the viewer.
   * @param far the distance to the far clipping plane, negative if the plane is behind the viewer.
   */
  def orthoProj(
    left: Float, right: Float,
    bottom: Float, top: Float,
    near: Float, far: Float
  ) :Mat4f = {
    val r_l = 1/(right - left);
    val t_b = 1/(top - bottom);
    val f_n = 1/(far - near);

    new Mat4f(
      2*r_l, 0, 0, 0,
      0, 2*t_b, 0, 0,
      0, 0, -2*f_n, 0,
      -(right + left)*r_l, -(top + bottom)*t_b, -(far + near)*f_n, 1
    )
  }

  // Transformation
  def transformation(
    scale: inVec2f,
    rotation: inMat2f,
    translation: inVec2f
  ) :Mat2x3f = {
    import rotation._
    import translation.{x => tx, y => ty}
    import scale.{x => sx, y => sy}

    new Mat2x3f(
      m00*sx, m10*sx,
      m01*sy, m11*sy,
      tx, ty
    )
  }

  /**
   * @param rotation Must be an orthogonal matrix (matrix that represents
   * an unscaled rotation) to achieve the desired result.
   */
  def inverseTransformation(
    scale: inVec2f,
    rotation: inMat2f,
    translation: inVec2f
  ) :Mat2x3f = {
    import translation.{x => tx, y => ty}

    val sx = 1/scale.x
    val sy = 1/scale.y

    val m00 = rotation.m00*sx
    val m10 = rotation.m01*sy
    val m01 = rotation.m10*sx
    val m11 = rotation.m11*sy

    new Mat2x3f(
      m00, m10,
      m01, m11,
      -m00*tx - m01*ty,
      -m10*tx - m11*ty
    )
  }
  
  def transformation(
    scale: inVec3f,
    rotation: inMat3f,
    translation: inVec3f
  ) :Mat3x4f = {
    import scale.{x => sx, y => sy, z => sz}
    import rotation._
    import translation.{x => tx, y => ty, z => tz}

    new Mat3x4f(
      m00*sx, m10*sx, m20*sx,
      m01*sy, m11*sy, m21*sy,
      m02*sz, m12*sz, m22*sz,
      tx, ty, tz
    )
  }

  /**
   * @param rotation Must be an orthogonal matrix (matrix that represents
   * an unscaled rotation) to achieve the desired result.
   */
  def inverseTransformation(
    scale: inVec3f,
    rotation: inMat3f,
    translation: inVec3f
  ) :Mat3x4f = {
    import translation.{x => tx, y => ty, z => tz}

    val sx = 1/scale.x
    val sy = 1/scale.y
    val sz = 1/scale.z

    val m00 = rotation.m00*sx
    val m10 = rotation.m01*sy
    val m20 = rotation.m02*sz
    val m01 = rotation.m10*sx
    val m11 = rotation.m11*sy
    val m21 = rotation.m12*sz
    val m02 = rotation.m20*sx
    val m12 = rotation.m21*sy
    val m22 = rotation.m22*sz

    new Mat3x4f(
      m00, m10, m20,
      m01, m11, m21,
      m02, m12, m22,
      -m00*tx - m01*ty - m02*tz,
      -m10*tx - m11*ty - m12*tz,
      -m20*tx - m21*ty - m22*tz
    )
  }
}
