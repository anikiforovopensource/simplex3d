/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublex

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
  final val Epsilon = 2.22045e-16
  
  /** Constant <i>pi</i>.
   */
  final val Pi = JMath.PI
  
  /** Constant <i>e</i>.
   */
  final val E = JMath.E

  private final val DegToRad = 0.01745329251994329577
  private final val RadToDeg = 57.2957795130823208768
  private final val InvLog2 = 1.44269504088896340736


  // Copied here until the scala compiler can resolve inherited overloaded functions.
  // Int functions

  /** Returns an absolute value of the argument.
   * @param x an integer argument.
   * @return an absolute value of the agument.
   */
  final def abs(x: Int) :Int = if (x < 0) -x else x

  /** Returns the sign of the argument.
   * @param x an integer argument.
   * @return 1 if <i>x</i> > 0; -1 if <i>x</i> < 0; 0 otherwise.
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

  /** Clamps a value to a given range. The result is undefined if <i>minVal</i> > <i>maxVal</i>.
   * @param x an integer value to clamp.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return <i>minVal</i> if <i>x</i> < <i>minVal</i>; <i>maxVal</i> if <i>x</i> > <i>maxVal</i>; <i>x</i> otherwise.
   */
  final def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
    if (x <= minVal) minVal
    else if (x >= maxVal) maxVal
    else x
  }

  // Vec2i functions
  /** Performs component-wise <b>abs</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of <i>u</i>.
   */
  final def abs(u: inVec2i) :Vec2i = new Vec2i(abs(u.x), abs(u.y))
  
  /** Performs component-wise <b>sign</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to sign of components of <i>u</i>.
   */
  final def sign(u: inVec2i) :Vec2i = new Vec2i(sign(u.x), sign(u.y))
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>min(c, s)</code> for each component <i>c</i>.
   */
  final def min(u: inVec2i, s: Int) :Vec2i = new Vec2i(min(u.x, s), min(u.y, s))
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>min(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def min(u: inVec2i, v: inVec2i) :Vec2i = new Vec2i(min(u.x, v.x), min(u.y, v.y))
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>max(c, s)</code> for each component <i>c</i>.
   */
  final def max(u: inVec2i, s: Int) :Vec2i = new Vec2i(max(u.x, s), max(u.y, s))
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>max(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def max(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(max(u.x, v.x), max(u.y, v.y))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [<i>minValue</i>, <i>maxValue</i>].
   */
  final def clamp(u: inVec2i, minVal: Int, maxVal: Int) :Vec2i = {
    new Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  
  /** Performs component-wise <b>clamp</b> operation.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to <code>clamp(u.c, minVal.c, maxVal.c)</code> for each component <i>c</i>.
   */
  final def clamp(u: inVec2i, minVal: inVec2i, maxVal: inVec2i) :Vec2i = {
    new Vec2i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }

  /** Performs component-wise <b>less than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c < v.c)</code> for each component <i>c</i>.
   */
  final def lessThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  
  /** Performs component-wise <b>less than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c <= v.c)</code> for each component <i>c</i>.
   */
  final def lessThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  
  /** Performs component-wise <b>greater than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c > v.c)</code> for each component <i>c</i>.
   */
  final def greaterThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  
  /** Performs component-wise <b>greater than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c >= v.c)</code> for each component <i>c</i>.
   */
  final def greaterThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  
  /** Performs component-wise <b>equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c == v.c)</code> for each component <i>c</i>.
   */
  final def equal(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  
  /** Performs component-wise <b>not equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c != v.c)</code> for each component <i>c</i>.
   */
  final def notEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  // Vec3i functions
  /** Performs component-wise <b>abs</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of <i>u</i>.
   */
  final def abs(u: inVec3i) :Vec3i = new Vec3i(abs(u.x), abs(u.y), abs(u.z))
  
  /** Performs component-wise <b>sign</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to sign of components of <i>u</i>.
   */
  final def sign(u: inVec3i) :Vec3i = new Vec3i(sign(u.x), sign(u.y), sign(u.z))
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>min(c, s)</code> for each component <i>c</i>.
   */
  final def min(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>min(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def min(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>max(c, s)</code> for each component <i>c</i>.
   */
  final def max(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>max(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def max(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [<i>minValue</i>, <i>maxValue</i>].
   */
  final def clamp(u: inVec3i, minVal: Int, maxVal: Int) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  
  /** Performs component-wise <b>clamp</b> operation.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to <code>clamp(u.c, minVal.c, maxVal.c)</code> for each component <i>c</i>.
   */
  final def clamp(u: inVec3i, minVal: inVec3i, maxVal: inVec3i) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }

    /** Performs component-wise <b>less than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c < v.c)</code> for each component <i>c</i>.
   */
  final def lessThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  
  /** Performs component-wise <b>less than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c <= v.c)</code> for each component <i>c</i>.
   */
  final def lessThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  
  /** Performs component-wise <b>greater than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c > v.c)</code> for each component <i>c</i>.
   */
  final def greaterThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  
  /** Performs component-wise <b>greater than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c >= v.c)</code> for each component <i>c</i>.
   */
  final def greaterThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  
  /** Performs component-wise <b>equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c == v.c)</code> for each component <i>c</i>.
   */
  final def equal(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  
  /** Performs component-wise <b>not equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c != v.c)</code> for each component <i>c</i>.
   */
  final def notEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  // Vec4i functions
  /** Performs component-wise <b>abs</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to absolute values of components of <i>u</i>.
   */
  final def abs(u: inVec4i) :Vec4i = {
    new Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  
  /** Performs component-wise <b>sign</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to sign of components of <i>u</i>.
   */
  final def sign(u: inVec4i) :Vec4i = {
    new Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>min(c, s)</code> for each component <i>c</i>.
   */
  final def min(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  
  /** Performs component-wise <b>min</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>min(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def min(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @return a vector with components set to <code>max(c, s)</code> for each component <i>c</i>.
   */
  final def max(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  
  /** Performs component-wise <b>max</b> operation.
   * @param u an integer vector argument.
   * @param v an integer vector argument.
   * @return a vector with components set to <code>max(u.c, v.c)</code> for each component <i>c</i>.
   */
  final def max(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  
  /** Clamps components to the range from minValue (inclusive) to maxValue (inclusive).
   * @param u an integer vector argument.
   * @param minVal the lower bound (inclusive).
   * @param maxVal the upper bound (inclusive).
   * @return a vector with components clamped to [<i>minValue</i>, <i>maxValue</i>].
   */
  final def clamp(u: inVec4i, minVal: Int, maxVal: Int) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  
  /** Performs component-wise <b>clamp</b> operation.
   * @param u an integer vector argument.
   * @param minVal a vector with components used as lower bound (inclusive).
   * @param maxVal a vector with components used as upper bound (inclusive).
   * @return a vector with components set to <code>clamp(u.c, minVal.c, maxVal.c)</code> for each component <i>c</i>.
   */
  final def clamp(u: inVec4i, minVal: inVec4i, maxVal: inVec4i) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }

  /** Performs component-wise <b>less than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c < v.c)</code> for each component <i>c</i>.
   */
  final def lessThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  
  /** Performs component-wise <b>less than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c <= v.c)</code> for each component <i>c</i>.
   */
  final def lessThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  
  /** Performs component-wise <b>greater than</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c > v.c)</code> for each component <i>c</i>.
   */
  final def greaterThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  
  /** Performs component-wise <b>greater than or equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c >= v.c)</code> for each component <i>c</i>.
   */
  final def greaterThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  
  /** Performs component-wise <b>equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c == v.c)</code> for each component <i>c</i>.
   */
  final def equal(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  
  /** Performs component-wise <b>not equal</b> comparison.
   * @param u an integer vector on the left-hand side of comparison.
   * @param v an integer vector on the right-hand side of comparison.
   * @return a boolean vector with components set to <code>(u.c != v.c)</code> for each component <i>c</i>.
   */
  final def notEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }


  // Double functions
  def radians(x: Double) :Double = x*DegToRad
  def degrees(x: Double) :Double = x*RadToDeg

  def sin(w: Double) :Double = JMath.sin(w)
  def cos(w: Double) :Double = JMath.cos(w)
  def tan(w: Double) :Double = JMath.tan(w)

  def asin(w: Double) :Double = JMath.asin(w)
  def acos(w: Double) :Double = JMath.acos(w)
  def atan(y: Double, x: Double) :Double = JMath.atan2(y, x)
  def atan(w: Double) :Double = JMath.atan(w)

  def sinh(x: Double) :Double = JMath.sinh(x)
  def cosh(x: Double) :Double = JMath.cosh(x)
  def tanh(x: Double) :Double = JMath.tanh(x)

  def asinh(x: Double) :Double = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) -JMath.log(-x + JMath.sqrt(x*x + 1))
    else JMath.log(x + JMath.sqrt(x*x + 1))
  }
  def acosh(x: Double) :Double = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) scala.Double.NaN
    else JMath.log(x + JMath.sqrt(x*x - 1))
  }
  def atanh(x: Double) :Double = {
    // Possibly replace with a more accurate implementation.
    if (x < 0) -atanh(-x)
    else if (x < 0.2) 0.5*JMath.log1p(2*x/(1 - x))
    else 0.5*JMath.log((1 + x)/(1 - x))
  }

  def pow(x: Double, y: Double) :Double = JMath.pow(x, y)
  def exp(x: Double) :Double = JMath.exp(x)
  def log(x: Double) :Double = JMath.log(x)

  def exp2(x: Double) :Double = JMath.pow(2, x)
  def log2(x: Double) :Double = JMath.log(x)*InvLog2

  def sqrt(s: Double) :Double = JMath.sqrt(s)
  def inversesqrt(s: Double) :Double = 1/JMath.sqrt(s)

  def abs(x: Double) :Double = { if (x > 0) x else if (x == 0) 0 else -x }
  def sign(x: Double) :Double = {
    if (x > 0) 1
    else if (x < 0) -1
    else x // preserves nan and -0
  }
  def floor(x: Double) :Double = {
    if (x > 0) {
      if (x > scala.Long.MaxValue) x
      else {
        x.toLong
      }
    }
    else if (x < 0) {
      if (x < scala.Long.MinValue) x
      else {
        val i = x.toLong
        if (x == i) i else i - 1
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  def trunc(x: Double) :Double = {
    if (x > 0) {
      if (x > scala.Long.MaxValue) x
      else if (x >= 1) x.toLong
      else 0
    }
    else if (x < 0) {
      if (x < scala.Long.MinValue) x
      else if (x <= -1) x.toLong
      else -0d
    }
    else {
      x // preserves nan and -0
    }
  }
  def round(x: Double) :Double = {
    if (x > 0) {
      if (x > scala.Long.MaxValue) x
      else {
        val f = x + 0.5
        f.toLong
      }
    }
    else if (x < 0) {
      if (x < scala.Long.MinValue) x
      else if (x >= -0.5) -0d
      else {
        val f = x + 0.5
        val i = f.toLong
        if (f == i) f else i - 1
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  def roundEven(x: Double) :Double = JMath.rint(x)
  def ceil(x: Double) :Double = {
    if (x > 0) {
      if (x > scala.Long.MaxValue) x
      else {
        val i = x.toLong
        if (x == i) x else i + 1
      }
    }
    else if (x < 0) {
      if (x < scala.Long.MinValue) x
      else if (x > -1) -0d
      else {
        x.toLong
      }
    }
    else {
      x // preserves nan and -0
    }
  }
  /**
   * Equivalent to <code>x - floor(x)</code>
   */
  def fract(x: Double) :Double = {
    if (isnegzero(x)) -0d
    else if (isinf(x)) 0
    else x - floor(x)
  }
  /**
   * Equivalent to <code>x - y*floor(x/y)</code>
   */
  def mod(x: Double, y: Double) :Double = {
    if (isinf(x)) scala.Double.NaN
    else x - y*floor(x/y)
  }

  def min(x: Double, y: Double) :Double = {
    if (y == 0 && isnegzero(x)) x
    else if (x < y || isnan(x)) x
    else y
  }
  def max(x: Double, y: Double) :Double = {
    if (x == 0 && isnegzero(y)) x
    else if (x > y || isnan(x)) x
    else y
  }
  
  def clamp(x: Double, minVal: Double, maxVal: Double) :Double = {
    if (minVal > maxVal) scala.Double.NaN
    else if (x <= minVal) minVal
    else if (x >= maxVal) maxVal
    else x
  }

  def mix(x: Double, y: Double, a: Double) :Double = x*(1 - a) + y*a
  def step(edge: Double, x: Double) :Double = if (x < edge) 0 else if (isnan(x)) x else 1
  def smoothstep(edge0: Double, edge1: Double, x: Double) :Double = {
    if (edge0 > edge1) scala.Double.NaN
    else if (x <= edge0) 0
    else if (x >= edge1) 1
    else {
      val t = (x - edge0)/(edge1 - edge0)
      t*t*(3 - 2*t)
    }
  }

  def isnan(x: Double) :Boolean = java.lang.Double.isNaN(x)
  def isinf(x: Double) :Boolean = java.lang.Double.isInfinite(x)

  def length(x: Double) :Double = abs(x)
  def distance(x: Double, y: Double) :Double = abs(x - y)
  def dot(x: Double, y: Double) :Double = x*y
  def normalize(x: Double) :Double = {
    if (x > 0) 1
    else if (x < 0) -1
    else scala.Double.NaN
  }
  def faceforward(n: Double, i: Double, nref: Double) :Double = {
    if (i*nref < 0) n else -n
  }

  def reflect(i: Double, n: Double) :Double = {
    i - 2*(n*i)*n
  }
  def refract(i: Double, n: Double, eta: Double) :Double = {
    val ni = n*i
    val k = 1 - eta*eta*(1 - ni*ni)
    if (k < 0) 0 else eta*i - (eta*ni + sqrt(k))*n
  }

  /**
   * noise is 0 at multiples of length(simplex side),
   * simplex side is 1/sqrt(2) = 0.7071067811865475244
   * meaningful return values for x withing [-2E8, +2E8]
   */
  def noise1(x: Double) :Double = noise(x)
  def noise2(x: Double) :Vec2d = {
    new Vec2d(
      noise(x),
      noise(x + offset1)
    )
  }
  def noise3(x: Double) :Vec3d = {
    new Vec3d(
      noise(x),
      noise(x + offset1),
      noise(x + offset2)
    )
  }
  def noise4(x: Double) :Vec4d = {
    new Vec4d(
      noise(x),
      noise(x + offset1),
      noise(x + offset2),
      noise(x + offset3)
    )
  }

  // Vec2d functions
  def radians(u: inVec2d) :Vec2d = new Vec2d(radians(u.x), radians(u.y))
  def degrees(u: inVec2d) :Vec2d = new Vec2d(degrees(u.x), degrees(u.y))

  def sin(u: inVec2d) :Vec2d = new Vec2d(sin(u.x), sin(u.y))
  def cos(u: inVec2d) :Vec2d = new Vec2d(cos(u.x), cos(u.y))
  def tan(u: inVec2d) :Vec2d = new Vec2d(tan(u.x), tan(u.y))

  def asin(u: inVec2d) :Vec2d = new Vec2d(asin(u.x), asin(u.y))
  def acos(u: inVec2d) :Vec2d = new Vec2d(acos(u.x), acos(u.y))
  def atan(uy: inVec2d, ux: inVec2d) :Vec2d = {
    new Vec2d(atan(uy.x, ux.x), atan(uy.y, ux.y))
  }
  def atan(u: inVec2d) :Vec2d = new Vec2d(atan(u.x), atan(u.y))

  def sinh(u: inVec2d) :Vec2d = new Vec2d(sinh(u.x), sinh(u.y))
  def cosh(u: inVec2d) :Vec2d = new Vec2d(cosh(u.x), cosh(u.y))
  def tanh(u: inVec2d) :Vec2d = new Vec2d(tanh(u.x), tanh(u.y))

  def asinh(u: inVec2d) :Vec2d = new Vec2d(asinh(u.x), asinh(u.y))
  def acosh(u: inVec2d) :Vec2d = new Vec2d(acosh(u.x), acosh(u.y))
  def atanh(u: inVec2d) :Vec2d = new Vec2d(atanh(u.x), atanh(u.y))

  def pow(u: inVec2d, v: inVec2d) :Vec2d = {
    new Vec2d(pow(u.x, v.x), pow(u.y, v.y))
  }
  def exp(u: inVec2d) :Vec2d = new Vec2d(exp(u.x), exp(u.y))
  def log(u: inVec2d) :Vec2d = new Vec2d(log(u.x), log(u.y))

  def exp2(u: inVec2d) :Vec2d = new Vec2d(exp2(u.x), exp2(u.y))
  def log2(u: inVec2d) :Vec2d = new Vec2d(log2(u.x), log2(u.y))

  def sqrt(u: inVec2d) :Vec2d = new Vec2d(sqrt(u.x), sqrt(u.y))
  def inversesqrt(u: inVec2d) :Vec2d = {
    new Vec2d(inversesqrt(u.x), inversesqrt(u.y))
  }

  def abs(u: inVec2d) :Vec2d = new Vec2d(abs(u.x), abs(u.y))
  def sign(u: inVec2d) :Vec2d = new Vec2d(sign(u.x), sign(u.y))
  def floor(u: inVec2d) :Vec2d = new Vec2d(floor(u.x), floor(u.y))
  def trunc(u: inVec2d) :Vec2d = new Vec2d(trunc(u.x), trunc(u.y))
  def round(u: inVec2d) :Vec2d = new Vec2d(round(u.x), round(u.y))
  def roundEven(u: inVec2d) :Vec2d = {
    new Vec2d(roundEven(u.x), roundEven(u.y))
  }
  def ceil(u: inVec2d) :Vec2d = new Vec2d(ceil(u.x), ceil(u.y))
  def fract(u: inVec2d) :Vec2d = new Vec2d(fract(u.x), fract(u.y))
  def mod(u: inVec2d, s: Double) :Vec2d = new Vec2d(mod(u.x, s), mod(u.y, s))
  def mod(u: inVec2d, v: inVec2d) :Vec2d = {
    new Vec2d(mod(u.x, v.x), mod(u.y, v.y))
  }

  /** Separates each component of a given vector into fractional and integer
   * parts, both parts will have the same sign as the component.
   *
   * @param u the vector to be separated into fractional and integer parts.
   * @param i a result vector to store the integer parts of components of u.
   * @return a vector with fractional parts of components of u.
   */
  def modf(u: inVec2d, i: outVec2d) :Vec2d = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    u - i
  }

  def min(u: inVec2d, s: Double) :Vec2d = new Vec2d(min(u.x, s), min(u.y, s))
  def min(u: inVec2d, v: inVec2d) :Vec2d = {
    new Vec2d(min(u.x, v.x), min(u.y, v.y))
  }
  def max(u: inVec2d, s: Double) :Vec2d = new Vec2d(max(u.x, s), max(u.y, s))
  def max(u: inVec2d, v: inVec2d) :Vec2d = {
    new Vec2d(max(u.x, v.x), max(u.y, v.y))
  }
  def clamp(u: inVec2d, minVal: Double, maxVal: Double) :Vec2d = {
    new Vec2d(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  def clamp(u: inVec2d, minVal: inVec2d, maxVal: inVec2d) :Vec2d = {
    new Vec2d(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }

  def mix(u: inVec2d, v: inVec2d, a: Double) :Vec2d = {
    val b = 1 - a
    new Vec2d(b*u.x + a*v.x, b*u.y + a*v.y)
  }
  def mix(u: inVec2d, v: inVec2d, a: inVec2d) :Vec2d = {
    new Vec2d(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y))
  }
  def mix(u: inVec2d, v: inVec2d, a: inVec2b) :Vec2d = {
    new Vec2d(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y
    )
  }

  def step(edge: Double, u: inVec2d) :Vec2d = {
    new Vec2d(step(edge, u.x), step(edge, u.y))
  }
  def step(edge: inVec2d, u: inVec2d) :Vec2d = {
    new Vec2d(step(edge.x, u.x), step(edge.y, u.y))
  }
  def smoothstep(edge0: Double, edge1: Double, u: inVec2d) :Vec2d = {
    new Vec2d(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y)
    )
  }
  def smoothstep(edge0: inVec2d, edge1: inVec2d, u: inVec2d) :Vec2d = {
    new Vec2d(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y)
    )
  }

  def isnan(u: inVec2d) :Vec2b = new Vec2b(isnan(u.x), isnan(u.y))
  def isinf(u: inVec2d) :Vec2b = new Vec2b(isinf(u.x), isinf(u.y))
  
  def length(u: inVec2d) :Double = sqrt(u.x*u.x + u.y*u.y)
  def distance(u: inVec2d, v: inVec2d) :Double = {
    val x = u.x - v.x
    val y = u.y - v.y
    sqrt(x*x + y*y)
  }
  def dot(u: inVec2d, v: inVec2d) :Double = u.x*v.x + u.y*v.y
  def normalize(u: inVec2d) :Vec2d = u*inversesqrt(u.x*u.x + u.y*u.y)

  def faceforward(n: inVec2d, i: inVec2d, nref: inVec2d) :Vec2d = {
    if (dot(nref, i) < 0) Vec2d(n) else -n
  }

  def reflect(i: inVec2d, n: inVec2d) :Vec2d = {
    val t = -2*dot(n, i)
    new Vec2d(
      i.x + n.x*t,
      i.y + n.y*t
    )
  }
  def refract(i: inVec2d, n: inVec2d, eta: Double) :Vec2d = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec2d(0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec2d(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t
      )
    }
  }

  def lessThan(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  def lessThanEqual(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  def greaterThan(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  def greaterThanEqual(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  def equal(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  def notEqual(u: inVec2d, v: inVec2d) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  def noise1(u: inVec2d) :Double = {
    noise(u.x, u.y)
  }
  def noise2(u: inVec2d) :Vec2d = {
    new Vec2d(
      noise(u.x, u.y),
      noise(u.x + offset1, u.y + offset1)
    )
  }
  def noise3(u: inVec2d) :Vec3d = {
    new Vec3d(
      noise(u.x, u.y),
      noise(u.x + offset1, u.y + offset1),
      noise(u.x + offset2, u.y + offset2)
    )
  }
  def noise4(u: inVec2d) :Vec4d = {
    new Vec4d(
      noise(u.x, u.y),
      noise(u.x + offset1, u.y + offset1),
      noise(u.x + offset2, u.y + offset2),
      noise(u.x + offset3, u.y + offset3)
    )
  }


  // Vec3d functions
  def radians(u: inVec3d) :Vec3d = {
    new Vec3d(radians(u.x), radians(u.y), radians(u.z))
  }
  def degrees(u: inVec3d) :Vec3d = {
    new Vec3d(degrees(u.x), degrees(u.y), degrees(u.z))
  }

  def sin(u: inVec3d) :Vec3d = new Vec3d(sin(u.x), sin(u.y), sin(u.z))
  def cos(u: inVec3d) :Vec3d = new Vec3d(cos(u.x), cos(u.y), cos(u.z))
  def tan(u: inVec3d) :Vec3d = new Vec3d(tan(u.x), tan(u.y), tan(u.z))

  def asin(u: inVec3d) :Vec3d = new Vec3d(asin(u.x), asin(u.y), asin(u.z))
  def acos(u: inVec3d) :Vec3d = new Vec3d(acos(u.x), acos(u.y), acos(u.z))
  def atan(uy: inVec3d, ux: inVec3d) :Vec3d = {
    new Vec3d(atan(uy.x, ux.x), atan(uy.y, ux.y), atan(uy.z, ux.z))
  }
  def atan(u: inVec3d) :Vec3d = new Vec3d(atan(u.x), atan(u.y), atan(u.z))

  def sinh(u: inVec3d) :Vec3d = new Vec3d(sinh(u.x), sinh(u.y), sinh(u.z))
  def cosh(u: inVec3d) :Vec3d = new Vec3d(cosh(u.x), cosh(u.y), cosh(u.z))
  def tanh(u: inVec3d) :Vec3d = new Vec3d(tanh(u.x), tanh(u.y), tanh(u.z))

  def asinh(u: inVec3d) :Vec3d = {
    new Vec3d(asinh(u.x), asinh(u.y), asinh(u.z))
  }
  def acosh(u: inVec3d) :Vec3d = {
    new Vec3d(acosh(u.x), acosh(u.y), acosh(u.z))
  }
  def atanh(u: inVec3d) :Vec3d = {
    new Vec3d(atanh(u.x), atanh(u.y), atanh(u.z))
  }

  def pow(u: inVec3d, v: inVec3d) :Vec3d = {
    new Vec3d(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z))
  }
  def exp(u: inVec3d) :Vec3d = new Vec3d(exp(u.x), exp(u.y), exp(u.z))
  def log(u: inVec3d) :Vec3d = new Vec3d(log(u.x), log(u.y), log(u.z))

  def exp2(u: inVec3d) :Vec3d = new Vec3d(exp2(u.x), exp2(u.y), exp2(u.z))
  def log2(u: inVec3d) :Vec3d = new Vec3d(log2(u.x), log2(u.y), log2(u.z))

  def sqrt(u: inVec3d) :Vec3d = new Vec3d(sqrt(u.x), sqrt(u.y), sqrt(u.z))
  def inversesqrt(u: inVec3d) :Vec3d = {
    new Vec3d(inversesqrt(u.x), inversesqrt(u.y), inversesqrt(u.z))
  }

  def abs(u: inVec3d) :Vec3d = new Vec3d(abs(u.x), abs(u.y), abs(u.z))
  def sign(u: inVec3d) :Vec3d = new Vec3d(sign(u.x), sign(u.y), sign(u.z))
  def floor(u: inVec3d) :Vec3d = {
    new Vec3d(floor(u.x), floor(u.y), floor(u.z))
  }
  def trunc(u: inVec3d) :Vec3d = {
    new Vec3d(trunc(u.x), trunc(u.y), trunc(u.z))
  }
  def round(u: inVec3d) :Vec3d = {
    new Vec3d(round(u.x), round(u.y), round(u.z))
  }
  def roundEven(u: inVec3d) :Vec3d = {
    new Vec3d(roundEven(u.x), roundEven(u.y), roundEven(u.z))
  }
  def ceil(u: inVec3d) :Vec3d = new Vec3d(ceil(u.x), ceil(u.y), ceil(u.z))
  def fract(u: inVec3d) :Vec3d = {
    new Vec3d(fract(u.x), fract(u.y), fract(u.z))
  }
  def mod(u: inVec3d, s: Double) :Vec3d = {
    new Vec3d(mod(u.x, s), mod(u.y, s), mod(u.z, s))
  }
  def mod(u: inVec3d, v: inVec3d) :Vec3d = {
    new Vec3d(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z))
  }
  def modf(u: inVec3d, i: outVec3d) :Vec3d = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    i.z = trunc(u.z)
    u - i
  }

  def min(u: inVec3d, s: Double) :Vec3d = {
    new Vec3d(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  def min(u: inVec3d, v: inVec3d) :Vec3d = {
    new Vec3d(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  def max(u: inVec3d, s: Double) :Vec3d = {
    new Vec3d(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  def max(u: inVec3d, v: inVec3d) :Vec3d = {
    new Vec3d(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  def clamp(u: inVec3d, minVal: Double, maxVal: Double) :Vec3d = {
    new Vec3d(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  def clamp(u: inVec3d, minVal: inVec3d, maxVal: inVec3d) :Vec3d = {
    new Vec3d(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }

  def mix(u: inVec3d, v: inVec3d, a: Double) :Vec3d = {
    val b = 1 - a
    new Vec3d(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z)
  }
  def mix(u: inVec3d, v: inVec3d, a: inVec3d) :Vec3d = {
    new Vec3d(mix(u.x, v.x, a.x), mix(u.y, v.y, a.y), mix(u.z, v.z, a.z))
  }
  def mix(u: inVec3d, v: inVec3d, a: inVec3b) :Vec3d = {
    new Vec3d(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y,
      if (a.z) v.z else u.z
    )
  }

  def step(edge: Double, u: inVec3d) :Vec3d = {
    new Vec3d(step(edge, u.x), step(edge, u.y), step(edge, u.z))
  }
  def step(edge: inVec3d, u: inVec3d) :Vec3d = {
    new Vec3d(step(edge.x, u.x), step(edge.y, u.y), step(edge.z, u.z))
  }
  def smoothstep(edge0: Double, edge1: Double, u: inVec3d) :Vec3d = {
    new Vec3d(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y),
      smoothstep(edge0, edge1, u.z)
    )
  }
  def smoothstep(edge0: inVec3d, edge1: inVec3d, u: inVec3d) :Vec3d = {
    new Vec3d(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y),
      smoothstep(edge0.z, edge1.z, u.z)
    )
  }

  def isnan(u: inVec3d) :Vec3b = {
    new Vec3b(isnan(u.x), isnan(u.y), isnan(u.z))
  }
  def isinf(u: inVec3d) :Vec3b = {
    new Vec3b(isinf(u.x), isinf(u.y), isinf(u.z))
  }

  def length(u: inVec3d) :Double = sqrt(u.x*u.x + u.y*u.y + u.z*u.z)
  def distance(u: inVec3d, v: inVec3d) :Double = {
    val x = u.x - v.x
    val y = u.y - v.y
    val z = u.z - v.z
    sqrt(x*x + y*y + z*z)
  }
  def dot(u: inVec3d, v: inVec3d) :Double = u.x*v.x + u.y*v.y + u.z*v.z
  def cross(u: inVec3d, v: inVec3d) :Vec3d = {
    new Vec3d(
      u.y*v.z-v.y*u.z,
      u.z*v.x-v.z*u.x,
      u.x*v.y-v.x*u.y
    )
  }
  def normalize(u: inVec3d) :Vec3d = {
    u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z)
  }

  def faceforward(n: inVec3d, i: inVec3d, nref: inVec3d) :Vec3d = {
    if (dot(nref, i) < 0) Vec3d(n) else -n
  }

  def reflect(i: inVec3d, n: inVec3d) :Vec3d = {
    val t = -2*dot(n, i)
    new Vec3d(
      i.x + n.x*t,
      i.y + n.y*t,
      i.z + n.z*t
    )
  }
  def refract(i: inVec3d, n: inVec3d, eta: Double) :Vec3d = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec3d(0, 0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec3d(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t,
        i.z*eta - n.z*t
      )
    }
  }

  def lessThan(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  def lessThanEqual(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  def greaterThan(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  def greaterThanEqual(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  def equal(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  def notEqual(u: inVec3d, v: inVec3d) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  def noise1(u: inVec3d) :Double = {
    noise(u.x, u.y, u.z)
  }
  def noise2(u: inVec3d) :Vec2d = {
    new Vec2d(
      noise(u.x, u.y, u.z),
      noise(u.x + offset1, u.y + offset1, u.z + offset1)
    )
  }
  def noise3(u: inVec3d) :Vec3d = {
    new Vec3d(
      noise(u.x, u.y, u.z),
      noise(u.x + offset1, u.y + offset1, u.z + offset1),
      noise(u.x + offset2, u.y + offset2, u.z + offset2)
    )
  }
  def noise4(u: inVec3d) :Vec4d = {
    new Vec4d(
      noise(u.x, u.y, u.z),
      noise(u.x + offset1, u.y + offset1, u.z + offset1),
      noise(u.x + offset2, u.y + offset2, u.z + offset2),
      noise(u.x + offset3, u.y + offset3, u.z + offset3)
    )
  }

  // Vec4d functions
  def radians(u: inVec4d) :Vec4d = {
    new Vec4d(radians(u.x), radians(u.y), radians(u.z), radians(u.w))
  }
  def degrees(u: inVec4d) :Vec4d = {
    new Vec4d(degrees(u.x), degrees(u.y), degrees(u.z), degrees(u.w))
  }

  def sin(u: inVec4d) :Vec4d = {
    new Vec4d(sin(u.x), sin(u.y), sin(u.z), sin(u.w))
  }
  def cos(u: inVec4d) :Vec4d = {
    new Vec4d(cos(u.x), cos(u.y), cos(u.z), cos(u.w))
  }
  def tan(u: inVec4d) :Vec4d = {
    new Vec4d(tan(u.x), tan(u.y), tan(u.z), tan(u.w))
  }

  def asin(u: inVec4d) :Vec4d = {
    new Vec4d(asin(u.x), asin(u.y), asin(u.z), asin(u.w))
  }
  def acos(u: inVec4d) :Vec4d = {
    new Vec4d(acos(u.x), acos(u.y), acos(u.z), acos(u.w))
  }
  def atan(uy: inVec4d, ux: inVec4d) :Vec4d = {
    new Vec4d(
      atan(uy.x, ux.x),
      atan(uy.y, ux.y),
      atan(uy.z, ux.z),
      atan(uy.w, ux.w)
    )
  }
  def atan(u: inVec4d) :Vec4d = {
    new Vec4d(atan(u.x), atan(u.y), atan(u.z), atan(u.w))
  }

  def sinh(u: inVec4d) :Vec4d = {
    new Vec4d(sinh(u.x), sinh(u.y), sinh(u.z), sinh(u.w))
  }
  def cosh(u: inVec4d) :Vec4d = {
    new Vec4d(cosh(u.x), cosh(u.y), cosh(u.z), cosh(u.w))
  }
  def tanh(u: inVec4d) :Vec4d = {
    new Vec4d(tanh(u.x), tanh(u.y), tanh(u.z), tanh(u.w))
  }

  def asinh(u: inVec4d) :Vec4d = {
    new Vec4d(asinh(u.x), asinh(u.y), asinh(u.z), asinh(u.w))
  }
  def acosh(u: inVec4d) :Vec4d = {
    new Vec4d(acosh(u.x), acosh(u.y), acosh(u.z), acosh(u.w))
  }
  def atanh(u: inVec4d) :Vec4d = {
    new Vec4d(atanh(u.x), atanh(u.y), atanh(u.z), atanh(u.w))
  }

  def pow(u: inVec4d, v: inVec4d) :Vec4d = {
    new Vec4d(pow(u.x, v.x), pow(u.y, v.y), pow(u.z, v.z), pow(u.w, v.w))
  }
  def exp(u: inVec4d) :Vec4d = {
    new Vec4d(exp(u.x), exp(u.y), exp(u.z), exp(u.w))
  }
  def log(u: inVec4d) :Vec4d = {
    new Vec4d(log(u.x), log(u.y), log(u.z), log(u.w))
  }

  def exp2(u: inVec4d) :Vec4d = {
    new Vec4d(exp2(u.x), exp2(u.y), exp2(u.z), exp2(u.w))
  }
  def log2(u: inVec4d) :Vec4d = {
    new Vec4d(log2(u.x), log2(u.y), log2(u.z), log2(u.w))
  }

  def sqrt(u: inVec4d) :Vec4d = {
    new Vec4d(sqrt(u.x), sqrt(u.y), sqrt(u.z), sqrt(u.w))
  }
  def inversesqrt(u: inVec4d) :Vec4d = {
    new Vec4d(
      inversesqrt(u.x),
      inversesqrt(u.y),
      inversesqrt(u.z),
      inversesqrt(u.w)
    )
  }

  def abs(u: inVec4d) :Vec4d = {
    new Vec4d(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  def sign(u: inVec4d) :Vec4d = {
    new Vec4d(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  def floor(u: inVec4d) :Vec4d = {
    new Vec4d(floor(u.x), floor(u.y), floor(u.z), floor(u.w))
  }
  def trunc(u: inVec4d) :Vec4d = {
    new Vec4d(trunc(u.x), trunc(u.y), trunc(u.z), trunc(u.w))
  }
  def round(u: inVec4d) :Vec4d = {
    new Vec4d(round(u.x), round(u.y), round(u.z), round(u.w))
  }
  def roundEven(u: inVec4d) :Vec4d = {
    new Vec4d(
      roundEven(u.x), roundEven(u.y),
      roundEven(u.z), roundEven(u.w)
    )
  }
  def ceil(u: inVec4d) :Vec4d = {
    new Vec4d(ceil(u.x), ceil(u.y), ceil(u.z), ceil(u.w))
  }
  def fract(u: inVec4d) :Vec4d = {
    new Vec4d(fract(u.x), fract(u.y), fract(u.z), fract(u.w))
  }
  def mod(u: inVec4d, s: Double) :Vec4d = {
    new Vec4d(mod(u.x, s), mod(u.y, s), mod(u.z, s), mod(u.w, s))
  }
  def mod(u: inVec4d, v: inVec4d) :Vec4d = {
    new Vec4d(mod(u.x, v.x), mod(u.y, v.y), mod(u.z, v.z), mod(u.w, v.w))
  }
  def modf(u: inVec4d, i: outVec4d) :Vec4d = {
    i.x = trunc(u.x)
    i.y = trunc(u.y)
    i.z = trunc(u.z)
    i.w = trunc(u.w)
    u - i
  }

  def min(u: inVec4d, s: Double) :Vec4d = {
    new Vec4d(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  def min(u: inVec4d, v: inVec4d) :Vec4d = {
    new Vec4d(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  def max(u: inVec4d, s: Double) :Vec4d = {
    new Vec4d(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  def max(u: inVec4d, v: inVec4d) :Vec4d = {
    new Vec4d(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  def clamp(u: inVec4d, minVal: Double, maxVal: Double) :Vec4d = {
    new Vec4d(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  def clamp(u: inVec4d, minVal: inVec4d, maxVal: inVec4d) :Vec4d = {
    new Vec4d(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }

  def mix(u: inVec4d, v: inVec4d, a: Double) :Vec4d = {
    val b = 1 - a
    new Vec4d(b*u.x + a*v.x, b*u.y + a*v.y, b*u.z + a*v.z, b*u.w + a*v.w)
  }
  def mix(u: inVec4d, v: inVec4d, a: inVec4d) :Vec4d = {
    new Vec4d(
      mix(u.x, v.x, a.x),
      mix(u.y, v.y, a.y),
      mix(u.z, v.z, a.z),
      mix(u.w, v.w, a.w)
    )
  }
  def mix(u: inVec4d, v: inVec4d, a: inVec4b) :Vec4d = {
    new Vec4d(
      if (a.x) v.x else u.x,
      if (a.y) v.y else u.y,
      if (a.z) v.z else u.z,
      if (a.w) v.w else u.w
    )
  }

  def step(edge: Double, u: inVec4d) :Vec4d = {
    new Vec4d(step(edge, u.x), step(edge, u.y),
        step(edge, u.z), step(edge, u.w))
  }
  def step(edge: inVec4d, u: inVec4d) :Vec4d = {
    new Vec4d(
      step(edge.x, u.x),
      step(edge.y, u.y),
      step(edge.z, u.z),
      step(edge.w, u.w)
    )
  }
  def smoothstep(edge0: Double, edge1: Double, u: inVec4d) :Vec4d = {
    new Vec4d(
      smoothstep(edge0, edge1, u.x),
      smoothstep(edge0, edge1, u.y),
      smoothstep(edge0, edge1, u.z),
      smoothstep(edge0, edge1, u.w)
    )
  }
  def smoothstep(edge0: inVec4d, edge1: inVec4d, u: inVec4d) :Vec4d = {
    new Vec4d(
      smoothstep(edge0.x, edge1.x, u.x),
      smoothstep(edge0.y, edge1.y, u.y),
      smoothstep(edge0.z, edge1.z, u.z),
      smoothstep(edge0.w, edge1.w, u.w)
    )
  }

  def isnan(u: inVec4d) :Vec4b = {
    new Vec4b(isnan(u.x), isnan(u.y), isnan(u.z), isnan(u.w))
  }
  def isinf(u: inVec4d) :Vec4b = {
    new Vec4b(isinf(u.x), isinf(u.y), isinf(u.z), isinf(u.w))
  }

  def length(u: inVec4d) :Double = sqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
  def distance(u: inVec4d, v: inVec4d) :Double = {
    val x = u.x - v.x
    val y = u.y - v.y
    val z = u.z - v.z
    val w = u.w - v.w
    sqrt(x*x + y*y + z*z + w*w)
  }
  def dot(u: inVec4d, v: inVec4d) :Double = {
    u.x*v.x + u.y*v.y + u.z*v.z + u.w*v.w
  }
  def normalize(u: inVec4d) :Vec4d = {
    u*inversesqrt(u.x*u.x + u.y*u.y + u.z*u.z + u.w*u.w)
  }

  def faceforward(n: inVec4d, i: inVec4d, nref: inVec4d) :Vec4d = {
    if (dot(nref, i) < 0) Vec4d(n) else -n
  }

  def reflect(i: inVec4d, n: inVec4d) :Vec4d = {
    val t = -2*dot(n, i)
    new Vec4d(
      i.x + n.x*t,
      i.y + n.y*t,
      i.z + n.z*t,
      i.w + n.w*t
    )
  }
  def refract(i: inVec4d, n: inVec4d, eta: Double) :Vec4d = {
    val dotni = dot(n, i)
    val k = 1 - eta*eta*(1 - dotni*dotni)
    if (k < 0) {
      new Vec4d(0, 0, 0, 0)
    }
    else {
      val t = eta*dotni + sqrt(k)
      new Vec4d(
        i.x*eta - n.x*t,
        i.y*eta - n.y*t,
        i.z*eta - n.z*t,
        i.w*eta - n.w*t
      )
    }
  }

  def lessThan(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  def lessThanEqual(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  def greaterThan(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  def greaterThanEqual(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  def equal(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  def notEqual(u: inVec4d, v: inVec4d) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }

  def noise1(u: inVec4d) :Double = {
    noise(u.x, u.y, u.z, u.w)
  }
  def noise2(u: inVec4d) :Vec2d = {
    new Vec2d(
      noise(u.x, u.y, u.z, u.w),
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1)
    )
  }
  def noise3(u: inVec4d) :Vec3d = {
    new Vec3d(
      noise(u.x, u.y, u.z, u.w),
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1),
      noise(u.x + offset2, u.y + offset2, u.z + offset2, u.w + offset2)
    )
  }
  def noise4(u: inVec4d) :Vec4d = {
    new Vec4d(
      noise(u.x, u.y, u.z, u.w),
      noise(u.x + offset1, u.y + offset1, u.z + offset1, u.w + offset1),
      noise(u.x + offset2, u.y + offset2, u.z + offset2, u.w + offset2),
      noise(u.x + offset3, u.y + offset3, u.z + offset3, u.w + offset3)
    )
  }

  // Mat functions
  def matrixCompMult(a: inMat2d, b: inMat2d) :Mat2d = {
    new Mat2d(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11
    )
  }
  def matrixCompMult(a: inMat2x3d, b: inMat2x3d) :Mat2x3d = {
    new Mat2x3d(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11,
      a.m02*b.m02, a.m12*b.m12
    )
  }
  def matrixCompMult(a: inMat2x4d, b: inMat2x4d) :Mat2x4d = {
    new Mat2x4d(
      a.m00*b.m00, a.m10*b.m10,
      a.m01*b.m01, a.m11*b.m11,
      a.m02*b.m02, a.m12*b.m12,
      a.m03*b.m03, a.m13*b.m13
    )
  }
  def matrixCompMult(a: inMat3x2d, b: inMat3x2d) :Mat3x2d = {
    new Mat3x2d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21
    )
  }
  def matrixCompMult(a: inMat3d, b: inMat3d) :Mat3d = {
    new Mat3d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22
    )
  }
  def matrixCompMult(a: inMat3x4d, b: inMat3x4d) :Mat3x4d = {
    new Mat3x4d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22,
      a.m03*b.m03, a.m13*b.m13, a.m23*b.m23
    )
  }
  def matrixCompMult(a: inMat4x2d, b: inMat4x2d) :Mat4x2d = {
    new Mat4x2d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31
    )
  }
  def matrixCompMult(a: inMat4x3d, b: inMat4x3d) :Mat4x3d = {
    new Mat4x3d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32
    )
  }
  def matrixCompMult(a: inMat4d, b: inMat4d) :Mat4d = {
    new Mat4d(
      a.m00*b.m00, a.m10*b.m10, a.m20*b.m20, a.m30*b.m30,
      a.m01*b.m01, a.m11*b.m11, a.m21*b.m21, a.m31*b.m31,
      a.m02*b.m02, a.m12*b.m12, a.m22*b.m22, a.m32*b.m32,
      a.m03*b.m03, a.m13*b.m13, a.m23*b.m23, a.m33*b.m33
    )
  }

  def outerProduct(c: inVec2d, r: inVec2d) :Mat2d = {
    new Mat2d(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y
    )
  }
  def outerProduct(c: inVec2d, r: inVec3d) :Mat2x3d = {
    new Mat2x3d(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y,
      c.x*r.z, c.y*r.z
    )
  }
  def outerProduct(c: inVec2d, r: inVec4d) :Mat2x4d = {
    new Mat2x4d(
      c.x*r.x, c.y*r.x,
      c.x*r.y, c.y*r.y,
      c.x*r.z, c.y*r.z,
      c.x*r.w, c.y*r.w
    )
  }
  def outerProduct(c: inVec3d, r: inVec2d) :Mat3x2d = {
    new Mat3x2d(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y
    )
  }
  def outerProduct(c: inVec3d, r: inVec3d) :Mat3d = {
    new Mat3d(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z
    )
  }
  def outerProduct(c: inVec3d, r: inVec4d) :Mat3x4d = {
    new Mat3x4d(
      c.x*r.x, c.y*r.x, c.z*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z,
      c.x*r.w, c.y*r.w, c.z*r.w
    )
  }
  def outerProduct(c: inVec4d, r: inVec2d) :Mat4x2d = {
    new Mat4x2d(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y
    )
  }
  def outerProduct(c: inVec4d, r: inVec3d) :Mat4x3d = {
    new Mat4x3d(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z
    )
  }
  def outerProduct(c: inVec4d, r: inVec4d) :Mat4d = {
    new Mat4d(
      c.x*r.x, c.y*r.x, c.z*r.x, c.w*r.x,
      c.x*r.y, c.y*r.y, c.z*r.y, c.w*r.y,
      c.x*r.z, c.y*r.z, c.z*r.z, c.w*r.z,
      c.x*r.w, c.y*r.w, c.z*r.w, c.w*r.w
    )
  }

  def transpose(a: inMat2d) :Mat2d = {
    new Mat2d(
      a.m00, a.m01,
      a.m10, a.m11
    )
  }
  def transpose(a: inMat3x2d) :Mat2x3d = {
    new Mat2x3d(
      a.m00, a.m01,
      a.m10, a.m11,
      a.m20, a.m21
    )
  }
  def transpose(a: inMat4x2d) :Mat2x4d = {
    new Mat2x4d(
      a.m00, a.m01,
      a.m10, a.m11,
      a.m20, a.m21,
      a.m30, a.m31
    )
  }
  def transpose(a: inMat2x3d) :Mat3x2d = {
    new Mat3x2d(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12
    )
  }
  def transpose(a: inMat3d) :Mat3d = {
    new Mat3d(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12,
      a.m20, a.m21, a.m22
    )
  }
  def transpose(a: inMat4x3d) :Mat3x4d = {
    new Mat3x4d(
      a.m00, a.m01, a.m02,
      a.m10, a.m11, a.m12,
      a.m20, a.m21, a.m22,
      a.m30, a.m31, a.m32
    )
  }
  def transpose(a: inMat2x4d) :Mat4x2d = {
    new Mat4x2d(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13
    )
  }
  def transpose(a: inMat3x4d) :Mat4x3d = {
    new Mat4x3d(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13,
      a.m20, a.m21, a.m22, a.m23
    )
  }
  def transpose(a: inMat4d) :Mat4d = {
    new Mat4d(
      a.m00, a.m01, a.m02, a.m03,
      a.m10, a.m11, a.m12, a.m13,
      a.m20, a.m21, a.m22, a.m23,
      a.m30, a.m31, a.m32, a.m33
    )
  }

  def determinant(m: inMat2d) :Double = m.m00*m.m11 - m.m01*m.m10

  def determinant(m: inMat3d) :Double = {
    import m._

    val c0 = m11*m22 - m12*m21
    val c1 = m12*m20 - m10*m22
    val c2 = m10*m21 - m11*m20

    m00*c0 + m01*c1 + m02*c2
  }

  def determinant(m: inMat4d) :Double = {
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
  def inverse(m: inMat2d) :Mat2d = {
    val invDet = 1/determinant(m)
    new Mat2d(
       m.m11*invDet, -m.m10*invDet,
      -m.m01*invDet,  m.m00*invDet
    )
  }

  /**
   * This is a general matrix inverse. You can invert transofrmations
   * quicker by using Transform3d.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat3d) :Mat3d = {
    import m._

    val c0 = m11*m22 - m12*m21
    val c1 = m12*m20 - m10*m22
    val c2 = m10*m21 - m11*m20

    val invDet = 1/(m00*c0 + m01*c1 + m02*c2)

    new Mat3d(
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
   * quicker by using Transform3d.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat4d) :Mat4d = {
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

    new Mat4d(
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

  def isnegzero(x: Double) :Boolean = (x == 0 && java.lang.Double.doubleToRawLongBits(x) != 0)
  def isposzero(x: Double) :Boolean = (x == 0 && java.lang.Double.doubleToRawLongBits(x) == 0)
  
  def isneginf(x: Double) :Boolean = isinf(x) && x < 0
  def isposinf(x: Double) :Boolean = isinf(x) && x > 0

  
  // Lerp
  def lerp(m: inMat2d, n: inMat2d, a: Double) :Mat2d = {
    val b = 1 - a

    new Mat2d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11
    )
  }
  def lerp(m: inMat2x3d, n: inMat2x3d, a: Double) :Mat2x3d = {
    val b = 1 - a

    new Mat2x3d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12
    )
  }
  def lerp(m: inMat2x4d, n: inMat2x4d, a: Double) :Mat2x4d = {
    val b = 1 - a

    new Mat2x4d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12,
      b*m.m03 + a*n.m03, b*m.m13 + a*n.m13
    )
  }
  def lerp(m: inMat3x2d, n: inMat3x2d, a: Double) :Mat3x2d = {
    val b = 1 - a

    new Mat3x2d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21
    )
  }
  def lerp(m: inMat3d, n: inMat3d, a: Double) :Mat3d = {
    val b = 1 - a

    new Mat3d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22
    )
  }
  def lerp(m: inMat3x4d, n: inMat3x4d, a: Double) :Mat3x4d = {
    val b = 1 - a

    new Mat3x4d(
      b*m.m00 + a*n.m00, b*m.m10 + a*n.m10, b*m.m20 + a*n.m20,
      b*m.m01 + a*n.m01, b*m.m11 + a*n.m11, b*m.m21 + a*n.m21,
      b*m.m02 + a*n.m02, b*m.m12 + a*n.m12, b*m.m22 + a*n.m22,
      b*m.m03 + a*n.m03, b*m.m13 + a*n.m13, b*m.m23 + a*n.m23
    )
  }
  def lerp(m: inMat4x2d, n: inMat4x2d, a: Double) :Mat4x2d = {
    val b = 1 - a

    new Mat4x2d(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31
    )
  }
  def lerp(m: inMat4x3d, n: inMat4x3d, a: Double) :Mat4x3d = {
    val b = 1 - a

    new Mat4x3d(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
      b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32
    )
  }
  def lerp(m: inMat4d, n: inMat4d, a: Double) :Mat4d = {
    val b = 1 - a

    new Mat4d(
      b*m.m00 +a*n.m00, b*m.m10 +a*n.m10, b*m.m20 +a*n.m20, b*m.m30 +a*n.m30,
      b*m.m01 +a*n.m01, b*m.m11 +a*n.m11, b*m.m21 +a*n.m21, b*m.m31 +a*n.m31,
      b*m.m02 +a*n.m02, b*m.m12 +a*n.m12, b*m.m22 +a*n.m22, b*m.m32 +a*n.m32,
      b*m.m03 +a*n.m03, b*m.m13 +a*n.m13, b*m.m23 +a*n.m23, b*m.m33 +a*n.m33
    )
  }

  def hasErrors(x: Double) :Boolean = isinf(x) || isnan(x)
  def hasErrors(u: inVec2d) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y)
  )
  def hasErrors(u: inVec3d) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y) || hasErrors(u.z)
  )
  def hasErrors(u: inVec4d) :Boolean = (
    hasErrors(u.x) || hasErrors(u.y) || hasErrors(u.z) || hasErrors(u.w)
  )
  def hasErrors(q: inQuat4d) :Boolean = (
    hasErrors(q.a) || hasErrors(q.b) || hasErrors(q.c) || hasErrors(q.d)
  )
  def hasErrors(m: inMat2d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)
  )
  def hasErrors(m: inMat2x3d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)||
    hasErrors(m.m02)|| hasErrors(m.m12)
  )
  def hasErrors(m: inMat2x4d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)||
    hasErrors(m.m01)|| hasErrors(m.m11)||
    hasErrors(m.m02)|| hasErrors(m.m12)||
    hasErrors(m.m03)|| hasErrors(m.m13)
  )
  def hasErrors(m: inMat3x2d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)
  )
  def hasErrors(m: inMat3d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)
  )
  def hasErrors(m: inMat3x4d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)||
    hasErrors(m.m03)|| hasErrors(m.m13)|| hasErrors(m.m23)
  )
  def hasErrors(m: inMat4x2d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)
  )
  def hasErrors(m: inMat4x3d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)|| hasErrors(m.m32)
  )
  def hasErrors(m: inMat4d) :Boolean = (
    hasErrors(m.m00)|| hasErrors(m.m10)|| hasErrors(m.m20)|| hasErrors(m.m30)||
    hasErrors(m.m01)|| hasErrors(m.m11)|| hasErrors(m.m21)|| hasErrors(m.m31)||
    hasErrors(m.m02)|| hasErrors(m.m12)|| hasErrors(m.m22)|| hasErrors(m.m32)||
    hasErrors(m.m03)|| hasErrors(m.m13)|| hasErrors(m.m23)|| hasErrors(m.m33)
  )

  def approxEqual(x: Double, y: Double, absDelta: Double) :Boolean = {
    abs(x - y) < absDelta
  }
  def approxEqual(u: inVec2d, v: inVec2d, absDelta: Double) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta
  }
  def approxEqual(u: inVec3d, v: inVec3d, absDelta: Double) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta &&
    abs(v.z - u.z) < absDelta
  }
  def approxEqual(u: inVec4d, v: inVec4d, absDelta: Double) :Boolean = {
    abs(v.x - u.x) < absDelta &&
    abs(v.y - u.y) < absDelta &&
    abs(v.z - u.z) < absDelta &&
    abs(v.w - u.w) < absDelta
  }
  def approxEqual(p: inQuat4d, q: inQuat4d, absDelta: Double) :Boolean = {
    abs(p.a - q.a) < absDelta &&
    abs(p.b - q.b) < absDelta &&
    abs(p.c - q.c) < absDelta &&
    abs(p.d - q.d) < absDelta
  }
  def approxEqual(m: inMat2d, n: inMat2d, absDelta: Double) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta
    )
  }
  def approxEqual(m: inMat2x3d, n: inMat2x3d, absDelta: Double) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&

      abs(n.m02 - m.m02) < absDelta &&
      abs(n.m12 - m.m12) < absDelta
    )
  }
  def approxEqual(m: inMat2x4d, n: inMat2x4d, absDelta: Double) :Boolean = {
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

  def approxEqual(m: inMat3x2d, n: inMat3x2d, absDelta: Double) :Boolean = {
    (
      abs(n.m00 - m.m00) < absDelta &&
      abs(n.m10 - m.m10) < absDelta &&
      abs(n.m20 - m.m20) < absDelta &&

      abs(n.m01 - m.m01) < absDelta &&
      abs(n.m11 - m.m11) < absDelta &&
      abs(n.m21 - m.m21) < absDelta
    )
  }
  def approxEqual(m: inMat3d, n: inMat3d, absDelta: Double) :Boolean = {
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
  def approxEqual(m: inMat3x4d, n: inMat3x4d, absDelta: Double) :Boolean = {
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
  def approxEqual(m: inMat4x2d, n: inMat4x2d, absDelta: Double) :Boolean = {
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
  def approxEqual(m: inMat4x3d, n: inMat4x3d, absDelta: Double) :Boolean = {
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
  def approxEqual(m: inMat4d, n: inMat4d, absDelta: Double) :Boolean = {
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
   * quicker by using Transform2d.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat2x3d) :Mat2x3d = {
    import m._

    val invDet = 1/(m00*m11 - m01*m10)

    new Mat2x3d(
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
   * quicker by using Transform3d.inverse(scale, rotation, translation).
   * A rotation matrix that does not scale can be inverted even faster by
   * using transpose. In the latter case you can avoid inverse alltogether
   * by using transpose multiplication:
   * instead of multiplying a matrix by a vectors (M*v),
   * you can multiply the vector by the matrix (v*M).
   *
   * <br/>If matrix determinant is zero the result is undefined.
   */
  def inverse(m: inMat3x4d) :Mat3x4d = {
    import m._

    val a0 = m00*m11 - m01*m10
    val a1 = m00*m12 - m02*m10
    val a2 = m00*m13 - m03*m10
    val a3 = m01*m12 - m02*m11
    val a4 = m01*m13 - m03*m11
    val a5 = m02*m13 - m03*m12

    val invDet = 1/(a0*m22 - a1*m21 + a3*m20)

    new Mat3x4d(
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
  def normSquare(q: inQuat4d) :Double = q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d
  def norm(q: inQuat4d) :Double = {
    sqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
  }
  def conjugate(q: inQuat4d) :Quat4d = new Quat4d(q.a, -q.b, -q.c, -q.d)

  def normalize(q: inQuat4d) :Quat4d = {
    q*inversesqrt(q.a*q.a + q.b*q.b + q.c*q.c + q.d*q.d)
  }
  
  /**
   * This method is here for completness. Normally you should work with
   * unit quaternions (<code>norm(q) == 1</code>), and in this case 
   * <code>inverse(q) == conjugate(q)</code>.
   */
  def inverse(q: inQuat4d) :Quat4d = conjugate(q)/normSquare(q)

  def slerp(p: inQuat4d, q: inQuat4d, a: Double) :Quat4d = {
    if (approxEqual(p, q, 1e-14)) return Quat4d(q)

    var cosTheta = p.a*q.a + p.b*q.b + p.c*q.c+ p.d*q.d
    var negate = false
    if (cosTheta < 0) {
      cosTheta = -cosTheta
      negate = true
    }

    var t = a
    var s = 1 - t

    if (cosTheta < 0.99) {
      val theta = acos(cosTheta)
      val invSinTheta = 1/sin(theta)

      t = sin(t*theta)*invSinTheta
      s = sin(s*theta)*invSinTheta
      if (negate) t = -t
    }

    new Quat4d(
      s*p.a + t*q.a,
      s*p.b + t*q.b,
      s*p.c + t*q.c,
      s*p.d + t*q.d
    )
  }

  /**
   * The quaternion must have unit norm to achieve the desired result.
   */
  def rotateVector(u: inVec3d, q: inQuat4d) = {
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

    new Vec3d(
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
  def rotationMat(angle: Double) :Mat2d = {
    val cosA = cos(angle)
    val sinA = sin(angle)

    new Mat2d(
       cosA, sinA,
      -sinA, cosA
    )
  }

  /**
   * The matrix must represent non-scaling rotation to achieve
   * the desired result.
   */
  def rotationAngle(m: inMat2d) :Double = {
    acos((m.m00 + m.m11)*0.5)
  }

  /**
   * The matrix must represent non-scaling rotation to achieve
   * the desired result.
   */
  def quaternion(m: inMat3d) :Quat4d = {
    import m._

    val trace = m00 + m11 + m22

    if (trace > 0) {
      val t = trace + 1
      val s = inversesqrt(t)*0.5
      new Quat4d(
        s*t,
        (m21 - m12)*s,
        (m02 - m20)*s,
        (m10 - m01)*s
      )
    }
    else if (m00 > m11 && m00 > m22) {
      val t = m00 - m11 - m22 + 1
      val s = inversesqrt(t)*0.5
      new Quat4d(
        (m21 - m12)*s,
        s*t,
        (m10 + m01)*s,
        (m02 + m20)*s
      )
    }
    else if (m11 > m22) {
      val t = -m00 + m11 - m22 + 1
      val s = inversesqrt(t)*0.5
      new Quat4d(
        (m02 - m20)*s,
        (m10 + m01)*s,
        s*t,
        (m21 + m12)*s
      )
    }
    else {
      val t = -m00 - m11 + m22 + 1
      val s = inversesqrt(t)*0.5
      new Quat4d(
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
  def quaternion(angle: Double, axis: inVec3d) :Quat4d = {
    val halfAngle = angle*0.5
    val s = sin(halfAngle)
    new Quat4d(cos(halfAngle), s*axis.x, s*axis.y, s*axis.z)
  }

  /**
   * The quaternion must have unit norm to achieve the desired result.
   */
  def rotationMat(q: inQuat4d) :Mat3d = {
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

    new Mat3d(
      tc - td, bc + da, bd - ca,
      bc - da, 1 - tb - td, cd + ba,
      bd + ca, cd - ba, tc - tb
    )
  }
  /**
   * The axis must have unit length to achieve the desired result.
   */
  def rotationMat(angle: Double, axis: inVec3d) :Mat3d = {
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

    new Mat3d(
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
  def angleAxis(q: inQuat4d, axis: outVec3d) :Double = {
    import q._

    if (approxEqual(abs(a), 1, 1e-15)) {
      axis := Vec3d.UnitX
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
  def angleAxis(m: inMat3d, axis: outVec3d) :Double = {
    import m._

    val cosAngle = (m00 + m11 + m22 - 1)*0.5

    if (approxEqual(cosAngle, 1, 1e-14)) {
      axis := Vec3d.UnitX
      return 0
    }
    else if (approxEqual(cosAngle, -1, 1e-14)) {
      if (m00 > m11 && m00 > m22) {
        val r = sqrt((m00 + 1)*0.5)
        val t = 1/(4*r)
        axis.x = r
        axis.y = (m01 + m10)*t
        axis.z = (m02 + m20)*t
      }
      else if (m11 > m22) {
        val r = sqrt((m11 + 1)*0.5)
        val t = 1/(4*r)
        axis.y = r
        axis.x = (m01 + m10)*t
        axis.z = (m12 + m21)*t
      }
      else {
        val r = sqrt((m22 + 1)*0.5)
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

  def lookAt(direction: inVec3d, up: inVec3d) :Mat3d = {
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
    new Mat3d(
      xax, xay, xaz,
      yax, yay, yaz,
      zax, zay, zaz
    )
  }

  // Projection
  /**
   * @param fieldOfView field of view angle in y direction, in radians.
   * @param aspectRatio width/height aspect ratio.
   * @param near the distance to the near clipping plane, must be positive,
   *   approximately log2(far/near) bits of depth buffer precision are lost.
   * @param far the distance to the far clipping plane, must be positive.
   */
  def perspectiveProj(
    fieldOfView: Double, aspectRatio: Double,
    near: Double, far: Double
  ) :Mat4d = {
    val focus = 1/tan(fieldOfView * 0.5)
    val inv_nf = 1/(near - far)

    new Mat4d(
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
    left: Double, right: Double,
    bottom: Double, top: Double,
    near: Double, far: Double
  ) :Mat4d = {
    val near2 = near*2
    val inv_rl = 1/(right - left)
    val inv_tb = 1/(top - bottom)
    val inv_nf = 1/(near - far)

    new Mat4d(
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
    left: Double, right: Double,
    bottom: Double, top: Double,
    near: Double, far: Double
  ) :Mat4d = {
    val r_l = 1/(right - left);
    val t_b = 1/(top - bottom);
    val f_n = 1/(far - near);

    new Mat4d(
      2*r_l, 0, 0, 0,
      0, 2*t_b, 0, 0,
      0, 0, -2*f_n, 0,
      -(right + left)*r_l, -(top + bottom)*t_b, -(far + near)*f_n, 1
    )
  }

  // Transformation
  def transformation(
    scale: inVec2d,
    rotation: inMat2d,
    translation: inVec2d
  ) :Mat2x3d = {
    import rotation._
    import translation.{x => tx, y => ty}
    import scale.{x => sx, y => sy}

    new Mat2x3d(
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
    scale: inVec2d,
    rotation: inMat2d,
    translation: inVec2d
  ) :Mat2x3d = {
    import translation.{x => tx, y => ty}

    val sx = 1/scale.x
    val sy = 1/scale.y

    val m00 = rotation.m00*sx
    val m10 = rotation.m01*sy
    val m01 = rotation.m10*sx
    val m11 = rotation.m11*sy

    new Mat2x3d(
      m00, m10,
      m01, m11,
      -m00*tx - m01*ty,
      -m10*tx - m11*ty
    )
  }

  def transformation(
    scale: inVec3d,
    rotation: inMat3d,
    translation: inVec3d
  ) :Mat3x4d = {
    import scale.{x => sx, y => sy, z => sz}
    import rotation._
    import translation.{x => tx, y => ty, z => tz}

    new Mat3x4d(
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
    scale: inVec3d,
    rotation: inMat3d,
    translation: inVec3d
  ) :Mat3x4d = {
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

    new Mat3x4d(
      m00, m10, m20,
      m01, m11, m21,
      m02, m12, m22,
      -m00*tx - m01*ty - m02*tz,
      -m10*tx - m11*ty - m12*tz,
      -m20*tx - m21*ty - m22*tz
    )
  }
}
