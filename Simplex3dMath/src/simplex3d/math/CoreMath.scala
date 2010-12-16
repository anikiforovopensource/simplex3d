/*
 ** Simplex3d, CoreMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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


private[math] class CoreMath


/** <code>CoreMath</code> contains functions to operate on Boolean and Int values and vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
object CoreMath extends CommonMath {
  // Int functions

  /** Computes an absolute value of the argument.
   * @param x, function argument.
   * @return an absolute value of x.
   */
  final def abs(x: Int) :Int = if (x < 0) -x else x

  /** Computes sign of the argument.
   * @param x, function argument.
   * @return 1 if x > 0, -1 if x < 0, 0 otherwise.
   */
  final def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0

  /** Finds the smallest value among the arguments.
   * @param x, function argument.
   * @param y, function argument.
   * @return smallest value from the set (x, y).
   */
  final def min(x: Int, y: Int) :Int = if (x < y) x else y

  /** Finds the largest value among the arguments.
   * @param x, function argument.
   * @param y, function argument.
   * @return largest value from the set (x, y).
   */
  final def max(x: Int, y: Int) :Int = if (x > y) x else y

  /** Clamps a value to a given range.
   * The result is undefined if minVal > maxVal.
   *
   * @param x, value to clamp.
   * @param minVal, range lower bound, inclusive.
   * @param maxVal, range upper bound, inclusive.
   * @return minVal if x < minVal, maxVal if x > maxVal, x otherwise.
   */
  final def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
    if (x <= minVal) minVal
    else if (x >= maxVal) maxVal
    else x
  }

  // Vec2i functions
  final def abs(u: inVec2i) :Vec2i = new Vec2i(abs(u.x), abs(u.y))
  final def sign(u: inVec2i) :Vec2i = new Vec2i(sign(u.x), sign(u.y))
  final def min(u: inVec2i, s: Int) :Vec2i = new Vec2i(min(u.x, s), min(u.y, s))
  final def min(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(min(u.x, v.x), min(u.y, v.y))
  }
  final def max(u: inVec2i, s: Int) :Vec2i = new Vec2i(max(u.x, s), max(u.y, s))
  final def max(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(max(u.x, v.x), max(u.y, v.y))
  }
  final def clamp(u: inVec2i, minVal: Int, maxVal: Int) :Vec2i = {
    new Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  final def clamp(u: inVec2i, minVal: inVec2i, maxVal: inVec2i) :Vec2i = {
    new Vec2i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }

  final def lessThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  final def lessThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  final def greaterThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  final def greaterThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  final def equal(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  final def notEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  // Vec3i functions
  final def abs(u: inVec3i) :Vec3i = new Vec3i(abs(u.x), abs(u.y), abs(u.z))
  final def sign(u: inVec3i) :Vec3i = new Vec3i(sign(u.x), sign(u.y), sign(u.z))
  final def min(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  final def min(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  final def max(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  final def max(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  final def clamp(u: inVec3i, minVal: Int, maxVal: Int) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  final def clamp(u: inVec3i, minVal: inVec3i, maxVal: inVec3i) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }

  final def lessThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  final def lessThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  final def greaterThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  final def greaterThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  final def equal(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  final def notEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  // Vec4i functions
  final def abs(u: inVec4i) :Vec4i = {
    new Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  final def sign(u: inVec4i) :Vec4i = {
    new Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  final def min(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  final def min(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  final def max(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  final def max(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  final def clamp(u: inVec4i, minVal: Int, maxVal: Int) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  final def clamp(u: inVec4i, minVal: inVec4i, maxVal: inVec4i) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }

  final def lessThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  final def lessThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  final def greaterThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  final def greaterThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  final def equal(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  final def notEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }
}
