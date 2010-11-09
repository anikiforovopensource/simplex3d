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


private[math] class CoreMath {

  // Boolean functions.
  
  // Vec2b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  def any(u: inVec2b) :Boolean = {
    u.x || u.y
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  def all(u: inVec2b) :Boolean = {
    u.x && u.y
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  def not(u: inVec2b) :Vec2b = Vec2b(!u.x, !u.y)


  // Vec3b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  def any(u: inVec3b) :Boolean = {
    u.x || u.y || u.z
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  def all(u: inVec3b) :Boolean = {
    u.x && u.y && u.z
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  def not(u: inVec3b) :Vec3b = Vec3b(!u.x, !u.y, !u.z)


  // Vec4b functions.

  /** This function is equivalent to logical OR on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if any of the components are true, false otherwise.
   */
  def any(u: inVec4b) :Boolean = {
    u.x || u.y || u.z || u.w
  }

  /** This function is equivalent to logical AND on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return true if all the components are true, false otherwise.
   */
  def all(u: inVec4b) :Boolean = {
    u.x && u.y && u.z && u.w
  }

  /** This function is equivalent to logical NOT on components
   * of the argument vector.
   *
   * @param u a boolean vector.
   * @return a boolean vector with negated components of u.
   */
  def not(u: inVec4b) :Vec4b = Vec4b(!u.x, !u.y, !u.z, !u.w)


  // Int functions

  /** Computes an absolute value of the argument.
   * @param x, function argument.
   * @return an absolute value of x.
   */
  def abs(x: Int) :Int = if (x < 0) -x else x

  /** Computes sign of the argument.
   * @param x, function argument.
   * @return 1 if x > 0, -1 if x < 0, 0 otherwise.
   */
  def sign(x: Int) :Int = if (x > 0) 1 else if (x < 0) -1 else 0

  /** Finds the smallest value among the arguments.
   * @param x, function argument.
   * @param y, function argument.
   * @return smallest value from the set (x, y).
   */
  def min(x: Int, y: Int) :Int = if (x < y) x else y

  /** Finds the largest value among the arguments.
   * @param x, function argument.
   * @param y, function argument.
   * @return largest value from the set (x, y).
   */
  def max(x: Int, y: Int) :Int = if (x > y) x else y

  /** Clamps a value to a given range.
   * The result is undefined if minVal > maxVal.
   *
   * @param x, value to clamp.
   * @param minVal, range lower bound, inclusive.
   * @param maxVal, range upper bound, inclusive.
   * @return minVal if x < minVal, maxVal if x > maxVal, x otherwise.
   */
  def clamp(x: Int, minVal: Int, maxVal: Int) :Int = {
    if (x <= minVal) minVal
    else if (x >= maxVal) maxVal
    else x
  }

  // Vec2i functions
  def abs(u: inVec2i) :Vec2i = new Vec2i(abs(u.x), abs(u.y))
  def sign(u: inVec2i) :Vec2i = new Vec2i(sign(u.x), sign(u.y))
  def min(u: inVec2i, s: Int) :Vec2i = new Vec2i(min(u.x, s), min(u.y, s))
  def min(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(min(u.x, v.x), min(u.y, v.y))
  }
  def max(u: inVec2i, s: Int) :Vec2i = new Vec2i(max(u.x, s), max(u.y, s))
  def max(u: inVec2i, v: inVec2i) :Vec2i = {
    new Vec2i(max(u.x, v.x), max(u.y, v.y))
  }
  def clamp(u: inVec2i, minVal: Int, maxVal: Int) :Vec2i = {
    new Vec2i(clamp(u.x, minVal, maxVal), clamp(u.y, minVal, maxVal))
  }
  def clamp(u: inVec2i, minVal: inVec2i, maxVal: inVec2i) :Vec2i = {
    new Vec2i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y)
    )
  }

  def lessThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x < v.x,
      u.y < v.y
    )
  }
  def lessThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x <= v.x,
      u.y <= v.y
    )
  }
  def greaterThan(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x > v.x,
      u.y > v.y
    )
  }
  def greaterThanEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x >= v.x,
      u.y >= v.y
    )
  }
  def equal(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x == v.x,
      u.y == v.y
    )
  }
  def notEqual(u: inVec2i, v: inVec2i) :Vec2b = {
    new Vec2b(
      u.x != v.x,
      u.y != v.y
    )
  }

  // Vec3i functions
  def abs(u: inVec3i) :Vec3i = new Vec3i(abs(u.x), abs(u.y), abs(u.z))
  def sign(u: inVec3i) :Vec3i = new Vec3i(sign(u.x), sign(u.y), sign(u.z))
  def min(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(min(u.x, s), min(u.y, s), min(u.z, s))
  }
  def min(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z))
  }
  def max(u: inVec3i, s: Int) :Vec3i = {
    new Vec3i(max(u.x, s), max(u.y, s), max(u.z, s))
  }
  def max(u: inVec3i, v: inVec3i) :Vec3i = {
    new Vec3i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z))
  }
  def clamp(u: inVec3i, minVal: Int, maxVal: Int) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal)
    )
  }
  def clamp(u: inVec3i, minVal: inVec3i, maxVal: inVec3i) :Vec3i = {
    new Vec3i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z)
    )
  }

  def lessThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z
    )
  }
  def lessThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z
    )
  }
  def greaterThan(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z
    )
  }
  def greaterThanEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z
    )
  }
  def equal(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z
    )
  }
  def notEqual(u: inVec3i, v: inVec3i) :Vec3b = {
    new Vec3b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z
    )
  }

  // Vec4i functions
  def abs(u: inVec4i) :Vec4i = {
    new Vec4i(abs(u.x), abs(u.y), abs(u.z), abs(u.w))
  }
  def sign(u: inVec4i) :Vec4i = {
    new Vec4i(sign(u.x), sign(u.y), sign(u.z), sign(u.w))
  }
  def min(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(min(u.x, s), min(u.y, s), min(u.z, s), min(u.w, s))
  }
  def min(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(min(u.x, v.x), min(u.y, v.y), min(u.z, v.z), min(u.w, v.w))
  }
  def max(u: inVec4i, s: Int) :Vec4i = {
    new Vec4i(max(u.x, s), max(u.y, s), max(u.z, s), max(u.w, s))
  }
  def max(u: inVec4i, v: inVec4i) :Vec4i = {
    new Vec4i(max(u.x, v.x), max(u.y, v.y), max(u.z, v.z), max(u.w, v.w))
  }
  def clamp(u: inVec4i, minVal: Int, maxVal: Int) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal, maxVal),
      clamp(u.y, minVal, maxVal),
      clamp(u.z, minVal, maxVal),
      clamp(u.w, minVal, maxVal)
    )
  }
  def clamp(u: inVec4i, minVal: inVec4i, maxVal: inVec4i) :Vec4i = {
    new Vec4i(
      clamp(u.x, minVal.x, maxVal.x),
      clamp(u.y, minVal.y, maxVal.y),
      clamp(u.z, minVal.z, maxVal.z),
      clamp(u.w, minVal.w, maxVal.w)
    )
  }

  def lessThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x < v.x,
      u.y < v.y,
      u.z < v.z,
      u.w < v.w
    )
  }
  def lessThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x <= v.x,
      u.y <= v.y,
      u.z <= v.z,
      u.w <= v.w
    )
  }
  def greaterThan(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x > v.x,
      u.y > v.y,
      u.z > v.z,
      u.w > v.w
    )
  }
  def greaterThanEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x >= v.x,
      u.y >= v.y,
      u.z >= v.z,
      u.w >= v.w
    )
  }
  def equal(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x == v.x,
      u.y == v.y,
      u.z == v.z,
      u.w == v.w
    )
  }
  def notEqual(u: inVec4i, v: inVec4i) :Vec4b = {
    new Vec4b(
      u.x != v.x,
      u.y != v.y,
      u.z != v.z,
      u.w != v.w
    )
  }


  // Generic matrix functions.

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   */
  def matrixToArray(m: AnyMat[_], array: Array[Float]) {
    matrixToArray(m, array, 0)
  }

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   * @param offset an offset into the array.
   */
  def matrixToArray(m: AnyMat[_], array: Array[Float], offset: Int) {
    array(offset + 0) = m.f00
    array(offset + 1) = m.f10
    array(offset + 2) = m.f20
    array(offset + 3) = m.f30

    array(offset + 4) = m.f01
    array(offset + 5) = m.f11
    array(offset + 6) = m.f21
    array(offset + 7) = m.f31

    array(offset + 8) = m.f02
    array(offset + 9) = m.f12
    array(offset + 10)= m.f22
    array(offset + 11)= m.f32

    array(offset + 12)= m.f03
    array(offset + 13)= m.f13
    array(offset + 14)= m.f23
    array(offset + 15)= m.f33
  }

  /** Writes a matrix into a given buffer in column major order.
   * Use buffer.position() to change buffer offset.
   *
   * @param m the source matrix.
   * @param buffer the destanation buffer.
   */
  def matrixToBuffer(m: AnyMat[_], buffer: FloatBuffer) {
    buffer.put(m.f00)
    buffer.put(m.f10)
    buffer.put(m.f20)
    buffer.put(m.f30)

    buffer.put(m.f01)
    buffer.put(m.f11)
    buffer.put(m.f21)
    buffer.put(m.f31)

    buffer.put(m.f02)
    buffer.put(m.f12)
    buffer.put(m.f22)
    buffer.put(m.f32)

    buffer.put(m.f03)
    buffer.put(m.f13)
    buffer.put(m.f23)
    buffer.put(m.f33)
  }

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   */
  def matrixToArray(m: AnyMat[_], array: Array[Double]) {
    matrixToArray(m, array, 0)
  }

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   * @param offset an offset into the array.
   */
  def matrixToArray(m: AnyMat[_], array: Array[Double], offset: Int) {
    array(offset + 0) = m.d00
    array(offset + 1) = m.d10
    array(offset + 2) = m.d20
    array(offset + 3) = m.d30

    array(offset + 4) = m.d01
    array(offset + 5) = m.d11
    array(offset + 6) = m.d21
    array(offset + 7) = m.d31

    array(offset + 8) = m.d02
    array(offset + 9) = m.d12
    array(offset + 10)= m.d22
    array(offset + 11)= m.d32

    array(offset + 12)= m.d03
    array(offset + 13)= m.d13
    array(offset + 14)= m.d23
    array(offset + 15)= m.d33
  }

  /** Writes a matrix into a given buffer in column major order.
   * Use buffer.position() to change buffer offset.
   *
   * @param m the source matrix.
   * @param buffer the destanation buffer.
   */
  def matrixToBuffer(m: AnyMat[_], buffer: DoubleBuffer) {
    buffer.put(m.d00)
    buffer.put(m.d10)
    buffer.put(m.d20)
    buffer.put(m.d30)

    buffer.put(m.d01)
    buffer.put(m.d11)
    buffer.put(m.d21)
    buffer.put(m.d31)

    buffer.put(m.d02)
    buffer.put(m.d12)
    buffer.put(m.d22)
    buffer.put(m.d32)

    buffer.put(m.d03)
    buffer.put(m.d13)
    buffer.put(m.d23)
    buffer.put(m.d33)
  }
}


/** <code>CoreMath</code> contains functions to operate on Ints and Int vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
object CoreMath extends CoreMath
