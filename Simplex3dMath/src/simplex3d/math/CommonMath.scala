/*
 ** Simplex3d, CoreMath module
 * Copyright (C) 2009-2011, Simplex3d Team
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
  final def Bool(x: Boolean) :Boolean = x

  /** Casts an Int to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0, true otherwise.
   */
  final def Bool(x: Int) :Boolean = (x != 0)

  /** Casts a Float to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  final def Bool(x: Float) :Boolean = (x != 0)

  /** Casts a Double to a Boolean.
   * @param x a value to cast.
   * @return false if x == 0.0, true otherwise.
   */
  final def Bool(x: Double) :Boolean = (x != 0)
  
  final def Bool(u: AnyVec[_]) :Boolean = u.bx

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


  // Generic matrix functions.

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   */
  final def matrixToArray(m: AnyMat[_], array: Array[Float]) {
    matrixToArray(m, array, 0)
  }

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   * @param offset an offset into the array.
   */
  final def matrixToArray(m: AnyMat[_], array: Array[Float], offset: Int) {
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
  final def matrixToBuffer(m: AnyMat[_], buffer: FloatBuffer) {
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
  final def matrixToArray(m: AnyMat[_], array: Array[Double]) {
    matrixToArray(m, array, 0)
  }

  /** Writes a matrix into a given array in column major order.
   * @param m the source matrix.
   * @param array the destanation array.
   * @param offset an offset into the array.
   */
  final def matrixToArray(m: AnyMat[_], array: Array[Double], offset: Int) {
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
  final def matrixToBuffer(m: AnyMat[_], buffer: DoubleBuffer) {
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
