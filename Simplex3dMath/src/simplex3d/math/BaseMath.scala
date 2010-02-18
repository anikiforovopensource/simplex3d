/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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


/** BaseMath contains casting functions and functions to operate on boolean
 *  vectors.
 *
 *  @author Aleksey Nikiforov (lex)
 */
object BaseMath {

    // Casting.
    // bool() instead of boolean(), since Boolean can not be cast to other types
    // in Scala nor Java.

    /** Cast Boolean to Boolean. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def bool(x: Boolean) :Boolean = x

    /** Cast Byte to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0, true otherwise.
     */
    def bool(x: Byte) :Boolean = (x != 0)

    /** Cast Short to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0, true otherwise.
     */
    def bool(x: Short) :Boolean = (x != 0)

    /** Cast Int to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0, true otherwise.
     */
    def bool(x: Int) :Boolean = (x != 0)

    /** Cast Long to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0, true otherwise.
     */
    def bool(x: Long) :Boolean = (x != 0)

    /** Cast Float to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0.0, true otherwise.
     */
    def bool(x: Float) :Boolean = (x != 0)

    /** Cast Double to Boolean.
     *  @param x a value to cast.
     *  @return false if x == 0.0, true otherwise.
     */
    def bool(x: Double) :Boolean = (x != 0)

    /** Cast Boolean to Byte.
     *  @param x a value to cast.
     *  @return 1 if x is true, 0 otherwise.
     */
    def byte(x: Boolean) :Byte = if (x) 1 else 0

    /** Cast Byte to Byte. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def byte(x: Byte) :Byte = x

    /** Cast Short to Byte.
     *  @param x a value to cast.
     *  @return 8 lower bits of x.
     */
    def byte(x: Short) :Byte = x.asInstanceOf[Byte]

    /** Cast Int to Byte.
     *  @param x a value to cast.
     *  @return 8 lower bits of x.
     */
    def byte(x: Int) :Byte = x.asInstanceOf[Byte]

    /** Cast Long to Byte.
     *  @param x a value to cast.
     *  @return 8 lower bits of x.
     */
    def byte(x: Long) :Byte = x.asInstanceOf[Byte]

    /** Cast Float to Byte.
     *  @param x a value to cast.
     *  @return 8 lower bits of integer part of x,
     *           0 if x <= Int.MinValue,
     *          -1 if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def byte(x: Float) :Byte = x.asInstanceOf[Byte]

    /** Cast Double to Byte.
     *  @param x a value to cast.
     *  @return 8 lower bits of integer part of x,
     *           0 if x <= Int.MinValue,
     *          -1 if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def byte(x: Double) :Byte = x.asInstanceOf[Byte]

    /** Cast Boolean to Short.
     *  @param x a value to cast.
     *  @return 1 if x is true, 0 otherwise.
     */
    def short(x: Boolean) :Short = if (x) 1 else 0

    /** Cast Byte to Short.
     *  @param x a value to cast.
     *  @return x as Short keeping the sign.
     */
    def short(x: Byte) :Short = x.asInstanceOf[Short]

    /** Cast Short to Short. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def short(x: Short) :Short = x

    /** Cast Int to Short.
     *  @param x a value to cast.
     *  @return 16 lower bits of x.
     */
    def short(x: Int) :Short = x.asInstanceOf[Short]

    /** Cast Long to Short.
     *  @param x a value to cast.
     *  @return 16 lower bits of x.
     */
    def short(x: Long) :Short = x.asInstanceOf[Short]

    /** Cast Float to Short.
     *  @param x a value to cast.
     *  @return 16 lower bits of integer part of x,
     *           0 if x <= Int.MinValue,
     *          -1 if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def short(x: Float) :Short = x.asInstanceOf[Short]

    /** Cast Double to Short.
     *  @param x a value to cast.
     *  @return 16 lower bits of integer part of x,
     *           0 if x <= Int.MinValue,
     *          -1 if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def short(x: Double) :Short = x.asInstanceOf[Short]

    /** Cast Boolean to Int.
     *  @param x a value to cast.
     *  @return 1 if x is true, 0 otherwise.
     */
    def int(x: Boolean) :Int = if (x) 1 else 0

    /** Cast Byte to Int.
     *  @param x a value to cast.
     *  @return x as Int keeping the sign.
     */
    def int(x: Byte) :Int = x.asInstanceOf[Int]

    /** Cast Short to Int.
     *  @param x a value to cast.
     *  @return x as Int keeping the sign.
     */
    def int(x: Short) :Int = x.asInstanceOf[Int]

    /** Cast Int to Int. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def int(x: Int) :Int = x

    /** Cast Long to Int.
     *  @param x a value to cast.
     *  @return 32 lower bits of x.
     */
    def int(x: Long) :Int = x.asInstanceOf[Int]

    /** Cast Float to Int.
     *  @param x a value to cast.
     *  @return integer part of x,
     *           Int.MinValue if x <= Int.MinValue,
     *           Int.MaxValue if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def int(x: Float) :Int = x.asInstanceOf[Int]

    /** Cast Double to Int.
     *  @param x a value to cast.
     *  @return integer part of x,
     *           Int.MinValue if x <= Int.MinValue,
     *           Int.MaxValue if x >= Int.MaxValue,
     *           possible loss of precision.
     */
    def int(x: Double) :Int = x.asInstanceOf[Int]

    /** Cast Boolean to Long.
     *  @param x a value to cast.
     *  @return 1 if x is true, 0 otherwise.
     */
    def long(x: Boolean) :Long = if (x) 1 else 0

    /** Cast Byte to Long.
     *  @param x a value to cast.
     *  @return x as Long keeping the sign.
     */
    def long(x: Byte) :Long = x.asInstanceOf[Long]

    /** Cast Short to Long.
     *  @param x a value to cast.
     *  @return x as Long keeping the sign.
     */
    def long(x: Short) :Long = x.asInstanceOf[Long]

    /** Cast Int to Long.
     *  @param x a value to cast.
     *  @return x as Long keeping the sign.
     */
    def long(x: Int) :Long = x.asInstanceOf[Long]

    /** Cast Long to Long. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def long(x: Long) :Long = x

    /** Cast Float to Long.
     *  @param x a value to cast.
     *  @return integer part of x,
     *           Long.MinValue if x <= Long.MinValue,
     *           Long.MaxValue if x >= Long.MaxValue,
     *           possible loss of precision.
     */
    def long(x: Float) :Long = x.asInstanceOf[Long]

    /** Cast Double to Long.
     *  @param x a value to cast.
     *  @return integer part of x,
     *           Long.MinValue if x <= Long.MinValue,
     *           Long.MaxValue if x >= Long.MaxValue,
     *           possible loss of precision.
     */
    def long(x: Double) :Long = x.asInstanceOf[Long]

    /** Cast Boolean to Float.
     *  @param x a value to cast.
     *  @return 1.0 if x is true, 0.0 otherwise.
     */
    def float(x: Boolean) :Float = if (x) 1 else 0

    /** Cast Byte to Float.
     *  @param x a value to cast.
     *  @return x as Float.
     */
    def float(x: Byte) :Float = x.asInstanceOf[Float]

    /** Cast Short to Float.
     *  @param x a value to cast.
     *  @return x as Float.
     */
    def float(x: Short) :Float = x.asInstanceOf[Float]

    /** Cast Int to Float.
     *  @param x a value to cast.
     *  @return x as Float, possible loss of precision.
     */
    def float(x: Int) :Float = x.asInstanceOf[Float]

    /** Cast Long to Float.
     *  @param x a value to cast.
     *  @return x as Float, possible loss of precision.
     */
    def float(x: Long) :Float = x.asInstanceOf[Float]

    /** Cast Float to Float. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def float(x: Float) :Float = x

    /** Cast Double to Float.
     *  @param x a value to cast.
     *  @return x as Float, possible loss of precision.
     */
    def float(x: Double) :Float = x.asInstanceOf[Float]

    /** Cast Boolean to Double.
     *  @param x a value to cast.
     *  @return 1.0 if x is true, 0.0 otherwise.
     */
    def double(x: Boolean) :Double = if (x) 1 else 0

    /** Cast Byte to Double.
     *  @param x a value to cast.
     *  @return x as Double.
     */
    def double(x: Byte) :Double = x.asInstanceOf[Double]

    /** Cast Short to Double.
     *  @param x a value to cast.
     *  @return x as Double.
     */
    def double(x: Short) :Double = x.asInstanceOf[Double]

    /** Cast Int to Double.
     *  @param x a value to cast.
     *  @return x as Double.
     */
    def double(x: Int) :Double = x.asInstanceOf[Double]

    /** Cast Long to Double.
     *  @param x a value to cast.
     *  @return x as Double, possible loss of precision.
     */
    def double(x: Long) :Double = x.asInstanceOf[Double]

    /** Cast Float to Double.
     *  @param x a value to cast.
     *  @return x as Double.
     */
    def double(x: Float) :Double = x.asInstanceOf[Double]

    /** Cast Double to Double. This method is here for completeness.
     *  @param x a value to cast.
     *  @return x.
     */
    def double(x: Double) :Double = x


    // Vec2b functions.

    /** Equivalent to logical OR on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if any of the components are true, false otherwise.
     */
    def any(u: AnyVec2b) :Boolean = {
        u.x || u.y
    }

    /** Equivalent to logical AND on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if all the components are true, false otherwise.
     */
    def all(u: AnyVec2b) :Boolean = {
        u.x && u.y
    }

    /** Equivalent to logical NOT on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return a boolean vector with negated components u.
     */
    def not(u: AnyVec2b) :Vec2b = Vec2b(!u.x, !u.y)


    // Vec3b functions.

    /** Equivalent to logical OR on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if any of the components are true, false otherwise.
     */
    def any(u: AnyVec3b) :Boolean = {
        u.x || u.y || u.z
    }

    /** Equivalent to logical AND on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if all the components are true, false otherwise.
     */
    def all(u: AnyVec3b) :Boolean = {
        u.x && u.y && u.z
    }

    /** Equivalent to logical NOT on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return a boolean vector with negated components u.
     */
    def not(u: AnyVec3b) :Vec3b = Vec3b(!u.x, !u.y, !u.z)


    // Vec4b functions.

    /** Equivalent to logical OR on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if any of the components are true, false otherwise.
     */
    def any(u: AnyVec4b) :Boolean = {
        u.x || u.y || u.z || u.w
    }

    /** Equivalent to logical AND on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return true if all the components are true, false otherwise.
     */
    def all(u: AnyVec4b) :Boolean = {
        u.x && u.y && u.z && u.w
    }

    /** Equivalent to logical NOT on the components of the argument vector.
     *  @param u a boolean vector.
     *  @return a boolean vector with negated components u.
     */
    def not(u: AnyVec4b) :Vec4b = Vec4b(!u.x, !u.y, !u.z, !u.w)
}
