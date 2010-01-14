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


/**
 * @author Aleksey Nikiforov (lex)
 */
object BaseMath {

    // Random
    private val randomLoc = new ThreadLocal[java.util.Random] {
        override def initialValue = new java.util.Random
    }
    private[math] def random = randomLoc.get

    def setSeed(seed: Long) { random.setSeed(seed) }
    def nextBoolean() :Boolean = random.nextBoolean
    def nextByte() :Byte = byte(random.nextInt)
    def nextByte(n: Int) :Byte = byte(random.nextInt(n))
    def nextShort() :Short = short(random.nextInt)
    def nextShort(n: Int) :Short = short(random.nextInt(n))
    def nextInt() :Int = random.nextInt
    def nextInt(n: Int) = random.nextInt(n)
    def nextFloat() :Float = random.nextFloat
    def nextDouble() :Double = random.nextDouble

    def nextVec2b() :Vec2b = Vec2b(nextBoolean, nextBoolean)
    def nextVec3b() :Vec3b = Vec3b(nextBoolean, nextBoolean, nextBoolean)
    def nextVec4b() :Vec4b = {
        Vec4b(nextBoolean, nextBoolean, nextBoolean, nextBoolean)
    }

    // Cast
    def byte(x: Byte) :Byte = x
    def byte(x: Short) :Byte = x.asInstanceOf[Byte]
    def byte(x: Int) :Byte = x.asInstanceOf[Byte]
    def byte(x: Long) :Byte = x.asInstanceOf[Byte]
    def byte(x: Float) :Byte = x.asInstanceOf[Byte]
    def byte(x: Double) :Byte = x.asInstanceOf[Byte]
    def short(x: Byte) :Short = x.asInstanceOf[Short]
    def short(x: Short) :Short = x
    def short(x: Int) :Short = x.asInstanceOf[Short]
    def short(x: Long) :Short = x.asInstanceOf[Short]
    def short(x: Float) :Short = x.asInstanceOf[Short]
    def short(x: Double) :Short = x.asInstanceOf[Short]
    def int(x: Byte) :Int = x.asInstanceOf[Int]
    def int(x: Short) :Int = x.asInstanceOf[Int]
    def int(x: Int) :Int = x
    def int(x: Long) :Int = x.asInstanceOf[Int]
    def int(x: Float) :Int = x.asInstanceOf[Int]
    def int(x: Double) :Int = x.asInstanceOf[Int]
    def long(x: Byte) :Long = x.asInstanceOf[Long]
    def long(x: Short) :Long = x.asInstanceOf[Long]
    def long(x: Int) :Long = x.asInstanceOf[Long]
    def long(x: Long) :Long = x
    def long(x: Float) :Long = x.asInstanceOf[Long]
    def long(x: Double) :Long = x.asInstanceOf[Long]
    def float(x: Byte) :Float = x.asInstanceOf[Float]
    def float(x: Short) :Float = x.asInstanceOf[Float]
    def float(x: Int) :Float = x.asInstanceOf[Float]
    def float(x: Long) :Float = x.asInstanceOf[Float]
    def float(x: Float) :Float = x
    def float(x: Double) :Float = x.asInstanceOf[Float]
    def double(x: Byte) :Double = x.asInstanceOf[Double]
    def double(x: Short) :Double = x.asInstanceOf[Double]
    def double(x: Int) :Double = x.asInstanceOf[Double]
    def double(x: Long) :Double = x.asInstanceOf[Double]
    def double(x: Float) :Double = x.asInstanceOf[Double]
    def double(x: Double) :Double = x

    // Vec2b functions
    def any(u: AnyVec2b) :Boolean = {
        u.x || u.y
    }
    def all(u: AnyVec2b) :Boolean = {
        u.x && u.y
    }
    def not(u: AnyVec2b) :Vec2b = Vec2b(!u.x, !u.y)

    // Vec3b functions
    def any(u: AnyVec3b) :Boolean = {
        u.x || u.y || u.z
    }
    def all(u: AnyVec3b) :Boolean = {
        u.x && u.y && u.z
    }
    def not(u: AnyVec3b) :Vec3b = Vec3b(!u.x, !u.y, !u.z)

    // Vec4b functions
    def any(u: AnyVec4b) :Boolean = {
        u.x || u.y || u.z || u.w
    }
    def all(u: AnyVec4b) :Boolean = {
        u.x && u.y && u.z && u.w
    }
    def not(u: AnyVec4b) :Vec4b = Vec4b(!u.x, !u.y, !u.z, !u.w)
}
