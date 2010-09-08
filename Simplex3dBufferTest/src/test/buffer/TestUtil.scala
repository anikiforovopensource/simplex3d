/*
 * Simplex3d, BufferTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBufferTest.
 *
 * Simplex3dBufferTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBufferTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package test.buffer

import org.scalatest._

import java.nio._
import simplex3d.buffer._
import simplex3d.buffer.RawData._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TestUtil extends FunSuite {

  private val random = new java.util.Random(0)

  def checkBuffer(testing: Buffer, data: Buffer) {
    assert(testing ne data)

    assert((testing.position, data.position) == (0, 0))
    assert(testing.capacity == data.capacity)
    assert((testing.limit, data.limit) == (testing.capacity, testing.capacity))
    assert(testing.equals(data))

    (testing, data) match {
      case (b1: ByteBuffer, b2: ByteBuffer) =>
      case (b1: ShortBuffer, b2: ShortBuffer) =>
      case (b1: CharBuffer, b2: CharBuffer) =>
      case (b1: IntBuffer, b2: IntBuffer) =>
      case (b1: FloatBuffer, b2: FloatBuffer) =>
      case (b1: DoubleBuffer, b2: DoubleBuffer) =>
    }
  }

  def wrapArray(array: AnyRef) :Buffer = {
    array match {
      case a: Array[Byte] => ByteBuffer.wrap(a)
      case a: Array[Short] => ShortBuffer.wrap(a)
      case a: Array[Char] => CharBuffer.wrap(a)
      case a: Array[Int] => IntBuffer.wrap(a)
      case a: Array[Float] => FloatBuffer.wrap(a)
      case a: Array[Double] => DoubleBuffer.wrap(a)
    }
  }

  private def byteArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Byte](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextInt.toByte
      }
    }
    array
  }
  private def shortArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Short](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextInt.toShort
      }
    }
    array
  }
  private def charArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Char](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextInt.toChar
      }
    }
    array
  }
  private def intArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Int](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextInt
      }
    }
    array
  }
  private def floatArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Float](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextFloat
      }
    }
    array
  }
  private def doubleArray(size: Int, fillRandom: Boolean) = {
    val array = new Array[Double](size)
    if (fillRandom) {
      var i = 0; while (i < array.length) {
        array(i) = random.nextDouble
      }
    }
    array
  }

  private def genArray[A](count: Int, descriptor: Descriptor[_, _], fillRandom: Boolean) :A = {
    (descriptor.rawType match {
      case SByte => byteArray(descriptor.components*count, fillRandom)
      case UByte => byteArray(descriptor.components*count, fillRandom)
      case SShort => shortArray(descriptor.components*count, fillRandom)
      case UShort => charArray(descriptor.components*count, fillRandom)
      case SInt => intArray(descriptor.components*count, fillRandom)
      case UInt => intArray(descriptor.components*count, fillRandom)
      case HalfFloat => shortArray(descriptor.components*count, fillRandom)
      case RawFloat => floatArray(descriptor.components*count, fillRandom)
      case RawDouble => doubleArray(descriptor.components*count, fillRandom)
    }).asInstanceOf[A]
  }
  def genArray[A](count: Int, descriptor: Descriptor[_, _]) :A = {
    genArray(count, descriptor, false)
  }
  def genRandomArray[A](count: Int, descriptor: Descriptor[_, _]) :A = {
    genArray(count, descriptor, true)
  }
}
