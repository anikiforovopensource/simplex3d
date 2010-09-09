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
import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
import simplex3d.buffer.RawData._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TestUtil extends FunSuite {

  private val randomSrc = new java.util.Random(0)

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

  private def random(b: ByteBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toByte)
      i += 1
    }
    b
  }
  private def random(b: ShortBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toShort)
      i += 1
    }
    b
  }
  private def random(b: CharBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toChar)
      i += 1
    }
    b
  }
  private def random(b: IntBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt)
      i += 1
    }
    b
  }
  private def random(b: FloatBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextFloat)
      i += 1
    }
    b
  }
  private def random(b: DoubleBuffer, fillRandom: Boolean) = {
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextDouble)
      i += 1
    }
    b
  }

  private def genArray[R <: RawData](
    size: Int, descriptor: Descriptor[_, R], fillRandom: Boolean
  ) :R#ArrayType = {
    (descriptor.rawType match {
      case SByte => random(ByteBuffer.wrap(new Array[Byte](size)), fillRandom).array
      case UByte => random(ByteBuffer.wrap(new Array[Byte](size)), fillRandom).array
      case SShort => random(ShortBuffer.wrap(new Array[Short](size)), fillRandom).array
      case UShort => random(CharBuffer.wrap(new Array[Char](size)), fillRandom).array
      case SInt => random(IntBuffer.wrap(new Array[Int](size)), fillRandom).array
      case UInt => random(IntBuffer.wrap(new Array[Int](size)), fillRandom).array
      case HalfFloat => random(ShortBuffer.wrap(new Array[Short](size)), fillRandom).array
      case RawFloat => random(FloatBuffer.wrap(new Array[Float](size)), fillRandom).array
      case RawDouble => random(DoubleBuffer.wrap(new Array[Double](size)), fillRandom).array
    }).asInstanceOf[AnyRef].asInstanceOf[R#ArrayType]
  }
  def genArray[R <: RawData](
    size: Int, descriptor: Descriptor[_, R]
  ) :R#ArrayType = {
    genArray(size, descriptor, false)
  }
  def genRandomArray[R <: RawData](
    size: Int, descriptor: Descriptor[_, R]
  ) :R#ArrayType = {
    genArray(size, descriptor, true)
  }
  
  private def genBuffer[R <: RawData](
    byteSize: Int, descriptor: Descriptor[_, R], fillRandom: Boolean
  ) :(ByteBuffer, R#BufferType) = {
    (descriptor.rawType match {
      case SByte => val b = alloc(byteSize); (b, random(b, fillRandom))
      case UByte => val b = alloc(byteSize); (b, random(b, fillRandom))
      case SShort => val b = alloc(byteSize); (b, random(b.asShortBuffer, fillRandom))
      case UShort => val b = alloc(byteSize); (b, random(b.asCharBuffer, fillRandom))
      case SInt => val b = alloc(byteSize); (b, random(b.asIntBuffer, fillRandom))
      case UInt => val b = alloc(byteSize); (b, random(b.asIntBuffer, fillRandom))
      case HalfFloat => val b = alloc(byteSize); (b, random(b.asShortBuffer, fillRandom))
      case RawFloat => val b = alloc(byteSize); (b, random(b.asFloatBuffer, fillRandom))
      case RawDouble => val b = alloc(byteSize); (b, random(b.asDoubleBuffer, fillRandom))
    }).asInstanceOf[(ByteBuffer, R#BufferType)]
  }
  def genBuffer[R <: RawData](
    byteSize: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#BufferType) = {
    genBuffer(byteSize, descriptor, false)
  }
  def genRandomBuffer[R <: RawData](
    byteSize: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#BufferType) = {
    genBuffer(byteSize, descriptor, true)
  }
}
