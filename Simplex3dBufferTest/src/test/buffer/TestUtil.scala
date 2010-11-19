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
import scala.reflect._
import simplex3d.buffer._
import simplex3d.buffer.RawType._
import simplex3d.buffer._
import simplex3d.buffer.floatm._
import simplex3d.buffer.doublem._
import simplex3d.math._
import simplex3d.math.floatm._
import simplex3d.math.doublem._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TestUtil extends FunSuite {

  private val randomSrc = new java.util.Random(0)
  private def ni = randomSrc.nextInt
  private def nf = randomSrc.nextFloat
  private def nd = randomSrc.nextDouble

  private def alloc(size: Int) = {
    ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder)
  }
  
  def size(capacity: Int, offset: Int, stride: Int, components: Int) = {
    val s = (capacity - offset + stride - components)/stride
    if (s > 0) s else 0
  }

  def isUnsigned(rawType: Int) :Boolean = {
    rawType match {
      case UByte => true
      case UShort => true
      case UInt => true
      case _ => false
    }
  }

  def wrap(bytes: ByteBuffer, descriptor: Descriptor[_, _]) :Buffer = {
    descriptor.rawType match {
      case SByte => bytes
      case UByte => bytes
      case SShort => bytes.asShortBuffer()
      case UShort => bytes.asCharBuffer()
      case SInt => bytes.asIntBuffer()
      case UInt => bytes.asIntBuffer()
      case HalfFloat => bytes.asShortBuffer()
      case RawFloat => bytes.asFloatBuffer()
      case RawDouble => bytes.asDoubleBuffer()
    }
  }

  def checkSubDataExceptions(seq: ReadDataSeq[_, _]) {
    intercept[IllegalArgumentException] {
      seq.rawBufferSubData(-1, seq.size)
    }

    intercept[IllegalArgumentException] {
      seq.rawBufferSubData(seq.size, seq.size)
    }

    intercept[IllegalArgumentException] {
      seq.rawBufferSubData(0, -1)
    }

    intercept[IllegalArgumentException] {
      seq.rawBufferSubData(0, seq.size + 1)
    }
  }

  def checkBuffer(testing: Buffer, data: Buffer) {
    assert(testing ne data)

    assert((testing.position, data.position) == (0, 0))
    assert(data.limit == data.capacity)
    assert(testing.limit == data.capacity)
    assert(testing.capacity == data.capacity)
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

  private def random(b: ByteBuffer, fillRandom: Boolean) :ByteBuffer = {
    if (!fillRandom) return b
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toByte)
      i += 1
    }
    b
  }
  private def random(b: ShortBuffer, fillRandom: Boolean) :ShortBuffer = {
    if (!fillRandom) return b
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toShort)
      i += 1
    }
    b
  }
  private def random(b: CharBuffer, fillRandom: Boolean) :CharBuffer = {
    if (!fillRandom) return b
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt.toChar)
      i += 1
    }
    b
  }
  private def random(b: IntBuffer, fillRandom: Boolean) :IntBuffer = {
    if (!fillRandom) return b
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextInt)
      i += 1
    }
    b
  }
  private def random(b: FloatBuffer, fillRandom: Boolean) :FloatBuffer = {
    if (!fillRandom) return b
    var i = 0; while(i < b.limit) {
      b.put(i, randomSrc.nextFloat)
      i += 1
    }
    b
  }
  private def random(b: DoubleBuffer, fillRandom: Boolean) :DoubleBuffer = {
    if (!fillRandom) return b
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
    byteCapacity: Int, descriptor: Descriptor[_, R], fillRandom: Boolean
  ) :(ByteBuffer, R#BufferType) = {
    (descriptor.rawType match {
      case SByte => val b = alloc(byteCapacity); (b, random(b.duplicate, fillRandom))
      case UByte => val b = alloc(byteCapacity); (b, random(b.duplicate, fillRandom))
      case SShort => val b = alloc(byteCapacity); (b, random(b.asShortBuffer, fillRandom))
      case UShort => val b = alloc(byteCapacity); (b, random(b.asCharBuffer, fillRandom))
      case SInt => val b = alloc(byteCapacity); (b, random(b.asIntBuffer, fillRandom))
      case UInt => val b = alloc(byteCapacity); (b, random(b.asIntBuffer, fillRandom))
      case HalfFloat => val b = alloc(byteCapacity); (b, random(b.asShortBuffer, fillRandom))
      case RawFloat => val b = alloc(byteCapacity); (b, random(b.asFloatBuffer, fillRandom))
      case RawDouble => val b = alloc(byteCapacity); (b, random(b.asDoubleBuffer, fillRandom))
    }).asInstanceOf[(ByteBuffer, R#BufferType)]
  }
  def genBuffer[R <: RawData](
    byteCapacity: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#BufferType) = {
    genBuffer(byteCapacity, descriptor, false)
  }
  def genRandomBuffer[R <: RawData](
    byteCapacity: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#BufferType) = {
    genBuffer(byteCapacity, descriptor, true)
  }

  private def rand[T](m: ClassManifest[T]) :T = {
    (m match {
      case MetaManifest.Int1 => ni
      case Vec2i.Manifest => Vec2i(ni, ni)
      case Vec3i.Manifest => Vec3i(ni, ni, ni)
      case Vec4i.Manifest => Vec4i(ni, ni, ni, ni)
      case MetaManifest.Float1 => nf
      case Vec2f.Manifest => Vec2f(nf, nf)
      case Vec3f.Manifest => Vec3f(nf, nf, nf)
      case Vec4f.Manifest => Vec4f(nf, nf, nf, nf)
      case MetaManifest.Double1 => nd
      case Vec2d.Manifest => Vec2d(nd, nd)
      case Vec3d.Manifest => Vec3d(nd, nd, nd)
      case Vec4d.Manifest => Vec4d(nd, nd, nd, nd)
    }).asInstanceOf[T]
  }
  private def randPrim[T](m: ClassManifest[T]) :T = {
    (m match {
      case MetaManifest.Int1 => ni.asInstanceOf[AnyRef]
      case MetaManifest.Float1 => nf.asInstanceOf[AnyRef]
      case MetaManifest.Double1 => nd.asInstanceOf[AnyRef]
    }).asInstanceOf[T]
  }
  private def mkPrimSeq[E <: MetaElement, R <: RawData](size: Int, descriptor: Descriptor[E, R]) = {
    (descriptor.componentManifest match {
      case MetaManifest.Int1 =>
        descriptor.rawType match {
          case SByte => DataArray[Int1, SByte](size*descriptor.components)
          case UByte => DataArray[Int1, UByte](size*descriptor.components)
          case SShort => DataArray[Int1, SShort](size*descriptor.components)
          case UShort => DataArray[Int1, UShort](size*descriptor.components)
          case SInt => DataArray[Int1, SInt](size*descriptor.components)
          case UInt => DataArray[Int1, UInt](size*descriptor.components)
        }
      case MetaManifest.Float1 =>
        descriptor.rawType match {
          case SByte => DataArray[Float1, SByte](size*descriptor.components)
          case UByte => DataArray[Float1, UByte](size*descriptor.components)
          case SShort => DataArray[Float1, SShort](size*descriptor.components)
          case UShort => DataArray[Float1, UShort](size*descriptor.components)
          case SInt => DataArray[Float1, SInt](size*descriptor.components)
          case UInt => DataArray[Float1, UInt](size*descriptor.components)
          case HalfFloat => DataArray[Float1, HalfFloat](size*descriptor.components)
          case RawFloat => DataArray[Float1, RawFloat](size*descriptor.components)
        }
      case MetaManifest.Double1 =>
        descriptor.rawType match {
          case SByte => DataArray[Double1, SByte](size*descriptor.components)
          case UByte => DataArray[Double1, UByte](size*descriptor.components)
          case SShort => DataArray[Double1, SShort](size*descriptor.components)
          case UShort => DataArray[Double1, UShort](size*descriptor.components)
          case SInt => DataArray[Double1, SInt](size*descriptor.components)
          case UInt => DataArray[Double1, UInt](size*descriptor.components)
          case HalfFloat => DataArray[Double1, HalfFloat](size*descriptor.components)
          case RawFloat => DataArray[Double1, RawFloat](size*descriptor.components)
          case RawDouble => DataArray[Double1, RawDouble](size*descriptor.components)
        }
    }).asInstanceOf[DataArray[E#Component, R]]
  }
  def genRandomCollection[E <: MetaElement, R <: RawData](
    size: Int, descriptor: Descriptor[E, R]
  ) :(Array[E#Read], Buffer) = {
    val array = descriptor.readManifest.newArray(size).asInstanceOf[Array[E#Read]]
    val seq = mkPrimSeq(size, descriptor)

    val seed = randomSrc.nextLong

    randomSrc.setSeed(seed)
    var i = 0; while (i < array.length) {
      array(i) = rand(descriptor.elementManifest).asInstanceOf[E#Read]
      i += 1
    }

    randomSrc.setSeed(seed)
    i = 0; while (i < seq.length) {
      seq(i) = randPrim(descriptor.componentManifest).asInstanceOf[E#Component#Read]
      i += 1
    }

    (array, seq.buffer)
  }
}
