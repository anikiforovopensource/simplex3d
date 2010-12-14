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

import Descriptors._


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

  private def RandomDataArray[E <: MetaElement, R <: RawData](size: Int)(
    implicit factory: DataSeqFactory[E, R], descriptor: Descriptor[E, R]
  ) :DataArray[E, R] = {
    factory.mkDataArray(genRandomArray(size*descriptor.components, descriptor))
  }
  def genRandomSeq(size: Int) :Data[_ <: MetaElement] = {
    genRandomSeq(None, None, size)
  }
  def genRandomSeq(manifest: Option[ClassManifest[_]], rawType: Option[Int], size: Int) :Data[_ <: MetaElement] = {
    val m = manifest match {
      case Some(man) =>
        man
      case None =>
        randomSrc.nextInt(12) match {
          case 0 => MetaManifest.Int1
          case 1 => Vec2i.Manifest
          case 2 => Vec3i.Manifest
          case 3 => Vec4i.Manifest
          case 4 => MetaManifest.Float1
          case 5 => Vec2f.Manifest
          case 6 => Vec3f.Manifest
          case 7 => Vec4f.Manifest
          case 8 => MetaManifest.Double1
          case 9 => Vec2d.Manifest
          case 10 => Vec3d.Manifest
          case 11 => Vec4d.Manifest
        }
      }
    
    val max = m match {
      case MetaManifest.Int1 | Vec2i.Manifest | Vec3i.Manifest | Vec4i.Manifest => 6
      case MetaManifest.Float1 | Vec2f.Manifest | Vec3f.Manifest | Vec4f.Manifest => 8
      case MetaManifest.Double1 | Vec2d.Manifest | Vec3d.Manifest | Vec4d.Manifest => 9
    }
    
    val r = rawType match {
      case Some(i) => assert(i < max); i
      case None => randomSrc.nextInt(max) match {
        case 0 => SByte
        case 1 => UByte
        case 2 => SShort
        case 3 => UShort
        case 4 => SInt
        case 5 => UInt
        case 6 => HalfFloat
        case 7 => RawFloat
        case 8 => RawDouble
      }
    }
      
      genRandomSeq(m, r, size)
  }
  
  def genRandomSeq(manifest: ClassManifest[_], rawType: Int, size: Int) :Data[_ <: MetaElement] = {
    manifest match {
      case MetaManifest.Int1 => rawType match {
        case SByte => RandomDataArray[Int1, SByte](size)
        case UByte => RandomDataArray[Int1, UByte](size)
        case SShort => RandomDataArray[Int1, SShort](size)
        case UShort => RandomDataArray[Int1, UShort](size)
        case SInt => RandomDataArray[Int1, SInt](size)
        case UInt => RandomDataArray[Int1, UInt](size)
      }
      case Vec2i.Manifest => rawType match {
        case SByte => RandomDataArray[Vec2i, SByte](size)
        case UByte => RandomDataArray[Vec2i, UByte](size)
        case SShort => RandomDataArray[Vec2i, SShort](size)
        case UShort => RandomDataArray[Vec2i, UShort](size)
        case SInt => RandomDataArray[Vec2i, SInt](size)
        case UInt => RandomDataArray[Vec2i, UInt](size)
      }
      case Vec3i.Manifest => rawType match {
        case SByte => RandomDataArray[Vec3i, SByte](size)
        case UByte => RandomDataArray[Vec3i, UByte](size)
        case SShort => RandomDataArray[Vec3i, SShort](size)
        case UShort => RandomDataArray[Vec3i, UShort](size)
        case SInt => RandomDataArray[Vec3i, SInt](size)
        case UInt => RandomDataArray[Vec3i, UInt](size)
      }
      case Vec4i.Manifest => rawType match {
        case SByte => RandomDataArray[Vec4i, SByte](size)
        case UByte => RandomDataArray[Vec4i, UByte](size)
        case SShort => RandomDataArray[Vec4i, SShort](size)
        case UShort => RandomDataArray[Vec4i, UShort](size)
        case SInt => RandomDataArray[Vec4i, SInt](size)
        case UInt => RandomDataArray[Vec4i, UInt](size)
      }
      
      case MetaManifest.Float1 => rawType match {
        case SByte => RandomDataArray[Float1, SByte](size)
        case UByte => RandomDataArray[Float1, UByte](size)
        case SShort => RandomDataArray[Float1, SShort](size)
        case UShort => RandomDataArray[Float1, UShort](size)
        case SInt => RandomDataArray[Float1, SInt](size)
        case UInt => RandomDataArray[Float1, UInt](size)
        case HalfFloat => RandomDataArray[Float1, HalfFloat](size)
        case RawFloat => RandomDataArray[Float1, RawFloat](size)
      }
      case Vec2f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec2f, SByte](size)
        case UByte => RandomDataArray[Vec2f, UByte](size)
        case SShort => RandomDataArray[Vec2f, SShort](size)
        case UShort => RandomDataArray[Vec2f, UShort](size)
        case SInt => RandomDataArray[Vec2f, SInt](size)
        case UInt => RandomDataArray[Vec2f, UInt](size)
        case HalfFloat => RandomDataArray[Vec2f, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec2f, RawFloat](size)
      }
      case Vec3f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec3f, SByte](size)
        case UByte => RandomDataArray[Vec3f, UByte](size)
        case SShort => RandomDataArray[Vec3f, SShort](size)
        case UShort => RandomDataArray[Vec3f, UShort](size)
        case SInt => RandomDataArray[Vec3f, SInt](size)
        case UInt => RandomDataArray[Vec3f, UInt](size)
        case HalfFloat => RandomDataArray[Vec3f, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec3f, RawFloat](size)
      }
      case Vec4f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec4f, SByte](size)
        case UByte => RandomDataArray[Vec4f, UByte](size)
        case SShort => RandomDataArray[Vec4f, SShort](size)
        case UShort => RandomDataArray[Vec4f, UShort](size)
        case SInt => RandomDataArray[Vec4f, SInt](size)
        case UInt => RandomDataArray[Vec4f, UInt](size)
        case HalfFloat => RandomDataArray[Vec4f, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec4f, RawFloat](size)
      }
      
      case MetaManifest.Double1 => rawType match {
        case SByte => RandomDataArray[Double1, SByte](size)
        case UByte => RandomDataArray[Double1, UByte](size)
        case SShort => RandomDataArray[Double1, SShort](size)
        case UShort => RandomDataArray[Double1, UShort](size)
        case SInt => RandomDataArray[Double1, SInt](size)
        case UInt => RandomDataArray[Double1, UInt](size)
        case HalfFloat => RandomDataArray[Double1, HalfFloat](size)
        case RawFloat => RandomDataArray[Double1, RawFloat](size)
        case RawDouble => RandomDataArray[Double1, RawDouble](size)
      }
      case Vec2d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec2d, SByte](size)
        case UByte => RandomDataArray[Vec2d, UByte](size)
        case SShort => RandomDataArray[Vec2d, SShort](size)
        case UShort => RandomDataArray[Vec2d, UShort](size)
        case SInt => RandomDataArray[Vec2d, SInt](size)
        case UInt => RandomDataArray[Vec2d, UInt](size)
        case HalfFloat => RandomDataArray[Vec2d, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec2d, RawFloat](size)
        case RawDouble => RandomDataArray[Vec2d, RawDouble](size)
      }
      case Vec3d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec3d, SByte](size)
        case UByte => RandomDataArray[Vec3d, UByte](size)
        case SShort => RandomDataArray[Vec3d, SShort](size)
        case UShort => RandomDataArray[Vec3d, UShort](size)
        case SInt => RandomDataArray[Vec3d, SInt](size)
        case UInt => RandomDataArray[Vec3d, UInt](size)
        case HalfFloat => RandomDataArray[Vec3d, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec3d, RawFloat](size)
        case RawDouble => RandomDataArray[Vec3d, RawDouble](size)
      }
      case Vec4d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec4d, SByte](size)
        case UByte => RandomDataArray[Vec4d, UByte](size)
        case SShort => RandomDataArray[Vec4d, SShort](size)
        case UShort => RandomDataArray[Vec4d, UShort](size)
        case SInt => RandomDataArray[Vec4d, SInt](size)
        case UInt => RandomDataArray[Vec4d, UInt](size)
        case HalfFloat => RandomDataArray[Vec4d, HalfFloat](size)
        case RawFloat => RandomDataArray[Vec4d, RawFloat](size)
        case RawDouble => RandomDataArray[Vec4d, RawDouble](size)
      }
    }
  }
  
  final def testContent(
    components: Int,
    dest: inData[_], destFirst: Int,
    src: inData[_], srcFirst: Int,
    count: Int
  ) {
    val d = dest.backingSeq
    val s = src.backingSeq
    
    d.elementManifest match {
      case MetaManifest.Int1 =>
        testIntContent(
          components,
          d.asInstanceOf[ReadData[Int1]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[Int1]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
      case MetaManifest.Float1 =>
        testFloatContent(
          components,
          d.asInstanceOf[ReadData[Float1]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[Float1]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
      case MetaManifest.Double1 =>
        testDoubleContent(
          components,
          d.asInstanceOf[ReadData[Double1]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[Double1]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
    }
  }
  
  final def testTheRest(
    components: Int,
    dest: inData[_], destFirst: Int,
    original: inData[_],
    count: Int
  ) {
    if (dest.isInstanceOf[DataView[_, _]]) {
      val d = DataBuffer[Int1, SByte](dest.asInstanceOf[DataView[_, _]].backingSeq)
      val o = DataBuffer[Int1, SByte](original.asInstanceOf[DataView[_, _]].backingSeq)
      
      val byteOffset = dest.byteOffset + dest.byteStride*destFirst
      val byteSkip = components*dest.bytesPerRawComponent
      val byteLimit = byteOffset + dest.byteStride*count - (dest.byteStride - byteSkip)
      
      // Beggining
      var i = 0; while (i < byteOffset) {
        assert(d(i) == o(i))
        
        i += 1
      }
      
      // Pertially modified
      i = byteOffset; while (i < byteLimit) {
        var j = byteSkip; while (j < dest.byteStride) {
          assert(d(i + j) == o(i + j))
          
          j += 1
        }
        
        i += dest.byteStride
      }
      
      // Remaining
      i = byteLimit; while (i < dest.byteCapacity) {
        assert(d(i) == o(i))
        
        i += 1
      }
    }
    else {
      // Beginning
      testContent(components, dest, 0, original, 0, destFirst)
      
      // Remaining
      val lim = destFirst + count
      val rem = dest.size - lim
      if (rem > 0) testContent(components, dest, lim, original, lim, rem)
    }
  }
  
  private final def testIntContent(
    components: Int,
    dest: inData[Int1], destFirst: Int, destStride: Int,
    src: inData[Int1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Int1)
    assert(src.elementManifest == MetaManifest.Int1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        assert(dest(destFirst + destStride*i + j) == src(srcFirst + srcStride*i + j))
        
        j += 1
      }
      
      i += 1
    }
  }
  
  private final def testFloatContent(
    components: Int,
    dest: inData[Float1], destFirst: Int, destStride: Int,
    src: inData[Float1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Float1)
    assert(src.elementManifest == MetaManifest.Float1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        val a = dest(destFirst + destStride*i + j)
        val b = src(srcFirst + srcStride*i + j)
        if (FloatMath.isnan(a)) assert(FloatMath.isnan(b))
        else assert(a == b)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  private final def testDoubleContent(
    components: Int,
    dest: inData[Double1], destFirst: Int, destStride: Int,
    src: inData[Double1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Double1)
    assert(src.elementManifest == MetaManifest.Double1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        val a = dest(destFirst + destStride*i + j)
        val b = src(srcFirst + srcStride*i + j)
        if (DoubleMath.isnan(a)) assert(DoubleMath.isnan(b))
        else assert(a == b)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  def convert(src: inData[_], rawType: Int) :Data[_] = {
    val factory = genRandomSeq(src.backingSeq.elementManifest, rawType, 0)
    val contiguousCopy = factory.mkDataArray(src.components*src.size)
    
    // put content
    
    contiguousCopy
  }
  
  private final def putIntContent(
    components: Int,
    dest: outContiguousSeq[Int1, RawData],
    src: inData[Int1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Int1)
    assert(src.elementManifest == MetaManifest.Int1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        dest(components*i + j) = src(srcFirst + srcStride*i + j)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  private final def putFloatContent(
    components: Int,
    dest: outContiguousSeq[Float1, RawData],
    src: inData[Float1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Float1)
    assert(src.elementManifest == MetaManifest.Float1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        dest(components*i + j) = src(srcFirst + srcStride*i + j)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  private final def putDoubleContent(
    components: Int,
    dest: outContiguousSeq[Double1, RawData],
    src: inData[Double1], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elementManifest == MetaManifest.Double1)
    assert(src.elementManifest == MetaManifest.Double1)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        dest(components*i + j) = src(srcFirst + srcStride*i + j)
        
        j += 1
      }
      
      i += 1
    }
  }
}
