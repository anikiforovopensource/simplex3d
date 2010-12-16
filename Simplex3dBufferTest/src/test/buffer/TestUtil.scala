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
import simplex3d.math.CoreMath._

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
      case HFloat => bytes.asShortBuffer()
      case RFloat => bytes.asFloatBuffer()
      case RDouble => bytes.asDoubleBuffer()
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

  private def genArray[R <: Raw](
    size: Int, descriptor: Descriptor[_, R], fillRandom: Boolean
  ) :R#Array = {
    (descriptor.rawType match {
      case SByte => random(ByteBuffer.wrap(new Array[Byte](size)), fillRandom).array
      case UByte => random(ByteBuffer.wrap(new Array[Byte](size)), fillRandom).array
      case SShort => random(ShortBuffer.wrap(new Array[Short](size)), fillRandom).array
      case UShort => random(CharBuffer.wrap(new Array[Char](size)), fillRandom).array
      case SInt => random(IntBuffer.wrap(new Array[Int](size)), fillRandom).array
      case UInt => random(IntBuffer.wrap(new Array[Int](size)), fillRandom).array
      case HFloat => random(ShortBuffer.wrap(new Array[Short](size)), fillRandom).array
      case RFloat => random(FloatBuffer.wrap(new Array[Float](size)), fillRandom).array
      case RDouble => random(DoubleBuffer.wrap(new Array[Double](size)), fillRandom).array
    }).asInstanceOf[AnyRef].asInstanceOf[R#Array]
  }
  def genArray[R <: Raw](
    size: Int, descriptor: Descriptor[_, R]
  ) :R#Array = {
    genArray(size, descriptor, false)
  }
  def genRandomArray[R <: Raw](
    size: Int, descriptor: Descriptor[_, R]
  ) :R#Array = {
    genArray(size, descriptor, true)
  }
  
  private def genBuffer[R <: Raw](
    byteCapacity: Int, descriptor: Descriptor[_, R], fillRandom: Boolean
  ) :(ByteBuffer, R#Buffer) = {
    (descriptor.rawType match {
      case SByte => val b = alloc(byteCapacity); (b, random(b.duplicate, fillRandom))
      case UByte => val b = alloc(byteCapacity); (b, random(b.duplicate, fillRandom))
      case SShort => val b = alloc(byteCapacity); (b, random(b.asShortBuffer, fillRandom))
      case UShort => val b = alloc(byteCapacity); (b, random(b.asCharBuffer, fillRandom))
      case SInt => val b = alloc(byteCapacity); (b, random(b.asIntBuffer, fillRandom))
      case UInt => val b = alloc(byteCapacity); (b, random(b.asIntBuffer, fillRandom))
      case HFloat => val b = alloc(byteCapacity); (b, random(b.asShortBuffer, fillRandom))
      case RFloat => val b = alloc(byteCapacity); (b, random(b.asFloatBuffer, fillRandom))
      case RDouble => val b = alloc(byteCapacity); (b, random(b.asDoubleBuffer, fillRandom))
    }).asInstanceOf[(ByteBuffer, R#Buffer)]
  }
  def genBuffer[R <: Raw](
    byteCapacity: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#Buffer) = {
    genBuffer(byteCapacity, descriptor, false)
  }
  def genRandomBuffer[R <: Raw](
    byteCapacity: Int, descriptor: Descriptor[_, R]
  ) :(ByteBuffer, R#Buffer) = {
    genBuffer(byteCapacity, descriptor, true)
  }

  private def rand[T](m: ClassManifest[T]) :T = {
    (m match {
      case MetaManifest.SInt => ni
      case Vec2i.Manifest => Vec2i(ni, ni)
      case Vec3i.Manifest => Vec3i(ni, ni, ni)
      case Vec4i.Manifest => Vec4i(ni, ni, ni, ni)
      case MetaManifest.RFloat => nf
      case Vec2f.Manifest => Vec2f(nf, nf)
      case Vec3f.Manifest => Vec3f(nf, nf, nf)
      case Vec4f.Manifest => Vec4f(nf, nf, nf, nf)
      case MetaManifest.RDouble => nd
      case Vec2d.Manifest => Vec2d(nd, nd)
      case Vec3d.Manifest => Vec3d(nd, nd, nd)
      case Vec4d.Manifest => Vec4d(nd, nd, nd, nd)
    }).asInstanceOf[T]
  }
  private def randPrim[T](m: ClassManifest[T]) :T = {
    (m match {
      case MetaManifest.SInt => ni.asInstanceOf[AnyRef]
      case MetaManifest.RFloat => nf.asInstanceOf[AnyRef]
      case MetaManifest.RDouble => nd.asInstanceOf[AnyRef]
    }).asInstanceOf[T]
  }
  private def mkPrimSeq[E <: Meta, R <: Raw](size: Int, descriptor: Descriptor[E, R]) = {
    (descriptor.componentManifest match {
      case MetaManifest.SInt =>
        descriptor.rawType match {
          case SByte => DataArray[SInt, SByte](size*descriptor.components)
          case UByte => DataArray[SInt, UByte](size*descriptor.components)
          case SShort => DataArray[SInt, SShort](size*descriptor.components)
          case UShort => DataArray[SInt, UShort](size*descriptor.components)
          case SInt => DataArray[SInt, SInt](size*descriptor.components)
          case UInt => DataArray[SInt, UInt](size*descriptor.components)
        }
      case MetaManifest.RFloat =>
        descriptor.rawType match {
          case SByte => DataArray[RFloat, SByte](size*descriptor.components)
          case UByte => DataArray[RFloat, UByte](size*descriptor.components)
          case SShort => DataArray[RFloat, SShort](size*descriptor.components)
          case UShort => DataArray[RFloat, UShort](size*descriptor.components)
          case SInt => DataArray[RFloat, SInt](size*descriptor.components)
          case UInt => DataArray[RFloat, UInt](size*descriptor.components)
          case HFloat => DataArray[RFloat, HFloat](size*descriptor.components)
          case RFloat => DataArray[RFloat, RFloat](size*descriptor.components)
        }
      case MetaManifest.RDouble =>
        descriptor.rawType match {
          case SByte => DataArray[RDouble, SByte](size*descriptor.components)
          case UByte => DataArray[RDouble, UByte](size*descriptor.components)
          case SShort => DataArray[RDouble, SShort](size*descriptor.components)
          case UShort => DataArray[RDouble, UShort](size*descriptor.components)
          case SInt => DataArray[RDouble, SInt](size*descriptor.components)
          case UInt => DataArray[RDouble, UInt](size*descriptor.components)
          case HFloat => DataArray[RDouble, HFloat](size*descriptor.components)
          case RFloat => DataArray[RDouble, RFloat](size*descriptor.components)
          case RDouble => DataArray[RDouble, RDouble](size*descriptor.components)
        }
    }).asInstanceOf[DataArray[E#Component, R]]
  }
  def genRandomCollection[E <: Meta, R <: Raw](
    size: Int, descriptor: Descriptor[E, R]
  ) :(Array[E#Read], Buffer) = {
    val array = descriptor.readManifest.newArray(size).asInstanceOf[Array[E#Read]]
    val seq = mkPrimSeq(size, descriptor)

    val seed = randomSrc.nextLong

    randomSrc.setSeed(seed)
    var i = 0; while (i < array.length) {
      array(i) = rand(descriptor.elemManifest).asInstanceOf[E#Read]
      i += 1
    }

    randomSrc.setSeed(seed)
    i = 0; while (i < seq.length) {
      seq(i) = randPrim(descriptor.componentManifest).asInstanceOf[E#Component#Read]
      i += 1
    }

    (array, seq.buffer)
  }

  private def RandomDataArray[E <: Meta, R <: Raw](size: Int)(
    implicit factory: Factory[E, R], descriptor: Descriptor[E, R]
  ) :DataArray[E, R] = {
    factory.mkDataArray(genRandomArray(size*descriptor.components, descriptor))
  }
  def genRandomSeq(size: Int) :Data[_ <: Meta] = {
    genRandomSeq(None, None, size)
  }
  def genRandomSeq(manifest: Option[ClassManifest[_]], rawType: Option[Int], size: Int) :Data[_ <: Meta] = {
    val m = manifest match {
      case Some(man) =>
        man
      case None =>
        randomSrc.nextInt(12) match {
          case 0 => MetaManifest.SInt
          case 1 => Vec2i.Manifest
          case 2 => Vec3i.Manifest
          case 3 => Vec4i.Manifest
          case 4 => MetaManifest.RFloat
          case 5 => Vec2f.Manifest
          case 6 => Vec3f.Manifest
          case 7 => Vec4f.Manifest
          case 8 => MetaManifest.RDouble
          case 9 => Vec2d.Manifest
          case 10 => Vec3d.Manifest
          case 11 => Vec4d.Manifest
        }
      }
    
    val max = m match {
      case MetaManifest.SInt | Vec2i.Manifest | Vec3i.Manifest | Vec4i.Manifest => 6
      case MetaManifest.RFloat | Vec2f.Manifest | Vec3f.Manifest | Vec4f.Manifest => 8
      case MetaManifest.RDouble | Vec2d.Manifest | Vec3d.Manifest | Vec4d.Manifest => 9
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
        case 6 => HFloat
        case 7 => RFloat
        case 8 => RDouble
      }
    }
      
      genRandomSeq(m, r, size)
  }
  
  def genRandomSeq(manifest: ClassManifest[_], rawType: Int, size: Int) :Data[_ <: Meta] = {
    manifest match {
      case MetaManifest.SInt => rawType match {
        case SByte => RandomDataArray[SInt, SByte](size)
        case UByte => RandomDataArray[SInt, UByte](size)
        case SShort => RandomDataArray[SInt, SShort](size)
        case UShort => RandomDataArray[SInt, UShort](size)
        case SInt => RandomDataArray[SInt, SInt](size)
        case UInt => RandomDataArray[SInt, UInt](size)
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
      
      case MetaManifest.RFloat => rawType match {
        case SByte => RandomDataArray[RFloat, SByte](size)
        case UByte => RandomDataArray[RFloat, UByte](size)
        case SShort => RandomDataArray[RFloat, SShort](size)
        case UShort => RandomDataArray[RFloat, UShort](size)
        case SInt => RandomDataArray[RFloat, SInt](size)
        case UInt => RandomDataArray[RFloat, UInt](size)
        case HFloat => RandomDataArray[RFloat, HFloat](size)
        case RFloat => RandomDataArray[RFloat, RFloat](size)
      }
      case Vec2f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec2f, SByte](size)
        case UByte => RandomDataArray[Vec2f, UByte](size)
        case SShort => RandomDataArray[Vec2f, SShort](size)
        case UShort => RandomDataArray[Vec2f, UShort](size)
        case SInt => RandomDataArray[Vec2f, SInt](size)
        case UInt => RandomDataArray[Vec2f, UInt](size)
        case HFloat => RandomDataArray[Vec2f, HFloat](size)
        case RFloat => RandomDataArray[Vec2f, RFloat](size)
      }
      case Vec3f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec3f, SByte](size)
        case UByte => RandomDataArray[Vec3f, UByte](size)
        case SShort => RandomDataArray[Vec3f, SShort](size)
        case UShort => RandomDataArray[Vec3f, UShort](size)
        case SInt => RandomDataArray[Vec3f, SInt](size)
        case UInt => RandomDataArray[Vec3f, UInt](size)
        case HFloat => RandomDataArray[Vec3f, HFloat](size)
        case RFloat => RandomDataArray[Vec3f, RFloat](size)
      }
      case Vec4f.Manifest => rawType match {
        case SByte => RandomDataArray[Vec4f, SByte](size)
        case UByte => RandomDataArray[Vec4f, UByte](size)
        case SShort => RandomDataArray[Vec4f, SShort](size)
        case UShort => RandomDataArray[Vec4f, UShort](size)
        case SInt => RandomDataArray[Vec4f, SInt](size)
        case UInt => RandomDataArray[Vec4f, UInt](size)
        case HFloat => RandomDataArray[Vec4f, HFloat](size)
        case RFloat => RandomDataArray[Vec4f, RFloat](size)
      }
      
      case MetaManifest.RDouble => rawType match {
        case SByte => RandomDataArray[RDouble, SByte](size)
        case UByte => RandomDataArray[RDouble, UByte](size)
        case SShort => RandomDataArray[RDouble, SShort](size)
        case UShort => RandomDataArray[RDouble, UShort](size)
        case SInt => RandomDataArray[RDouble, SInt](size)
        case UInt => RandomDataArray[RDouble, UInt](size)
        case HFloat => RandomDataArray[RDouble, HFloat](size)
        case RFloat => RandomDataArray[RDouble, RFloat](size)
        case RDouble => RandomDataArray[RDouble, RDouble](size)
      }
      case Vec2d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec2d, SByte](size)
        case UByte => RandomDataArray[Vec2d, UByte](size)
        case SShort => RandomDataArray[Vec2d, SShort](size)
        case UShort => RandomDataArray[Vec2d, UShort](size)
        case SInt => RandomDataArray[Vec2d, SInt](size)
        case UInt => RandomDataArray[Vec2d, UInt](size)
        case HFloat => RandomDataArray[Vec2d, HFloat](size)
        case RFloat => RandomDataArray[Vec2d, RFloat](size)
        case RDouble => RandomDataArray[Vec2d, RDouble](size)
      }
      case Vec3d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec3d, SByte](size)
        case UByte => RandomDataArray[Vec3d, UByte](size)
        case SShort => RandomDataArray[Vec3d, SShort](size)
        case UShort => RandomDataArray[Vec3d, UShort](size)
        case SInt => RandomDataArray[Vec3d, SInt](size)
        case UInt => RandomDataArray[Vec3d, UInt](size)
        case HFloat => RandomDataArray[Vec3d, HFloat](size)
        case RFloat => RandomDataArray[Vec3d, RFloat](size)
        case RDouble => RandomDataArray[Vec3d, RDouble](size)
      }
      case Vec4d.Manifest => rawType match {
        case SByte => RandomDataArray[Vec4d, SByte](size)
        case UByte => RandomDataArray[Vec4d, UByte](size)
        case SShort => RandomDataArray[Vec4d, SShort](size)
        case UShort => RandomDataArray[Vec4d, UShort](size)
        case SInt => RandomDataArray[Vec4d, SInt](size)
        case UInt => RandomDataArray[Vec4d, UInt](size)
        case HFloat => RandomDataArray[Vec4d, HFloat](size)
        case RFloat => RandomDataArray[Vec4d, RFloat](size)
        case RDouble => RandomDataArray[Vec4d, RDouble](size)
      }
    }
  }
  
  final def testContent(
    components: Int,
    dest: inData[_], destFirst: Int,
    src: inData[_], srcFirst: Int,
    count: Int
  ) {
    val d = dest.backing
    val s = src.backing
    
    d.elemManifest match {
      case MetaManifest.SInt =>
        testIntContent(
          components,
          d.asInstanceOf[ReadData[SInt]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[SInt]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
      case MetaManifest.RFloat =>
        testFloatContent(
          components,
          d.asInstanceOf[ReadData[RFloat]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[RFloat]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
      case MetaManifest.RDouble =>
        testDoubleContent(
          components,
          d.asInstanceOf[ReadData[RDouble]], dest.offset + destFirst*dest.stride, dest.stride,
          s.asInstanceOf[ReadData[RDouble]], src.offset + srcFirst*src.stride, src.stride,
          count
        )
    }
  }
  
  // Test that remaining memory not tested by testContent is unmodified.
  final def testTheRest(
    components: Int,
    dest: inData[_], destFirst: Int,
    original: inData[_],
    count: Int
  ) {
    if (dest.isInstanceOf[DataView[_, _]]) {
      val d = DataBuffer[SInt, SByte](dest.asInstanceOf[DataView[_, _]].backing)
      val o = DataBuffer[SInt, SByte](original.asInstanceOf[DataView[_, _]].backing)
      
      val byteOffset = dest.byteOffset + dest.byteStride*destFirst
      val byteSkip = components*dest.bytesPerRawComponent
      val byteLimit = max(0, byteOffset + dest.byteStride*count - (dest.byteStride - byteSkip))
      
      // Beggining
      var i = 0; while (i < byteOffset) {
        assert(d(i) == o(i))
        
        i += 1
      }
      
      // Pertially modified
      i = byteOffset; while (i <= byteLimit - dest.byteStride) {
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
    dest: inData[SInt], destFirst: Int, destStride: Int,
    src: inData[SInt], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.SInt)
    assert(src.elemManifest == MetaManifest.SInt)
    
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
    dest: inData[RFloat], destFirst: Int, destStride: Int,
    src: inData[RFloat], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.RFloat)
    assert(src.elemManifest == MetaManifest.RFloat)
    
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
    dest: inData[RDouble], destFirst: Int, destStride: Int,
    src: inData[RDouble], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.RDouble)
    assert(src.elemManifest == MetaManifest.RDouble)
    
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
  
  def convert(src: inData[_], rawType: Int) :Contiguous[_, Raw] = {
    val factory = genRandomSeq(src.backing.elemManifest, rawType, 0)
    val contiguousCopy = factory.mkDataArray(src.components*src.size)
    
    src.backing.elemManifest match {
      case MetaManifest.SInt =>
        putIntContent(
          src.components,
          contiguousCopy.asInstanceOf[Contiguous[SInt, Raw]],
          src.backing.asInstanceOf[ReadData[SInt]], src.offset, src.stride,
          src.size
        )
      case MetaManifest.RFloat =>
        putFloatContent(
          src.components,
          contiguousCopy.asInstanceOf[Contiguous[RFloat, Raw]],
          src.backing.asInstanceOf[ReadData[RFloat]], src.offset, src.stride,
          src.size
        )
      case MetaManifest.RDouble =>
        putDoubleContent(
          src.components,
          contiguousCopy.asInstanceOf[Contiguous[RDouble, Raw]],
          src.backing.asInstanceOf[ReadData[RDouble]], src.offset, src.stride,
          src.size
        )
    }
    
    contiguousCopy
  }
  
  private final def putIntContent(
    components: Int,
    dest: outContiguous[SInt, Raw],
    src: inData[SInt], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.SInt)
    assert(src.elemManifest == MetaManifest.SInt)
    
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
    dest: outContiguous[RFloat, Raw],
    src: inData[RFloat], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.RFloat)
    assert(src.elemManifest == MetaManifest.RFloat)
    
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
    dest: outContiguous[RDouble, Raw],
    src: inData[RDouble], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.elemManifest == MetaManifest.RDouble)
    assert(src.elemManifest == MetaManifest.RDouble)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        dest(components*i + j) = src(srcFirst + srcStride*i + j)
        
        j += 1
      }
      
      i += 1
    }
  }
}
