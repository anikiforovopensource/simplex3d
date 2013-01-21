/*
 * Simplex3dData - Test Package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dDataTest.
 *
 * Simplex3dDataTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dDataTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.test.data

import org.scalatest._

import java.nio._
import scala.reflect._
import scala.reflect.runtime.universe._
import simplex3d.data._
import simplex3d.data.RawEnum._
import simplex3d.data._
import simplex3d.data.float._
import simplex3d.data.double._
import simplex3d.data.extension._
import simplex3d.math._
import simplex3d.math.floatx._
import simplex3d.math.doublex._
import simplex3d.math.doublex.functions._

import Descriptors._


/**
 * @author Aleksey Nikiforov (lex)
 */
object TestUtil extends FunSuite {

  private val randomSrc = new java.util.Random(0)


  private def alloc(size: Int) = {
    ByteBuffer.allocateDirect(size).order(ByteOrder.nativeOrder)
  }
  
  def dataSeqSize(capacity: Int, offset: Int, stride: Int, components: Int) = {
    val s = (capacity - offset + stride - components)/stride
    if (s > 0) s else 0
  }

  def isUnsigned(rawEnum: Int) :Boolean = {
    rawEnum match {
      case UByte => true
      case UShort => true
      case UInt => true
      case _ => false
    }
  }

  def wrap(bytes: ByteBuffer, descriptor: Descriptor[_, _]) :Buffer = {
    descriptor.rawEnum match {
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
      seq.bindingBufferSubData(-1, seq.size)
    }

    intercept[IllegalArgumentException] {
      seq.bindingBufferSubData(seq.size, seq.size)
    }

    intercept[IllegalArgumentException] {
      seq.bindingBufferSubData(0, -1)
    }

    intercept[IllegalArgumentException] {
      seq.bindingBufferSubData(0, seq.size + 1)
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
    (descriptor.rawEnum match {
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
    (descriptor.rawEnum match {
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

  private def rand[T](randomSrc: java.util.Random, m: ClassTag[T]) :T = {
    def ni = randomSrc.nextInt
    def nf = randomSrc.nextFloat
    def nd = randomSrc.nextDouble

    (m match {
      case PrimitiveFormat.SInt => ni
      case Vec2i.Tag => Vec2i(ni, ni)
      case Vec3i.Tag => Vec3i(ni, ni, ni)
      case Vec4i.Tag => Vec4i(ni, ni, ni, ni)
      case PrimitiveFormat.RFloat => nf
      case Vec2f.Tag => Vec2f(nf, nf)
      case Vec3f.Tag => Vec3f(nf, nf, nf)
      case Vec4f.Tag => Vec4f(nf, nf, nf, nf)
      case Mat3x2f.Tag => Mat3x2f(nf, nf, nf, nf, nf, nf)
      case PrimitiveFormat.RDouble => nd
      case Vec2d.Tag => Vec2d(nd, nd)
      case Vec3d.Tag => Vec3d(nd, nd, nd)
      case Vec4d.Tag => Vec4d(nd, nd, nd, nd)
      case Mat3x2d.Tag => Mat3x2d(nd, nd, nd, nd, nd, nd)
    }).asInstanceOf[T]
  }
  private def randPrim[T](randomSrc: java.util.Random, m: ClassTag[T]) :T = {
    def ni = randomSrc.nextInt
    def nf = randomSrc.nextFloat
    def nd = randomSrc.nextDouble

    (m match {
      case PrimitiveFormat.SInt => ni.asInstanceOf[AnyRef]
      case PrimitiveFormat.RFloat => nf.asInstanceOf[AnyRef]
      case PrimitiveFormat.RDouble => nd.asInstanceOf[AnyRef]
    }).asInstanceOf[T]
  }

  private def mkPrimSeq[F <: Format, R <: Raw](size: Int, descriptor: Descriptor[F, R]) = {
    (descriptor.componentTag match {
      case PrimitiveFormat.SInt =>
        descriptor.rawEnum match {
          case SByte => DataArray[SInt, SByte](size*descriptor.components)
          case UByte => DataArray[SInt, UByte](size*descriptor.components)
          case SShort => DataArray[SInt, SShort](size*descriptor.components)
          case UShort => DataArray[SInt, UShort](size*descriptor.components)
          case SInt => DataArray[SInt, SInt](size*descriptor.components)
          case UInt => DataArray[SInt, UInt](size*descriptor.components)
        }
      case PrimitiveFormat.RFloat =>
        descriptor.rawEnum match {
          case SByte => DataArray[RFloat, SByte](size*descriptor.components)
          case UByte => DataArray[RFloat, UByte](size*descriptor.components)
          case SShort => DataArray[RFloat, SShort](size*descriptor.components)
          case UShort => DataArray[RFloat, UShort](size*descriptor.components)
          case SInt => DataArray[RFloat, SInt](size*descriptor.components)
          case UInt => DataArray[RFloat, UInt](size*descriptor.components)
          case HFloat => DataArray[RFloat, HFloat](size*descriptor.components)
          case RFloat => DataArray[RFloat, RFloat](size*descriptor.components)
        }
      case PrimitiveFormat.RDouble =>
        descriptor.rawEnum match {
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
    }).asInstanceOf[DataArray[F#Component, R]]
  }
  def genRandomCollection[F <: Format, R <: Raw](
    size: Int, descriptor: Descriptor[F, R]
  ) :(Array[F#Accessor#Read], Buffer) = {
    val array = readTag(descriptor.accessorTag).newArray(size).asInstanceOf[Array[F#Accessor#Read]]
    val seq = mkPrimSeq(size, descriptor)

    val seed = randomSrc.nextLong
    val localSrc = new java.util.Random(seed)

    var i = 0; while (i < array.length) {
      array(i) = rand(localSrc, descriptor.formatTag).asInstanceOf[F#Accessor#Read]
      i += 1
    }

    localSrc.setSeed(seed)
    i = 0; while (i < seq.length) {
      seq(i) = randPrim(localSrc, descriptor.componentTag).asInstanceOf[F#Component#Accessor#Read]
      i += 1
    }

    (array, seq.buffer)
  }

  private def RandomDataArray[F <: Format, R <: Raw](size: Int)(
    implicit composition: CompositionFactory[F, _ >: R],
    primitives: DataFactory[F#Component, R],
    descriptor: Descriptor[F, R]
  ) :DataArray[F, R] = {
    composition.mkDataArray(primitives.mkDataArray(genRandomArray(size*descriptor.components, descriptor)))
  }
  def genRandomSeq(size: Int) :DataSeq[_ <: Format, Raw] = {
    genRandomSeq(None, None, size)
  }
  def genRandomSeq(
    manifest: Option[ClassTag[_ <: Format]], rawEnum: Option[Int], size: Int
  ) :Contiguous[_ <: Format, Raw] = {
    val m = manifest match {
      case Some(man) =>
        man
      case None =>
        randomSrc.nextInt(14) match {
          case 0 => PrimitiveFormat.SInt
          case 1 => Vec2i.Tag
          case 2 => Vec3i.Tag
          case 3 => Vec4i.Tag
          case 4 => PrimitiveFormat.RFloat
          case 5 => Vec2f.Tag
          case 6 => Vec3f.Tag
          case 7 => Vec4f.Tag
          case 8 => Mat3x2f.Tag
          case 9 => PrimitiveFormat.RDouble
          case 10 => Vec2d.Tag
          case 11 => Vec3d.Tag
          case 12 => Vec4d.Tag
          case 13 => Mat3x2d.Tag
        }
      }
    
    val (min, max) = m match {
      case PrimitiveFormat.SInt | Vec2i.Tag | Vec3i.Tag | Vec4i.Tag => (0, 6)
      case PrimitiveFormat.RFloat | Vec2f.Tag | Vec3f.Tag | Vec4f.Tag => (0, 8)
      case PrimitiveFormat.RDouble | Vec2d.Tag | Vec3d.Tag | Vec4d.Tag => (0, 9)
      case Mat3x2f.Tag => (7, 8)
      case Mat3x2d.Tag => (7, 9)
    }
    
    val r = rawEnum match {
      case Some(i) => assert(i < max); i
      case None => min + randomSrc.nextInt(max - min) match {
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
  
  def genRandomSeq[F <: Format](manifest: ClassTag[F], rawEnum: Int, size: Int) :Contiguous[F, Raw] = {
    (manifest match {
      case PrimitiveFormat.SInt => rawEnum match {
        case SByte => RandomDataArray[SInt, SByte](size)
        case UByte => RandomDataArray[SInt, UByte](size)
        case SShort => RandomDataArray[SInt, SShort](size)
        case UShort => RandomDataArray[SInt, UShort](size)
        case SInt => RandomDataArray[SInt, SInt](size)
        case UInt => RandomDataArray[SInt, UInt](size)
      }
      case Vec2i.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec2i, SByte](size)
        case UByte => RandomDataArray[Vec2i, UByte](size)
        case SShort => RandomDataArray[Vec2i, SShort](size)
        case UShort => RandomDataArray[Vec2i, UShort](size)
        case SInt => RandomDataArray[Vec2i, SInt](size)
        case UInt => RandomDataArray[Vec2i, UInt](size)
      }
      case Vec3i.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec3i, SByte](size)
        case UByte => RandomDataArray[Vec3i, UByte](size)
        case SShort => RandomDataArray[Vec3i, SShort](size)
        case UShort => RandomDataArray[Vec3i, UShort](size)
        case SInt => RandomDataArray[Vec3i, SInt](size)
        case UInt => RandomDataArray[Vec3i, UInt](size)
      }
      case Vec4i.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec4i, SByte](size)
        case UByte => RandomDataArray[Vec4i, UByte](size)
        case SShort => RandomDataArray[Vec4i, SShort](size)
        case UShort => RandomDataArray[Vec4i, UShort](size)
        case SInt => RandomDataArray[Vec4i, SInt](size)
        case UInt => RandomDataArray[Vec4i, UInt](size)
      }
      
      case PrimitiveFormat.RFloat => rawEnum match {
        case SByte => RandomDataArray[RFloat, SByte](size)
        case UByte => RandomDataArray[RFloat, UByte](size)
        case SShort => RandomDataArray[RFloat, SShort](size)
        case UShort => RandomDataArray[RFloat, UShort](size)
        case SInt => RandomDataArray[RFloat, SInt](size)
        case UInt => RandomDataArray[RFloat, UInt](size)
        case HFloat => RandomDataArray[RFloat, HFloat](size)
        case RFloat => RandomDataArray[RFloat, RFloat](size)
      }
      case Vec2f.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec2f, SByte](size)
        case UByte => RandomDataArray[Vec2f, UByte](size)
        case SShort => RandomDataArray[Vec2f, SShort](size)
        case UShort => RandomDataArray[Vec2f, UShort](size)
        case SInt => RandomDataArray[Vec2f, SInt](size)
        case UInt => RandomDataArray[Vec2f, UInt](size)
        case HFloat => RandomDataArray[Vec2f, HFloat](size)
        case RFloat => RandomDataArray[Vec2f, RFloat](size)
      }
      case Vec3f.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec3f, SByte](size)
        case UByte => RandomDataArray[Vec3f, UByte](size)
        case SShort => RandomDataArray[Vec3f, SShort](size)
        case UShort => RandomDataArray[Vec3f, UShort](size)
        case SInt => RandomDataArray[Vec3f, SInt](size)
        case UInt => RandomDataArray[Vec3f, UInt](size)
        case HFloat => RandomDataArray[Vec3f, HFloat](size)
        case RFloat => RandomDataArray[Vec3f, RFloat](size)
      }
      case Vec4f.Tag => rawEnum match {
        case SByte => RandomDataArray[Vec4f, SByte](size)
        case UByte => RandomDataArray[Vec4f, UByte](size)
        case SShort => RandomDataArray[Vec4f, SShort](size)
        case UShort => RandomDataArray[Vec4f, UShort](size)
        case SInt => RandomDataArray[Vec4f, SInt](size)
        case UInt => RandomDataArray[Vec4f, UInt](size)
        case HFloat => RandomDataArray[Vec4f, HFloat](size)
        case RFloat => RandomDataArray[Vec4f, RFloat](size)
      }
      case Mat3x2f.Tag => rawEnum match {
        case RFloat => RandomDataArray[Mat3x2f, RFloat](size)
      }
      
      case PrimitiveFormat.RDouble => rawEnum match {
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
      case Vec2d.Tag => rawEnum match {
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
      case Vec3d.Tag => rawEnum match {
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
      case Vec4d.Tag => rawEnum match {
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
      case Mat3x2d.Tag => rawEnum match {
        case RFloat => RandomDataArray[Mat3x2d, RFloat](size)
        case RDouble => RandomDataArray[Mat3x2d, RDouble](size)
      }
    }).asInstanceOf[Contiguous[F, Raw]]
  }
  
  final def testContent[F <: Format](
    components: Int,
    dest: inDataSeq[F, Raw], destFirst: Int,
    src: inDataSeq[F, Raw], srcFirst: Int,
    count: Int
  ) {
    testContent(
      components,
      dest, destFirst,
      src.primitives, src.offset + srcFirst*src.stride, src.stride,
      count
    )
  }
  final def testContent[F <: Format](
    components: Int,
    dest: inDataSeq[F, Raw], destFirst: Int,
    src: inDataSeq[F#Component, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    val d = dest.primitives
    
    d.formatTag match {
      case PrimitiveFormat.SInt =>
        testIntContent(
          components,
          d.asInstanceOf[ReadDataSeq[SInt, Raw]], dest.offset + destFirst*dest.stride, dest.stride,
          src.asInstanceOf[ReadDataSeq[SInt, Raw]], srcFirst, srcStride,
          count
        )
      case PrimitiveFormat.RFloat =>
        testFloatContent(
          components,
          d.asInstanceOf[ReadDataSeq[RFloat, Raw]], dest.offset + destFirst*dest.stride, dest.stride,
          src.asInstanceOf[ReadDataSeq[RFloat, Raw]], srcFirst, srcStride,
          count
        )
      case PrimitiveFormat.RDouble =>
        testDoubleContent(
          components,
          d.asInstanceOf[ReadDataSeq[RDouble, Raw]], dest.offset + destFirst*dest.stride, dest.stride,
          src.asInstanceOf[ReadDataSeq[RDouble, Raw]], srcFirst, srcStride,
          count
        )
    }
  }
  
  // Test that remaining memory not tested by testContent is unmodified.
  final def testTheRest[F <: Format](
    components: Int,
    dest: inDataSeq[F, Raw], destFirst: Int,
    original: inDataSeq[F, Raw],
    count: Int
  ) {
    if (dest.isInstanceOf[DataView[_, _]]) {
      val d = DataBuffer[SInt, SByte](dest.asInstanceOf[DataView[F, Raw]].primitives)
      val o = DataBuffer[SInt, SByte](original.asInstanceOf[DataView[F, Raw]].primitives)
      
      val byteOffset = dest.byteOffset + dest.byteStride*destFirst
      val byteSkip = components*dest.bytesPerComponent
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
    dest: inDataSeq[SInt, Raw], destFirst: Int, destStride: Int,
    src: inDataSeq[SInt, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.SInt)
    assert(src.formatTag == PrimitiveFormat.SInt)
    
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
    dest: inDataSeq[RFloat, Raw], destFirst: Int, destStride: Int,
    src: inDataSeq[RFloat, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.RFloat)
    assert(src.formatTag == PrimitiveFormat.RFloat)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        val a = dest(destFirst + destStride*i + j)
        val b = src(srcFirst + srcStride*i + j)
        if (floatx.functions.isnan(a)) assert(floatx.functions.isnan(b))
        else assert(a == b)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  private final def testDoubleContent(
    components: Int,
    dest: inDataSeq[RDouble, Raw], destFirst: Int, destStride: Int,
    src: inDataSeq[RDouble, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.RDouble)
    assert(src.formatTag == PrimitiveFormat.RDouble)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        val a = dest(destFirst + destStride*i + j)
        val b = src(srcFirst + srcStride*i + j)
        if (doublex.functions.isnan(a)) assert(doublex.functions.isnan(b))
        else assert(a == b)
        
        j += 1
      }
      
      i += 1
    }
  }
  
  def convert[F <: Format](src: inDataSeq[F, Raw], rawEnum: Int) :Contiguous[F, Raw] = {
    val factory = genRandomSeq(src.formatTag, rawEnum, 0)
    val contiguousCopy = factory.mkDataArray(src.components*src.size)
    
    src.primitives.formatTag match {
      case PrimitiveFormat.SInt =>
        putIntContent(
          src.components,
          contiguousCopy.primitives.asInstanceOf[Contiguous[SInt, Raw]],
          src.primitives.asInstanceOf[ReadContiguous[SInt, Raw]], src.offset*src.stride, src.stride,
          src.size
        )
      case PrimitiveFormat.RFloat =>
        putFloatContent(
          src.components,
          contiguousCopy.primitives.asInstanceOf[Contiguous[RFloat, Raw]],
          src.primitives.asInstanceOf[ReadContiguous[RFloat, Raw]], src.offset*src.stride, src.stride,
          src.size
        )
      case PrimitiveFormat.RDouble =>
        putDoubleContent(
          src.components,
          contiguousCopy.primitives.asInstanceOf[Contiguous[RDouble, Raw]],
          src.primitives.asInstanceOf[ReadContiguous[RDouble, Raw]], src.offset*src.stride, src.stride,
          src.size
        )
    }
    
    contiguousCopy
  }
  
  private final def putIntContent(
    components: Int,
    dest: Contiguous[SInt, Raw],
    src: inContiguous[SInt, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.SInt)
    assert(src.formatTag == PrimitiveFormat.SInt)
    
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
    dest: Contiguous[RFloat, Raw],
    src: inContiguous[RFloat, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.RFloat)
    assert(src.formatTag == PrimitiveFormat.RFloat)
    
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
    dest: Contiguous[RDouble, Raw],
    src: inContiguous[RDouble, Raw], srcFirst: Int, srcStride: Int,
    count: Int
  ) {
    assert(dest.formatTag == PrimitiveFormat.RDouble)
    assert(src.formatTag == PrimitiveFormat.RDouble)
    
    var i = 0; while (i < count) {
      var j = 0; while (j < components) {
        dest(components*i + j) = src(srcFirst + srcStride*i + j)
        
        j += 1
      }
      
      i += 1
    }
  }

  final def supportsRawEnum(primitives: ClassTag[_], raw: TypeTag[_]) = {
    import RawEnum.TypeTags._

    primitives match {
      case RawEnum.ClassTags.SInt => raw match {
        case SByte | UByte | SShort | UShort | SInt | UInt => true
        case _ => false
      }
      case RawEnum.ClassTags.RFloat => raw match {
        case SByte | UByte | SShort | UShort | SInt | UInt | HFloat |RFloat => true
        case _ => false
      }
      case RawEnum.ClassTags.RDouble => true
    }
  }
  
  final def readTag[M <: Accessor](m: ClassTag[M]) :ClassTag[M#Read] = {
    (m match {
      case PrimitiveFormat.SInt => ClassTag.Int
      case PrimitiveFormat.RFloat => ClassTag.Float
      case PrimitiveFormat.RDouble => ClassTag.Double
      case Vec2i.Tag => Vec2i.ReadTag
      case Vec3i.Tag => Vec3i.ReadTag
      case Vec4i.Tag => Vec4i.ReadTag
      case Vec2f.Tag => Vec2f.ReadTag
      case Vec3f.Tag => Vec3f.ReadTag
      case Vec4f.Tag => Vec4f.ReadTag
      case Mat3x2f.Tag => Mat3x2f.ReadTag
      case Vec2d.Tag => Vec2d.ReadTag
      case Vec3d.Tag => Vec3d.ReadTag
      case Vec4d.Tag => Vec4d.ReadTag
      case Mat3x2d.Tag => Mat3x2d.ReadTag
    }).asInstanceOf[ClassTag[M#Read]]
  }
}
