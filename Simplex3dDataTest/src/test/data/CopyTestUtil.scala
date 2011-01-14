/*
 * Simplex3d, DataTest package
 * Copyright (C) 2010-2011, Simplex3d Team
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

package test.data

import java.nio._
import org.scalatest._
import simplex3d.data._
import simplex3d.math.floatx.{functions => fx, _}
import simplex3d.math.doublex.{functions => dx, _}
import simplex3d.math.doublex.functions._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CopyTestUtil extends FunSuite {

  private val testSize = 7
  private val maxCopyOffset = 4

  private val maxByteStride = 8*12 //8 bytes for Double * 12 elements stride
  private val maxTrailingBytes = 8
  private[this] val cacheSize = 1 + testSize*maxByteStride + maxTrailingBytes
  private[this] val buffCache = Array(new Array[ByteBuffer](cacheSize), new Array[ByteBuffer](cacheSize))
  for (i <- 0 until cacheSize) {
    buffCache(0)(i) = ByteBuffer.allocateDirect(i)
    buffCache(1)(i) = ByteBuffer.allocateDirect(i)
  }
  
  def testCopy[E <: Meta, R <: Raw](
    factory: DataFactory[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    
    val dataArray = factory.mkDataArray(genArray(0, descriptor))
    val bytesPerComponent = dataArray.bytesPerComponent
    val components = dataArray.components
    val byteSize = testSize*components*bytesPerComponent

    // Test Array
    for (s <- testSize*components until testSize*components + components) {
      val array = genArray(s, descriptor)
      testCopying(factory.mkDataArray(array), descriptor)
    }

    // Test Buffer
    for (s <- byteSize until byteSize + components*bytesPerComponent) {
      val buffer = genRandomBuffer(s, descriptor)._1
      testCopying(factory.mkDataBuffer(buffer), descriptor)
    }

    // Test View
    for (
      stride <- components to components + 4;
      offset <- 0 until (stride - components);
      extra <- 0 to 1
    ) {
      val extraBytes = extra*bytesPerComponent
      val byteStride = stride*bytesPerComponent
      val buffer = genRandomBuffer(testSize*byteStride + extraBytes, descriptor)._1
      testCopying(factory.mkDataView(buffer, offset, stride), descriptor)
    }
  }

  private def testCopying[E <: Meta, R <: Raw](
    original: inData[E], descriptor: Descriptor[E, R]
  ) {
    copyAs(original, descriptor)
    putSeq(original)
    putPrimitive(original)
    putData(original)
  }

  private def copyAs[E <: Meta, R <: Raw](
    original: inData[E], descriptor: Descriptor[E, R]
  ) {
    val seq = dupSeq1(original)

    {
      val copy = seq.copyAsDataArray()
      assert(copy.byteCapacity == seq.components*seq.bytesPerComponent*seq.size)
      assert(copy.size == seq.size)
      testArray(copy, false, null)(descriptor)
      testContent(seq.components, copy, 0, original, 0, seq.size)
      assert(seq.readOnlyBuffer() == original.readOnlyBuffer())
    }

    {
      val copy = seq.copyAsDataBuffer()
      assert(copy.byteCapacity == seq.components*seq.bytesPerComponent*seq.size)
      assert(copy.size == seq.size)
      testBuffer(copy, false, null)(descriptor)
      testContent(seq.components, copy, 0, original, 0, seq.size)
      assert(seq.readOnlyBuffer() == original.readOnlyBuffer())
    }
  }

  private def putSeq[E <: Meta](original: inData[E]) {
    // Test exceptions. Destination and src must remain unchanged.
    {
      val src = genRandomSeq(original.elemManifest, original.rawType, original.size)
      val array = src.toArray(src.readManifest)
      
      {
        val dest = dupSeq1(original)
        
        val wrongSrc = wrongType(src)
        val wrongArray = wrongSrc.toArray(wrongSrc.readManifest)
        val wrongBackup = wrongArray.toList
        val wrongList = wrongArray.toList
        val wrongIndexedSeq = wrongArray.toIndexedSeq
        
        intercept[ClassCastException] {
          dest.put(0, wrongArray, 0, 1)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongArray, wrongBackup)
        
        intercept[ClassCastException] {
          dest.put(0, wrongList, 0, 1)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongList, wrongBackup)
        
        intercept[ClassCastException] {
          dest.put(0, wrongIndexedSeq, 0, 1)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongIndexedSeq, wrongBackup)
      }
      
      checkPutSeqExceptions(original, array)
      checkPutSeqExceptions(original, array.toList)
      checkPutSeqExceptions(original, array.toIndexedSeq)
    }

    for (size <- original.size - maxCopyOffset until original.size) {
      val src = genRandomSeq(original.elemManifest, original.rawType, size)
      val array = src.toArray(src.readManifest)

      checkPutSeq(original, src, array)
      checkPutSeq(original, src, array.toList)
      checkPutSeq(original, src, array.toIndexedSeq)
    }
  }
  private def checkPutSeqExceptions[E <: Meta](original: inData[E], collection: Seq[E#Read]) {
    assert(original.size == collection.size)
    val size = original.size
    
    val backupCollection = collection.toList
    val dest = dupSeq1(original)

    {
      val (index, first, count) = (0, 0, 1)
      intercept[ReadOnlyBufferException] {
        dest.asReadOnly().asInstanceOf[Data[E]].put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }
    
    {
      val (index, first, count) = (0, 0, -1)
      intercept[IllegalArgumentException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (0, 0, size + 1)
      intercept[BufferOverflowException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

  {
      val (index, first, count) = (0, 1, size)
      intercept[BufferUnderflowException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (-1, 0, 1)
      intercept[IndexOutOfBoundsException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (size + 1, 0, 1)
      intercept[IndexOutOfBoundsException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (size, 0, 1)
      intercept[BufferOverflowException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (0, -1, 1)
      intercept[IndexOutOfBoundsException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (0, size + 1, 1)
      intercept[IndexOutOfBoundsException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }

    {
      val (index, first, count) = (0, size, 1)
      intercept[BufferUnderflowException] {
        dest.put(index, collection, first, count)
      }
      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }
    
    // Corner cases
    {
      val (index, first, count) = (size, 0, 0)
      dest.put(index, collection, first, count)

      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }
    
    {
      val (index, first, count) = (0, size, 0)
      dest.put(index, collection, first, count)

      verify(dest.readOnlyBuffer, original.readOnlyBuffer)
      verify(collection, backupCollection)
    }
  }
  private def checkPutSeq[E <: Meta](original: inData[E], src: inData[E], collection: Seq[E#Read]) {
    val backupCollection = collection.toList
    
    for (
      index <- 0 to maxCopyOffset;
      first <- 0 to maxCopyOffset;
      count <- 0 to min(original.size - index, src.size - first)
    ) {
      val dest = dupSeq1(original)
      dest.put(index, collection, first, count)
      testContent(original.components, dest, index, src, first, count)
      testTheRest(original.components, dest, index, original, count)
      verify(collection, backupCollection)
    }
  }
  private def verify[T](collection: Seq[T], backupCollection: List[T]) {
    for ((u, v) <- collection zip backupCollection) {
      (u, v) match {
        case (a: Float, b: Float) => if (fx.isnan(a)) assert(fx.isnan(b)) else assert(a == b)
        case (a: Double, b: Double) => if (dx.isnan(a)) assert(dx.isnan(b)) else assert(a == b)
        case (a: Int, b: Int) => assert(a == b)
        case (a: AnyRef, b: AnyRef) => assert(a eq b)
      }
    }
  }


  private def putPrimitive[E <: Meta](original: inData[E]) {
    val size = original.size

    // Test exceptions. Destination and src must remain unchanged.
    {
      val random = genRandomSeq(original.elemManifest, original.rawType, size)
      val src = random.asInstanceOf[ReadData[E]].primitive
      val dest = dupSeq1(original)

      {
        val wrongSrc = wrongType(src).asInstanceOf[ReadContiguous[E#Component, Raw]]
        val wrongSrcBackup = dupSeq2(wrongSrc)
        
        intercept[ClassCastException] {
          dest.put(0, wrongSrc, 0, dest.components, 1)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongSrc.readOnlyBuffer, wrongSrcBackup.readOnlyBuffer)
      }


      val srcBackup = dupSeq2(src)
      
      {
        val (index, first, stride, count) = (0, 0, dest.components, 1)
        intercept[ReadOnlyBufferException] {
          dest.asReadOnly().asInstanceOf[Data[E]].put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, 0, dest.components, -1)
        intercept[IllegalArgumentException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, stride, count) = (0, 0, dest.components, size + 1)
        intercept[BufferOverflowException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, 1, dest.components, size)
        intercept[BufferUnderflowException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, 0, 0, size)
        intercept[IllegalArgumentException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, 0, -1, size)
        intercept[IllegalArgumentException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, stride, count) = (-1, 0, dest.components, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, stride, count) = (size + 1, 0, dest.components, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (size, 0, dest.components, 1)
        intercept[BufferOverflowException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, stride, count) = (0, -1, dest.components, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, stride, count) = (0, src.size + 1, dest.components, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, src.size, dest.components, 1)
        intercept[BufferUnderflowException] {
          dest.put(index, src, first, stride, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      //Corener cases
      {
        val (index, first, stride, count) = (size, 0, dest.components, 0)
        dest.put(index, src, first, stride, count)

        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
      
      {
        val (index, first, stride, count) = (0, src.size, dest.components, 0)
        dest.put(index, src, first, stride, count)

        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
    }


    val psize = original.size*original.components
    val poffset = maxCopyOffset*original.components
    
    for (size <- psize - poffset until psize; conversion <- List(true, false)) {
      val src =
        if (conversion) genRandomSeq(original.primitive.elemManifest, original.rawType, size)
        else genRandomSeq(
          original.primitive.elemManifest,
          conversionType(original.primitive.elemManifest, original.rawType),
          size
        )

      val srcBackup = dupSeq2(src)

      val converted =
        if (conversion) src
        else convert(src, original.rawType)
    
      for (
        index <- 0 to maxCopyOffset;
        first <- 0 to maxCopyOffset;
        stride <- 1 until 2*original.components;
        count <- 0 to min(original.size - index, (src.size - first - original.components + 1)/stride)
      ) {
        val dest = dupSeq1(original)
        dest.put(index, src, first, stride, count)
        testContent(original.components, dest, index, converted, first, stride, count)
        testTheRest(original.components, dest, index, original, count)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
    }
  }

  private def putData[E <: Meta](original: inData[E]) {
    val size = original.size

    // Test exceptions. Destination and src must remain unchanged.
    {
      val src = genRandomSeq(original.elemManifest, original.rawType, size)
      val dest = dupSeq1(original)

      {
        val wrongSrc = wrongType(src)
        val wrongSrcBackup = dupSeq2(wrongSrc)

        intercept[ClassCastException] {
          dest.put(0, wrongSrc, 0, 1)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongSrc.readOnlyBuffer, wrongSrcBackup.readOnlyBuffer)

        intercept[ClassCastException] {
          dest.put(0, wrongSrc)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongSrc.readOnlyBuffer, wrongSrcBackup.readOnlyBuffer)

        intercept[ClassCastException] {
          dest.put(wrongSrc)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(wrongSrc.readOnlyBuffer, wrongSrcBackup.readOnlyBuffer)
      }


      val srcBackup = dupSeq2(src)

      {
        val (index, first, count) = (0, 0, 1)
        intercept[ReadOnlyBufferException] {
          dest.asReadOnly().asInstanceOf[Data[E]].put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, 0, -1)
        intercept[IllegalArgumentException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, 0, size + 1)
        intercept[BufferOverflowException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, 1, size)
        intercept[BufferUnderflowException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (-1, 0, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (size + 1, 0, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (size, 0, 1)
        intercept[BufferOverflowException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, -1, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, src.size + 1, 1)
        intercept[IndexOutOfBoundsException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, src.size, 1)
        intercept[BufferUnderflowException] {
          dest.put(index, src, first, count)
        }
        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      //Corener cases
      {
        val (index, first, count) = (size, 0, 0)
        dest.put(index, src, first, count)

        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }

      {
        val (index, first, count) = (0, src.size, 0)
        dest.put(index, src, first, count)

        verify(dest.readOnlyBuffer, original.readOnlyBuffer)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
    }


    for (size <- original.size - maxCopyOffset until original.size; conversion <- List(true, false)) {
      val src =
        if (conversion) genRandomSeq(original.elemManifest, original.rawType, size)
        else genRandomSeq(original.elemManifest, conversionType(original.elemManifest, original.rawType), size)

      val srcBackup = dupSeq2(src)

      val converted =
        if (conversion) src
        else convert(src, original.rawType)

      for (
        index <- 0 to maxCopyOffset;
        first <- 0 to maxCopyOffset;
        count <- 0 to min(original.size - index, src.size - first)
      ) {
        val dest = dupSeq1(original)
        dest.put(index, src, first, count)
        testContent(original.components, dest, index, converted, first, count)
        testTheRest(original.components, dest, index, original, count)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
    }
  }


  private def wrongType[E <: Meta](s: inData[E]) :Data[E] = {
    if (s.primitive.elemManifest == MetaManifest.SInt) {
      genRandomSeq(MetaManifest.RFloat, RawType.RFloat, s.size).asInstanceOf[Data[E]]
    }
    else {
      genRandomSeq(MetaManifest.SInt, RawType.SInt, s.size).asInstanceOf[Data[E]]
    }
  }
  private def conversionType(elem: ClassManifest[_], rawType: Int) :Int = {
    import RawType._
    elem match {
      case Mat2x3f.Manifest => RFloat
      case Mat2x3d.Manifest => rawType match {
        case RFloat => RDouble
        case RDouble => RFloat
      }
      case _ => rawType match {
        case SByte => UShort
        case UByte => UShort
        case SShort => UShort
        case UShort => SByte
        case SInt => UShort
        case UInt => UShort
        case HFloat => RFloat
        case RFloat => SShort
        case RDouble => SShort
      }
    }
    
  }
  private def verify(a: Buffer, b: Buffer) {
    assert(a.position == 0)
    assert(a.limit == a.capacity)
    
    assert(b.position == 0)
    assert(b.limit == b.capacity)
    
    assert(a == b)
  }

  private def dupArray[T](array: T) :T = {
    (array match {
      case a: Array[Byte] => a.clone()
      case a: Array[Short] => a.clone()
      case a: Array[Char] => a.clone()
      case a: Array[Int] => a.clone()
      case a: Array[Float] => a.clone()
      case a: Array[Double] => a.clone()
    }).asInstanceOf[T]
  }
  private def dupBuffer(buffer: ByteBuffer) :ByteBuffer = {
    val nb = ByteBuffer.allocateDirect(buffer.capacity)
    buffer.rewind
    buffer.limit(buffer.capacity)
    nb.put(buffer)
    nb
  }

  private[this] def dupSeq1[E <: Meta](seq: inData[E]) :Data[E] = dupSeqN(0, seq)
  private[this] def dupSeq2[E <: Meta](seq: inData[E]) :Data[E] = dupSeqN(1, seq)
  private[this] def dupSeqN[E <: Meta](id: Int, seq: inData[E]) :Data[E] = {
    (seq match {
      case s: DataArray[_, _] =>
        val array = s.array match {
          case a: Array[Byte] => a.clone
          case a: Array[Short] => a.clone
          case a: Array[Char] => a.clone
          case a: Array[Int] => a.clone
          case a: Array[Float] => a.clone
          case a: Array[Double] => a.clone
        }
        seq.mkDataArray(array.asInstanceOf[Raw#Array])
      case s: DataBuffer[_, _] =>
        val buff = buffCache(id)(s.byteCapacity)
        buff.rewind
        val copy = s.mkDataBuffer(buff)
        buff.put(s.rawBuffer)
        copy
      case s: DataView[_, _] =>
        val buff = buffCache(id)(s.byteCapacity)
        buff.rewind
        val copy = s.mkDataView(buff, s.offset, s.stride)
        buff.put(s.rawBuffer)
        copy
    }).asInstanceOf[Data[E]]
  }
}
