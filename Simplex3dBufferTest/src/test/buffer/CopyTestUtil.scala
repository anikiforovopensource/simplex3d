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

import java.nio._
import org.scalatest._
import simplex3d.buffer._
import simplex3d.math.floatm.FloatMath
import simplex3d.math.doublem.DoubleMath
import simplex3d.math.CoreMath._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object CopyTestUtil extends FunSuite {

  private val size = 20
  private val maxCopyOffset = 5
  private val maxStride = 8*8
  private val maxTrailingBytes = 8
  private[this] val cacheSize = size*maxStride + maxTrailingBytes
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
    val byteSize = size*components*bytesPerComponent

    // Test Array
    for (s <- size*components until size*components + components) {
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
      stride <- components until components + 4; 
      offset <- 0 until (stride - components);
      extraBytes <- 0 until bytesPerComponent
    ) {
      val buffer = genRandomBuffer(offset + size*stride*bytesPerComponent + extraBytes, descriptor)._1
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
      val src = genRandomSeq(original.elemManifest, original.rawType, size).asInstanceOf[ReadData[E]]
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
      val src = genRandomSeq(original.elemManifest, original.rawType, size).asInstanceOf[ReadData[E]]
      val array = src.toArray(src.readManifest)

      checkPutSeq(original, src, array)
      checkPutSeq(original, src, array.toList)
      checkPutSeq(original, src, array.toIndexedSeq)
    }
  }
  private def checkPutSeqExceptions[E <: Meta](original: inData[E], collection: Seq[E#Read]) {
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
      index <- 0 until maxCopyOffset;
      first <- 0 until maxCopyOffset;
      count <- 0 until min(original.size - index, src.size - first)
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
        case (a: Float, b: Float) => if (FloatMath.isnan(a)) assert(FloatMath.isnan(b)) else assert(a == b)
        case (a: Double, b: Double) => if (DoubleMath.isnan(a)) assert(DoubleMath.isnan(b)) else assert(a == b)
        case (a: Int, b: Int) => assert(a == b)
        case (a: AnyRef, b: AnyRef) => assert(a eq b)
      }
    }
  }


  private def putPrimitive[E <: Meta](original: inData[E]) {
    // Test exceptions. Destination and src must remain unchanged.
    {
      val src = genRandomSeq(original.elemManifest, original.rawType, size).asInstanceOf[ReadData[E]].backing
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
    
    for (size <- psize - poffset until psize) {
      val random = genRandomSeq(original.backing.elemManifest, original.rawType, size)
      val src = random.asInstanceOf[ReadContiguous[E#Component, Raw]]
      val srcBackup = dupSeq2(src)
    
      for (
        index <- 0 until maxCopyOffset;
        first <- 0 until maxCopyOffset;
        stride <- 1 until 2*original.components;
        count <- 0 until min(original.size - index, (src.size - first)/stride)
      ) {
        val dest = dupSeq1(original)
        dest.put(index, src, first, stride, count)
        testContent(original.components, dest, index, src, first, stride, count)
        testTheRest(original.components, dest, index, original, count)
        verify(src.readOnlyBuffer, srcBackup.readOnlyBuffer)
      }
    }
  }

  private def putData(original: inData[_]) {
    // generate randomSeq and test put(data)
  }


  private def wrongType[E <: Meta](s: inData[E]) :Data[E] = {
    if (s.backing.elemManifest == MetaManifest.SInt) {
      genRandomSeq(MetaManifest.RFloat, RawType.RFloat, s.size).asInstanceOf[Data[E]]
    }
    else {
      genRandomSeq(MetaManifest.SInt, RawType.SInt, s.size).asInstanceOf[Data[E]]
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

/*
// Test Copy
  put(index: Int, seq: Seq[E#Element], first: Int, count: Int)
  put(index: Int, seq: Seq[E#Element])
  put(seq: Seq[E#Element])

  put(index: Int, src: inContiguous[E#Component, _], srcOffset: Int, srcStride: Int, count: Int)
  put(index: Int, src: inContiguous[E#Component, _])
  put(src: inContiguous[E#Component, _])

  put(index: Int, src: inDataSeq[E, _], first: Int, count: Int)
  put(index: Int, src: inDataSeq[E, _])
  put(src: inDataSeq[E, _])
*/
