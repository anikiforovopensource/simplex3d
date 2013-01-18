/*
 * Simplex3dData - Test Package
 * Copyright (C) 2011, Aleksey Nikiforov
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

import java.nio._
import scala.collection.mutable.ArrayBuffer
import org.scalatest._
import simplex3d.data._
import simplex3d.data.extension._
import simplex3d.math._
import simplex3d.math.double.functions._
import TestUtil._
import CopyTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object SortTestUtil extends FunSuite {
  
  private val size = 64
  private val random = new java.util.Random(123)
  private def randomMapping() :Array[Float] = {
    val array = new Array[Float](size)
    var i = 0; while (i < array.length) {
      array(i) = (random.nextFloat()*2 - 1)*1000
      i += 1
    }
    array
  }
  
  def testSort[F <: Format, R <: Raw](
    factory: DataFactory[F, R]
  )(implicit descriptor: Descriptor[F, R])
  {
    val maxStride = factory.components + 3
    val maxByteSize = size*maxStride*factory.bytesPerComponent
    val bytesPerElement = factory.components*factory.bytesPerComponent
    
    val directBuff = ByteBuffer.allocateDirect(maxByteSize).order(ByteOrder.nativeOrder)
    
    {
      val data = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
      val destCopy = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
      val dest = destCopy.copyAsDataArray()
      
      testSort(directBuff, data, dest, destCopy)
    }
    
    {
      val data = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
      val destCopy = factory.mkDataBuffer(genRandomBuffer(size*bytesPerElement, descriptor)._1).asReadOnly()
      val dest = destCopy.copyAsDataBuffer()
      
      testSort(directBuff, data, dest, destCopy)
    }
    
    {
      val data = factory.mkDataBuffer(genRandomBuffer(size*bytesPerElement, descriptor)._1).asReadOnly()
      val destCopy = factory.mkDataArray(genRandomArray(size*factory.components, descriptor)).asReadOnly()
      val dest = destCopy.copyAsDataArray()
      
      testSort(directBuff, data, dest, destCopy)
    }
    
    {
      val data = factory.mkDataBuffer(genRandomBuffer(size*bytesPerElement, descriptor)._1).asReadOnly()
      val destCopy = factory.mkDataBuffer(genRandomBuffer(size*bytesPerElement, descriptor)._1).asReadOnly()
      val dest = destCopy.copyAsDataBuffer()
      
      testSort(directBuff, data, dest, destCopy)
    }
    
    for (stride <- factory.components until maxStride; offset <- 0 until stride - factory.components) {
      val byteSize = size*stride*factory.bytesPerComponent
      val dataBuffer = genRandomBuffer(byteSize, Descriptors.SIntSByte)._1
      val destCopyBuffer = genRandomBuffer(byteSize, Descriptors.SIntSByte)._1
      val destBuffer = {
        val buff = genBuffer(byteSize, Descriptors.SIntSByte)._1
        buff.put(destCopyBuffer)
        buff.rewind()
        buff
      }
      
      val data = factory.mkDataView(dataBuffer, offset, stride).asReadOnly()
      val destCopy = factory.mkDataView(destCopyBuffer, offset, stride).asReadOnly()
      val dest = factory.mkDataView(destBuffer, offset, stride)
      
      testSort(directBuff, data, dest, destCopy)
    }
  }
  
  private def testSort[T <: Accessor](
    directBuff: ByteBuffer,
    data: inData[T],
    dest: Data[T], destCopy: inData[T]// dest must be filled with random data.
  ) {
    // Test exceptions, ensure that dest has not been written to.
    {
      intercept[IllegalArgumentException] {
        data.reorder(new DataSort(0), first = 0, stride = 1, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[IndexOutOfBoundsException] {
        data.reorder(new DataSort(1), first = -1, stride = 1, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[IndexOutOfBoundsException] {
        data.reorder(new DataSort(1), first = data.size + 1, stride = 1, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[BufferUnderflowException] {
        data.reorder(new DataSort(1), first = data.size, stride = 1, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[IllegalArgumentException] {
        data.reorder(new DataSort(1), first = 0, stride = 0, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[BufferUnderflowException] {
        data.reorder(new DataSort(1), first = 0, stride = data.size + 1, count = 1, dest = dest, destFirst = 0)
      }
      
      intercept[IllegalArgumentException] {
        data.reorder(new DataSort(1), first = 0, stride = 1, count = -1, dest = dest, destFirst = 0)
      }
      
      intercept[BufferUnderflowException] {
        data.reorder(new DataSort(1), first = 0, stride = 1, count = data.size + 1, dest = dest, destFirst = 0)
      }
      
      intercept[IndexOutOfBoundsException] {
        data.reorder(new DataSort(1), first = 0, stride = 1, count = 1, dest = dest, destFirst = -1)
      }
      
      intercept[IndexOutOfBoundsException] {
        data.reorder(new DataSort(1), first = 0, stride = 1, count = 1, dest = dest, destFirst = dest.size + 1)
      }
      
      intercept[BufferOverflowException] {
        data.reorder(new DataSort(1), first = 0, stride = 1, count = 1, dest = dest, destFirst = dest.size)
      }
      
      intercept[IllegalArgumentException] {
        val badOffset = if (data.offset == 0) 1 else 0
        val badDest = dest.asInstanceOf[DataSeq[_, _]].mkDataView(directBuff, badOffset, data.stride).asInstanceOf[Data[T]]
        data.reorder(new DataSort(1), first = 0, stride = 1, count = 1, dest = badDest, destFirst = dest.size)
      }
      
      intercept[IllegalArgumentException] {
        val badStride = if (data.stride == data.components) data.components + 1 else data.stride - 1
        val badDest = dest.asInstanceOf[DataSeq[_, _]].mkDataView(directBuff, data.offset, badStride).asInstanceOf[Data[T]]
        data.reorder(new DataSort(1), first = 0, stride = 1, count = 1, dest = badDest, destFirst = dest.size)
      }
      
      
      intercept[IllegalArgumentException] {
        data.reorder(new DataSort(1), stride = 0, dest = dest)
      }
      
      intercept[BufferUnderflowException] {
        data.reorder(new DataSort(1), stride = data.size + 1, dest = dest)
      }
      
      intercept[IllegalArgumentException] {
        val badOffset = if (data.offset == 0) 1 else 0
        val badDest = dest.asInstanceOf[DataSeq[_, _]].mkDataView(directBuff, badOffset, data.stride).asInstanceOf[Data[T]]
        data.reorder(new DataSort(1), stride = 1, dest = badDest)
      }
      
      intercept[IllegalArgumentException] {
        val badStride = if (data.stride == data.components) data.components + 1 else data.stride - 1
        val badDest = dest.asInstanceOf[DataSeq[_, _]].mkDataView(directBuff, data.offset, badStride).asInstanceOf[Data[T]]
        data.reorder(new DataSort(1), stride = 1, dest = badDest)
      }
      
      
      // Not tested exceptions on: rawType, formatTag, and sharesStorageWith()
    }
    assert(dest.bindingBuffer().equals(destCopy.bindingBuffer()))
    
    
    val mapping = randomMapping()
    val sortContext = new DataSort(1)
    
    
    def testFunction(first: Int, count: Int, stride: Int, destFirst: Int)(reorder: => Unit) {
      // Restore dest
      dest.bindingBuffer() match {
        case b: ByteBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[ByteBuffer])
        case b: CharBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[CharBuffer])
        case b: ShortBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[ShortBuffer])
        case b: IntBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[IntBuffer])
        case b: FloatBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[FloatBuffer])
        case b: DoubleBuffer => b.put(destCopy.bindingBuffer().asInstanceOf[DoubleBuffer])
      }
      
      // Sort
      var i = 0; while (i < count) {
        sortContext.map(i, mapping(i))
        i += 1
      }
      sortContext.sort(count)
      
      
      // Reorder
      reorder
      
      
      // Check unmodified parts
      val start = destFirst*data.stride
      val end = (destFirst + count*stride)*data.stride
      val destBuff = dest.buffer()
      
      val destCopyBuff = destCopy.readOnlyBuffer()
      
      destBuff.limit(start)
      destBuff.position(0)
      destCopyBuff.limit(start)
      destCopyBuff.position(0)
      assert(destBuff.equals(destCopyBuff))
      
      destBuff.limit(destBuff.capacity)
      destBuff.position(end)
      destCopyBuff.limit(destBuff.capacity)
      destCopyBuff.position(end)
      assert(destBuff.equals(destCopyBuff))
      
      
      // Verify modified part
      data.readOnlyBuffer() match {
        
        case _: ByteBuffer => def verify() {
          val col = new ArrayBuffer[(Float, ByteBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[ByteBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = ByteBuffer.wrap(new Array[Byte](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
        
        case _: CharBuffer => def verify() {
          val col = new ArrayBuffer[(Float, CharBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[CharBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = CharBuffer.wrap(new Array[Char](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
        
        case _: ShortBuffer => def verify() {
          val col = new ArrayBuffer[(Float, ShortBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[ShortBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = ShortBuffer.wrap(new Array[Short](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
        
        case _: IntBuffer => def verify() {
          val col = new ArrayBuffer[(Float, IntBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[IntBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = IntBuffer.wrap(new Array[Int](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
        
        case _: FloatBuffer => def verify() {
          val col = new ArrayBuffer[(Float, FloatBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[FloatBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = FloatBuffer.wrap(new Array[Float](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
        
        case _: DoubleBuffer => def verify() {
          val col = new ArrayBuffer[(Float, DoubleBuffer)]
          for (i <- 0 until count) {
            val srcPos = (first + i*stride)*data.stride
            val srcLim = srcPos + stride*data.stride
            val srcBuff = data.readOnlyBuffer().asInstanceOf[DoubleBuffer]
            srcBuff.limit(srcLim)
            srcBuff.position(srcPos)
            col += ((mapping(i), srcBuff))
          }
          val sorted = col.sortBy(_._1)
          
          val compareBuff = DoubleBuffer.wrap(new Array[Double](count*stride*data.stride))
          for ((_, buff) <- sorted) {
            compareBuff.put(buff)
          }
          compareBuff.rewind()
          
          // Compare
          destBuff.limit(end)
          destBuff.position(start)
          assert(destBuff.equals(compareBuff))
        }; verify()
      }
    }
    
    
    // Test with various first, stride, count, and destFirst parameters.
    for (
      first <- 0 until 3;
      stride <- 1 to 4;
      destFirst <- 0 until 3;
      maxCount = data.size/stride - math.max(first, destFirst); count <- List(0, 1, 2, maxCount - 2, maxCount - 1, maxCount)
    ) {
      testFunction(first, count, stride, destFirst) {
        data.reorder(sortContext, first, count, stride, dest, destFirst)
      }
    }
    
    // Test shorthands.
    for (stride <- 1 to 4) {
      testFunction(0, data.size/stride, stride, 0) {
        data.reorder(sortContext, stride, dest)
      }
    }
  }
}
