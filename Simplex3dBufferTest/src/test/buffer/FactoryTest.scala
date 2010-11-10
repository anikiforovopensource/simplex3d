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
import simplex3d.math._
import simplex3d.math.CoreMath._
import simplex3d.buffer._

import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FactoryTest extends FunSuite {

  private def testMakeIndex[R <: Unsigned](seq: ReadIndexSeq[R], descriptor: Descriptor[Int1, R]) {
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)

    arrayFromSize(seq.mkIndexArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkIndexArray(a))(descriptor)
    bufferFromSize(seq.mkIndexBuffer(_))(descriptor)
    bufferFromData(seq.mkIndexBuffer(_))(descriptor)
    readBufferFromData(seq.mkReadIndexBuffer(_))(descriptor)
  }

  def testIndexArrayUsingDataSize(factory: (Int, Int) => IndexArray[Unsigned]) {
    arrayFromSize(factory(_, 0))(Descriptors.Int1UByte)
    arrayFromSize(factory(_, 100))(Descriptors.Int1UByte)
    arrayFromSize(factory(_, 256))(Descriptors.Int1UByte)

    arrayFromSize(factory(_, 257))(Descriptors.Int1UShort)
    arrayFromSize(factory(_, 1000))(Descriptors.Int1UShort)
    arrayFromSize(factory(_, 65536))(Descriptors.Int1UShort)

    arrayFromSize(factory(_, 65537))(Descriptors.Int1UInt)
    arrayFromSize(factory(_, 100000))(Descriptors.Int1UInt)
  }

  def testIndexBufferUsingDataSize(factory: (Int, Int) => IndexBuffer[Unsigned]) {
    bufferFromSize(factory(_, 0))(Descriptors.Int1UByte)
    bufferFromSize(factory(_, 100))(Descriptors.Int1UByte)
    bufferFromSize(factory(_, 256))(Descriptors.Int1UByte)

    bufferFromSize(factory(_, 257))(Descriptors.Int1UShort)
    bufferFromSize(factory(_, 1000))(Descriptors.Int1UShort)
    bufferFromSize(factory(_, 65536))(Descriptors.Int1UShort)

    bufferFromSize(factory(_, 65537))(Descriptors.Int1UInt)
    bufferFromSize(factory(_, 100000))(Descriptors.Int1UInt)
  }

  def testIndexArrayFromSize[R <: Unsigned](
    factory: (Int) => IndexArray[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    arrayFromSize(factory)(descriptor)
    testMakeIndex(factory(0), descriptor)
  }

  def testIndexArrayFromData[R <: Unsigned](
    factory: (R#ArrayType) => IndexArray[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    arrayFromData(factory)(descriptor)
    testMakeIndex(factory(genArray(0, descriptor)), descriptor)
  }

  def testIndexBufferFromSize[R <: Unsigned](
    factory: (Int) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    bufferFromSize(factory)(descriptor)
    testMakeIndex(factory(0), descriptor)
  }

  def testIndexBufferFromData[R <: Unsigned](
    factory: (ByteBuffer) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    bufferFromData(factory)(descriptor)
    testMakeIndex(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testReadIndexBufferFromData[R <: Unsigned](
    factory: (ByteBuffer) => ReadIndexBuffer[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    readBufferFromData(factory)(descriptor)
    testMakeIndex(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testIndexArrayFromCollection[R <: Unsigned](
    factory: (IndexedSeq[Int]) => IndexArray[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    arrayFromCollection[Int1, R](factory)
    testMakeIndex(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  def testIndexBufferFromCollection[R <: Unsigned](
    factory: (IndexedSeq[Int]) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[Int1, R]) {
    bufferFromCollection[Int1, R](factory)
    testMakeIndex(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }


  private def testMakeData[E <: MetaElement, R <: RawData](seq: ReadDataSeq[E, R], descriptor: Descriptor[E, R]) {
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testArrayFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    arrayFromSize(factory)(descriptor)
    testMakeData(factory(0), descriptor)
  }

  def testArrayFromData[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    arrayFromData(factory)(descriptor)
    testMakeData(factory(genArray(0, descriptor)), descriptor)

    CastTest.testArrayCast(factory)(descriptor)
  }

  def testBufferFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    bufferFromSize(factory)(descriptor)
    testMakeData(factory(0), descriptor)
  }

  def testBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    bufferFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1), descriptor)

    CastTest.testBufferCast(factory)(descriptor)
  }

  def testViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => DataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    viewFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1, 0, 4), descriptor)
  }

  def testReadBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => ReadDataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    readBufferFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testReadViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => ReadDataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    readViewFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1, 0, 4), descriptor)
  }

  def testArrayFromCollection[E <: MetaElement, R <: RawData](
    factory: (IndexedSeq[E#Read]) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    arrayFromCollection(factory)
    testMakeData(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  def testBufferFromCollection[E <: MetaElement, R <: RawData](
    factory: (IndexedSeq[E#Read]) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    bufferFromCollection(factory)
    testMakeData(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  private def arrayFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[Exception] { factory(-1) }

    for (size <- 0 to 9) {
      val data = wrapArray(genArray(size*descriptor.components, descriptor))
      testArray(factory(size), false, data)(descriptor)
    }
  }

  private def arrayFromData[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    for (size <- 0 to 9) {
      val a = genRandomArray(size, descriptor);
      testArray(factory(a), false, wrapArray(a))(descriptor)
    }
  }

  private def bufferFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] { factory(-1) }

    for (size <- 0 to 64) {
      val (bytes, data) = genBuffer(
        size*descriptor.components*RawType.byteLength(descriptor.rawType),
        descriptor
      )
      testBuffer(factory(size), false, data)(descriptor)
    }
  }
  
  private def bufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor);
      factory(bytes.asReadOnlyBuffer)
    }

    val rawBytes = RawType.byteLength(descriptor.rawType)

    for (size <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(size, descriptor)

      // Test different buffer configurations
      for (i <- 0 to 1; j <- 0 to 1; n <- 0 to 1) {
        val order = if (n == 0) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
        val pos = min(i*rawBytes, size)
        val limit = max(0, bytes.capacity - j*rawBytes)
        val position = (if (pos > limit) limit else pos)

        bytes.clear()
        bytes.order(order)
        bytes.limit(limit)
        bytes.position(position)

        testBuffer(factory(bytes), false, data)(descriptor)

        assert(bytes.order == order)
        assert(bytes.limit == limit)
        assert(bytes.position == position)
      }
    }
  }

  private def viewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => DataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 4)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor);
      factory(bytes.asReadOnlyBuffer, 0, 4)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 4)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, -1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, 0)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, descriptor.components - 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 1, descriptor.components)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 2 + 1, descriptor.components + 2)
    }


    val rawBytes = RawType.byteLength(descriptor.rawType)
    def test(size: Int) {
      val (bytes, data) = genRandomBuffer(size, descriptor)

      for (
        stride <- descriptor.components to (descriptor.components + 4);
        offset <- 0 to min(stride - descriptor.components, data.limit)
      ) {
        // Test different buffer configurations
        for (i <- 0 to 1; j <- 0 to 1; n <- 0 to 1) {
          val order = if (n == 0) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
          val pos = min(i*rawBytes, size)
          val limit = max(0, bytes.capacity - j*rawBytes)
          val position = (if (pos > limit) limit else pos)

          bytes.clear()
          bytes.order(order)
          bytes.limit(limit)
          bytes.position(position)

          if (offset == 0 && stride == descriptor.components) {
            val view = factory(bytes, offset, stride)
            testView(view, offset, stride, false, data)(descriptor)
            testBuffer(view.asInstanceOf[ReadDataBuffer[E, R]], false, data)(descriptor)
          }
          else {
            testView(factory(bytes, offset, stride), offset, stride, false, data)(descriptor)
          }

          assert(bytes.order == order)
          assert(bytes.limit == limit)
          assert(bytes.position == position)
        }
      }
    }

    for (byteCapacity <- 0 to rawBytes; s <- 0 to 1) {
      test(byteCapacity + rawBytes*descriptor.components*3*s)
    }
    for (size <- 1 to 9) {
      test(size*rawBytes)
    }
  }

  private def readBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => ReadDataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes)
    }

    val rawBytes = RawType.byteLength(descriptor.rawType)
    
    for (size <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(size, descriptor);

      // Test different buffer configurations
      for (i <- 0 to 1; j <- 0 to 1; n <- 0 to 1) {
        val order = if (n == 0) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
        val pos = min(i*rawBytes, size)
        val limit = max(0, bytes.capacity - j*rawBytes)
        val position = (if (pos > limit) limit else pos)

        bytes.clear()
        bytes.order(order)
        bytes.limit(limit)
        bytes.position(position)

        testBuffer(factory(bytes), false, data)(descriptor)
        testBuffer(factory(bytes.asReadOnlyBuffer), true, data)(descriptor)

        assert(bytes.order == order)
        assert(bytes.limit == limit)
        assert(bytes.position == position)
      }
    }
  }

  private def readViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => ReadDataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 4)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 4)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, -1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, 0)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, descriptor.components - 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 1, descriptor.components)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 2 + 1, descriptor.components + 2)
    }

    
    val rawBytes = RawType.byteLength(descriptor.rawType)
    def test(size: Int) {
      val (bytes, data) = genRandomBuffer(size, descriptor)

      for (
        stride <- descriptor.components to (descriptor.components + 4);
        offset <- 0 to min(stride - descriptor.components, data.limit)
      ) {
        // Test different buffer configurations
        for (i <- 0 to 1; j <- 0 to 1; n <- 0 to 1) {
          val order = if (n == 0) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN
          val pos = min(i*rawBytes, size)
          val limit = max(0, bytes.capacity - j*rawBytes)
          val position = (if (pos > limit) limit else pos)

          bytes.clear()
          bytes.order(order)
          bytes.limit(limit)
          bytes.position(position)

          if (offset == 0 && stride == descriptor.components) {
            val view = factory(bytes, offset, stride)
            testView(view, offset, stride, false, data)(descriptor)
            testBuffer(view.asInstanceOf[ReadDataBuffer[E, R]], false, data)(descriptor)

            val rview = factory(bytes.asReadOnlyBuffer, offset, stride)
            testView(rview, offset, stride, true, data)(descriptor)
            testBuffer(rview.asInstanceOf[ReadDataBuffer[E, R]], true, data)(descriptor)
          }
          else {
            val view = factory(bytes, offset, stride)
            testView(view, offset, stride, false, data)(descriptor)

            val rview = factory(bytes.asReadOnlyBuffer, offset, stride)
            testView(rview, offset, stride, true, data)(descriptor)
          }

          assert(bytes.order == order)
          assert(bytes.limit == limit)
          assert(bytes.position == position)
        }
      }
    }

    for (byteCapacity <- 0 to rawBytes; s <- 0 to 1) {
      test(byteCapacity + rawBytes*descriptor.components*3*s)
    }
    for (size <- 1 to 9) {
      test(size*rawBytes)
    }
  }

  def arrayFromCollection[E <: MetaElement, R <: RawData](
    factory: (IndexedSeq[E#Read]) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    for (size <- 0 to 3) {
      val (col, data) = genRandomCollection(size, descriptor)
      testArray(factory(col), false, data)(descriptor)
    }
  }

  def bufferFromCollection[E <: MetaElement, R <: RawData](
    factory: (IndexedSeq[E#Read]) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    for (size <- 0 to 3) {
      val (col, data) = genRandomCollection(size, descriptor)
      testBuffer(factory(col), false, data)(descriptor)
    }
  }
}
