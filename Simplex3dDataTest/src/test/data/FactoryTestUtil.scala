/*
 * Simplex3d, DataTest package
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

package test.data

import java.nio._
import org.scalatest._
import simplex3d.math._
import simplex3d.math.doublex.functions._
import simplex3d.data._

import TestUtil._
import AttributeTestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FactoryTestUtil extends FunSuite {

  private def verifyClass[R <: Unsigned](
    seq: ReadIndexSeq[R], factory: ReadIndexSeq[R] => ReadIndexSeq[R]
  ) {
    val f1 = factory(seq)
    val f2 = factory(f1)
    assert(f1.getClass == f2.getClass)
  }

  private def verifyClass[F <: Format, R <: Raw](
    seq: ReadDataSeq[F, R], factory: ReadDataSeq[F, R] => ReadDataSeq[F, R]
  ) {
    val f1 = factory(seq)
    val f2 = factory(f1)
    assert(f1.getClass == f2.getClass)
  }

  private def testMakeIndex[R <: Unsigned](seq: ReadIndexSeq[R], descriptor: Descriptor[SInt, R]) {
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#Array) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)

    arrayFromSize(seq.mkIndexArray(_))(descriptor)
    arrayFromData((a: R#Array) => seq.mkIndexArray(a))(descriptor)
    bufferFromSize(seq.mkIndexBuffer(_))(descriptor)
    bufferFromData(seq.mkIndexBuffer(_))(descriptor)
    readBufferFromData(seq.mkReadIndexBuffer(_))(descriptor)


    seq match {
      case s: ReadIndexArray[_] =>
        assert(seq.getClass == seq.mkDataArray(0).getClass)
        assert(seq.getClass == seq.mkDataArray(genArray(0, descriptor)).getClass)
        assert(seq.getClass == seq.mkIndexArray(0).getClass)
        assert(seq.getClass == seq.mkIndexArray(genArray(0, descriptor)).getClass)
      case s: ReadIndexBuffer[_] =>
        assert(seq.getClass == seq.mkDataBuffer(0).getClass)
        assert(seq.getClass == seq.mkDataBuffer(ByteBuffer.allocateDirect(0)).getClass)
        assert(seq.getClass == seq.mkReadDataBuffer(ByteBuffer.allocateDirect(0)).getClass)
        assert(seq.getClass == seq.mkIndexBuffer(0).getClass)
        assert(seq.getClass == seq.mkIndexBuffer(ByteBuffer.allocateDirect(0)).getClass)
        assert(seq.getClass == seq.mkReadIndexBuffer(ByteBuffer.allocateDirect(0)).getClass)
    }

    verifyClass[SInt, R](seq, _.mkDataArray(0))
    verifyClass[SInt, R](seq, _.mkDataArray(genArray(0, descriptor)))
    verifyClass[SInt, R](seq, _.mkDataBuffer(0))
    verifyClass[SInt, R](seq, _.mkDataBuffer(ByteBuffer.allocateDirect(0)))
    verifyClass[SInt, R](seq, _.mkReadDataBuffer(ByteBuffer.allocateDirect(0)))
    verifyClass[SInt, R](seq, _.mkDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1))
    verifyClass[SInt, R](seq, _.mkReadDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1))
    
    verifyClass[R](seq, _.mkIndexArray(0))
    verifyClass[R](seq, _.mkIndexArray(genArray(0, descriptor)))
    verifyClass[R](seq, _.mkIndexBuffer(0))
    verifyClass[R](seq, _.mkIndexBuffer(ByteBuffer.allocateDirect(0)))
    verifyClass[R](seq, _.mkReadIndexBuffer(ByteBuffer.allocateDirect(0)))
  }

  def testIndexArrayUsingDataSize(factory: (Int, Int) => IndexArray[Unsigned]) {
    arrayFromSize(factory(_, 0))(Descriptors.SIntUByte)
    arrayFromSize(factory(_, 100))(Descriptors.SIntUByte)
    arrayFromSize(factory(_, 256))(Descriptors.SIntUByte)

    arrayFromSize(factory(_, 257))(Descriptors.SIntUShort)
    arrayFromSize(factory(_, 1000))(Descriptors.SIntUShort)
    arrayFromSize(factory(_, 65536))(Descriptors.SIntUShort)

    arrayFromSize(factory(_, 65537))(Descriptors.SIntUInt)
    arrayFromSize(factory(_, 100000))(Descriptors.SIntUInt)
  }

  def testIndexBufferUsingDataSize(factory: (Int, Int) => IndexBuffer[Unsigned]) {
    bufferFromSize(factory(_, 0))(Descriptors.SIntUByte)
    bufferFromSize(factory(_, 100))(Descriptors.SIntUByte)
    bufferFromSize(factory(_, 256))(Descriptors.SIntUByte)

    bufferFromSize(factory(_, 257))(Descriptors.SIntUShort)
    bufferFromSize(factory(_, 1000))(Descriptors.SIntUShort)
    bufferFromSize(factory(_, 65536))(Descriptors.SIntUShort)

    bufferFromSize(factory(_, 65537))(Descriptors.SIntUInt)
    bufferFromSize(factory(_, 100000))(Descriptors.SIntUInt)
  }

  def testIndexArrayFromSize[R <: Unsigned](
    factory: (Int) => IndexArray[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    arrayFromSize(factory)(descriptor)
    testMakeIndex(factory(0), descriptor)
  }

  def testIndexArrayFromData[R <: Unsigned](
    factory: (R#Array) => IndexArray[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    arrayFromData(factory)(descriptor)
    testMakeIndex(factory(genArray(0, descriptor)), descriptor)
  }

  def testIndexBufferFromSize[R <: Unsigned](
    factory: (Int) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    bufferFromSize(factory)(descriptor)
    testMakeIndex(factory(0), descriptor)
  }

  def testIndexBufferFromData[R <: Unsigned](
    factory: (ByteBuffer) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    bufferFromData(factory)(descriptor)
    testMakeIndex(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testReadIndexBufferFromData[R <: Unsigned](
    factory: (ByteBuffer) => ReadIndexBuffer[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    readBufferFromData(factory)(descriptor)
    testMakeIndex(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testIndexArrayFromCollection[R <: Unsigned](
    factory: (IndexedSeq[Int]) => IndexArray[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    arrayFromCollection[SInt, R](factory)
    testMakeIndex(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  def testIndexBufferFromCollection[R <: Unsigned](
    factory: (IndexedSeq[Int]) => IndexBuffer[R]
  )(implicit descriptor: Descriptor[SInt, R]) {
    bufferFromCollection[SInt, R](factory)
    testMakeIndex(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }


  private def testMakeData[F <: Format, R <: Raw](seq: ReadDataSeq[F, R], descriptor: Descriptor[F, R]) {
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#Array) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)


    seq match {
      case s: ReadDataArray[_, _] =>
        assert(seq.getClass == seq.mkDataArray(0).getClass)
        assert(seq.getClass == seq.mkDataArray(genArray(0, descriptor)).getClass)
      case s: ReadDataBuffer[_, _] =>
        assert(seq.getClass == seq.mkDataBuffer(0).getClass)
        assert(seq.getClass == seq.mkDataBuffer(ByteBuffer.allocateDirect(0)).getClass)
        assert(seq.getClass == seq.mkReadDataBuffer(ByteBuffer.allocateDirect(0)).getClass)
      case s: ReadDataView[_, _] =>
        assert(seq.getClass == seq.mkDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1).getClass)
        assert(seq.getClass == seq.mkReadDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1).getClass)
    }

    verifyClass[F, R](seq, _.mkDataArray(0))
    verifyClass[F, R](seq, _.mkDataArray(genArray(0, descriptor)))
    verifyClass[F, R](seq, _.mkDataBuffer(0))
    verifyClass[F, R](seq, _.mkDataBuffer(ByteBuffer.allocateDirect(0)))
    verifyClass[F, R](seq, _.mkReadDataBuffer(ByteBuffer.allocateDirect(0)))
    verifyClass[F, R](seq, _.mkDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1))
    verifyClass[F, R](seq, _.mkReadDataView(ByteBuffer.allocateDirect(0), 0, seq.components + 1))
  }

  def testArrayFromSize[F <: Format, R <: Raw](
    factory: (Int) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    arrayFromSize(factory)(descriptor)
    testMakeData(factory(0), descriptor)

    checkSubDataExceptions(factory(2))
  }

  def testArrayFromData[F <: Format, R <: Raw](
    factory: (R#Array) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    arrayFromData(factory)(descriptor)
    testMakeData(factory(genArray(0, descriptor)), descriptor)

    CastTestUtil.testArrayCast(factory)(descriptor)
    SerializationTestUtil.testSerialization(factory)(descriptor)
  }

  def testBufferFromSize[F <: Format, R <: Raw](
    factory: (Int) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    bufferFromSize(factory)(descriptor)
    testMakeData(factory(0), descriptor)

    checkSubDataExceptions(factory(2))
  }

  def testBufferFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    bufferFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1), descriptor)

    CastTestUtil.testBufferCast(factory)(descriptor)
  }

  def testViewFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer, Int, Int) => DataView[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    viewFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1, 0, 6), descriptor)

    checkSubDataExceptions(factory(genBuffer(2*4*8, descriptor)._1, 0, 6))
  }

  def testReadBufferFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer) => ReadDataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    readBufferFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1), descriptor)
  }

  def testReadViewFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer, Int, Int) => ReadDataView[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    readViewFromData(factory)(descriptor)
    testMakeData(factory(genBuffer(0, descriptor)._1, 0, 6), descriptor)
  }

  def testArrayFromCollection[F <: Format, R <: Raw](
    factory: (IndexedSeq[F#Meta#Read]) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    arrayFromCollection(factory)
    testMakeData(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  def testBufferFromCollection[F <: Format, R <: Raw](
    factory: (IndexedSeq[F#Meta#Read]) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    bufferFromCollection(factory)
    testMakeData(factory(genRandomCollection(0, descriptor)._1), descriptor)
  }

  private def arrayFromSize[F <: Format, R <: Raw](
    factory: (Int) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    intercept[Exception] { factory(-1) }

    for (size <- 0 to 9) {
      val data = wrapArray(genArray(size*descriptor.components, descriptor))
      testArray(factory(size), false, data)(descriptor)
    }
  }

  private def arrayFromData[F <: Format, R <: Raw](
    factory: (R#Array) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    for (size <- 0 to 9) {
      val a = genRandomArray(size, descriptor);
      testArray(factory(a), false, wrapArray(a))(descriptor)
    }
  }

  private def bufferFromSize[F <: Format, R <: Raw](
    factory: (Int) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    intercept[IllegalArgumentException] { factory(-1) }

    for (size <- 0 to 64) {
      val (bytes, data) = genBuffer(
        size*descriptor.components*RawType.byteLength(descriptor.rawType),
        descriptor
      )
      testBuffer(factory(size), false, data)(descriptor)
    }
  }
  
  private def bufferFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
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

  private def viewFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer, Int, Int) => DataView[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 6)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor);
      factory(bytes.asReadOnlyBuffer, 0, 6)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 6)
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
        offset <- 0 to (if (size == 0) stride - descriptor.components else min(stride - descriptor.components, data.limit))
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
            view.asInstanceOf[ReadDataBuffer[F, R]]
            testBuffer(view.asInstanceOf[ReadDataBuffer[F, R]], false, data)(descriptor)
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

    test(0)
    test(1)
    for (i <- 1 to descriptor.components; s <- 0 to 1) {
      val oversize = s*5*(descriptor.components + 4)*rawBytes
      test(i*rawBytes - 1 + oversize)
      test(i*rawBytes + oversize)
      test(i*rawBytes + 1 + oversize)
    }
  }

  private def readBufferFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer) => ReadDataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
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

  private def readViewFromData[F <: Format, R <: Raw](
    factory: (ByteBuffer, Int, Int) => ReadDataView[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 6)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 6)
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
        offset <- 0 to (if (size == 0) stride - descriptor.components else min(stride - descriptor.components, data.limit))
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
            testBuffer(view.asInstanceOf[ReadDataBuffer[F, R]], false, data)(descriptor)

            val rview = factory(bytes.asReadOnlyBuffer, offset, stride)
            testView(rview, offset, stride, true, data)(descriptor)
            testBuffer(rview.asInstanceOf[ReadDataBuffer[F, R]], true, data)(descriptor)
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

    test(0)
    test(1)
    for (i <- 1 to descriptor.components; s <- 0 to 1) {
      val oversize = s*5*(descriptor.components + 4)*rawBytes
      test(i*rawBytes - 1 + oversize)
      test(i*rawBytes + oversize)
      test(i*rawBytes + 1 + oversize)
    }
  }

  def arrayFromCollection[F <: Format, R <: Raw](
    factory: (IndexedSeq[F#Meta#Read]) => DataArray[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    for (size <- 0 to 3) {
      val (col, data) = genRandomCollection(size, descriptor)
      testArray(factory(col), false, data)(descriptor)
    }
  }

  def bufferFromCollection[F <: Format, R <: Raw](
    factory: (IndexedSeq[F#Meta#Read]) => DataBuffer[F, R]
  )(implicit descriptor: Descriptor[F, R]) {
    for (size <- 0 to 3) {
      val (col, data) = genRandomCollection(size, descriptor)
      testBuffer(factory(col), false, data)(descriptor)
    }
  }
}
