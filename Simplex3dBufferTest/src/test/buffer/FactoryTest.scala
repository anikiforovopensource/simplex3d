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

import TestUtil._
import AttributeTest._


/**
 * @author Aleksey Nikiforov (lex)
 */
object FactoryTest extends FunSuite {

  def testArrayFomSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    arrayFromSize(factory)(descriptor)

    val seq = factory(0)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testArrayFomData[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    arrayFromData(factory)(descriptor)

    val seq = factory(genArray(0, descriptor))
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testBufferFomSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    bufferFromSize(factory)(descriptor)

    val seq = factory(0)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    bufferFromData(factory)(descriptor)

    val seq = factory(genBuffer(0, descriptor)._1)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => DataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    viewFromData(factory)(descriptor)

    val seq = factory(genBuffer(0, descriptor)._1, 0, 1)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testReadBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => ReadDataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    readBufferFromData(factory)(descriptor)

    val seq = factory(genBuffer(0, descriptor)._1)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  def testReadViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => ReadDataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    readViewFromData(factory)(descriptor)

    val seq = factory(genBuffer(0, descriptor)._1, 0, 1)
    arrayFromSize(seq.mkDataArray(_))(descriptor)
    arrayFromData((a: R#ArrayType) => seq.mkDataArray(a))(descriptor)
    bufferFromSize(seq.mkDataBuffer(_))(descriptor)
    bufferFromData(seq.mkDataBuffer(_))(descriptor)
    viewFromData(seq.mkDataView(_, _, _))(descriptor)
    readBufferFromData(seq.mkReadDataBuffer(_))(descriptor)
    readViewFromData(seq.mkReadDataView(_, _, _))(descriptor)
  }

  private def arrayFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[Exception] { factory(-1) }

    for (i <- 0 to 8) {
      val data = wrapArray(genArray(i, descriptor))
      testArray(factory(i), false, data)(descriptor)
    }
  }

  private def arrayFromData[E <: MetaElement, R <: RawData](
    factory: (R#ArrayType) => DataArray[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    for (i <- 0 to 8) {
      val a = genRandomArray(i, descriptor);
      testArray(factory(a), false, wrapArray(a))(descriptor)
    }
  }

  private def bufferFromSize[E <: MetaElement, R <: RawData](
    factory: (Int) => DataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] { factory(-1) }

    for (i <- 0 to 64) {
      val (bytes, data) = genBuffer(i, descriptor)
      testBuffer(factory(i), false, data)(descriptor)
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

    for (i <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(i, descriptor);
      testBuffer(factory(bytes), false, data)(descriptor)
    }
  }

  private def viewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => DataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor);
      factory(bytes.asReadOnlyBuffer, 0, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 64, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, -1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, 0)
    }

    for (i <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(i, descriptor)
      for (offset <- 0 to 4; stride <- 1 to 5) {
        testView(factory(bytes, offset, stride), offset, stride, false, data)(descriptor)
      }
    }
  }

  private def readBufferFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer) => ReadDataBuffer[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes)
    }

    for (i <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(i, descriptor);
      testBuffer(factory(bytes), false, data)(descriptor)
      testBuffer(factory(bytes.asReadOnlyBuffer), true, data)(descriptor)
    }
  }

  private def readViewFromData[E <: MetaElement, R <: RawData](
    factory: (ByteBuffer, Int, Int) => ReadDataView[E, R]
  )(implicit descriptor: Descriptor[E, R]) {
    intercept[IllegalArgumentException] {
      val bytes = ByteBuffer.wrap(new Array[Byte](64))
      factory(bytes, 0, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, -1, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 64, 1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, -1)
    }

    intercept[IllegalArgumentException] {
      val (bytes, data) = genBuffer(64, descriptor)
      factory(bytes, 0, 0)
    }

    for (i <- 0 to 64) {
      val (bytes, data) = genRandomBuffer(i, descriptor)
      for (offset <- 0 to 4; stride <- 1 to 5) {
        testView(factory(bytes, offset, stride), offset, stride, false, data)(descriptor)
        testView(factory(bytes.asReadOnlyBuffer, offset, stride), offset, stride, true, data)(descriptor)
      }
    }
  }
}
