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

import org.scalatest._

import java.nio._
import simplex3d.data._
import simplex3d.data.RawType._

import TestUtil._


/**
 * @author Aleksey Nikiforov (lex)
 */
object AttributeTestUtil extends FunSuite {

  private def rawLength(rawType: Int) :Int = {
    rawType match {
      case SByte => 1
      case UByte => 1
      case SShort => 2
      case UShort => 2
      case SInt => 4
      case UInt => 4
      case HFloat => 2
      case RFloat => 4
      case RDouble => 8
    }
  }

  private def checkOrder(buff: Buffer) {
    buff match {
      case b: ByteBuffer => assert(b.order == ByteOrder.nativeOrder)
      case b: ShortBuffer => assert(b.order == ByteOrder.nativeOrder)
      case b: CharBuffer => assert(b.order == ByteOrder.nativeOrder)
      case b: IntBuffer => assert(b.order == ByteOrder.nativeOrder)
      case b: FloatBuffer => assert(b.order == ByteOrder.nativeOrder)
      case b: DoubleBuffer => assert(b.order == ByteOrder.nativeOrder)
    }
  }

  private def checkRawBuffer(offset: Int, bindingBuffer: Buffer, data: Buffer) {
    checkOrder(bindingBuffer)

    data.position(offset)

    assert(bindingBuffer ne data)
    assert(bindingBuffer.capacity == bindingBuffer.limit)

    bindingBuffer match {
      case b: ByteBuffer => {
        data match {
          case d: ByteBuffer => assert(b.position == offset); assert(b equals data)
          case d: ShortBuffer => assert(b.position == offset*2); assert(b.asShortBuffer equals data)
          case d: CharBuffer => assert(b.position == offset*2); assert(b.asCharBuffer equals data)
          case d: IntBuffer => assert(b.position == offset*4); assert(b.asIntBuffer equals data)
          case d: FloatBuffer => assert(b.position == offset*4); assert(b.asFloatBuffer equals data)
          case d: DoubleBuffer => assert(b.position == offset*8); assert(b.asDoubleBuffer equals data)
        }
      }
      case _ => assert(bindingBuffer.position == offset); assert(bindingBuffer equals data)
    }

    data.position(0)
  }

  private def checkRawBuffer(offset: Int, limit: Int, bindingBuffer: Buffer, data: Buffer) {
    checkOrder(bindingBuffer)

    data.position(offset)
    if (limit > data.capacity) data.limit(data.capacity) else data.limit(limit)

    assert(bindingBuffer ne data)

    bindingBuffer match {
      case b: ByteBuffer => {
        data match {
          case d: ByteBuffer =>
            assert(b.position == offset)
            if (limit > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit)
            assert(b equals data)
          case d: ShortBuffer =>
            assert(b.position == offset*2)
            if (limit*2 > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit*2)
            assert(b.asShortBuffer equals data)
          case d: CharBuffer =>
            assert(b.position == offset*2)
            if (limit*2 > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit*2)
            assert(b.asCharBuffer equals data)
          case d: IntBuffer =>
            assert(b.position == offset*4)
            if (limit*4 > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit*4)
            assert(b.asIntBuffer equals data)
          case d: FloatBuffer =>
            assert(b.position == offset*4)
            if (limit*4 > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit*4)
            assert(b.asFloatBuffer equals data)
          case d: DoubleBuffer =>
            assert(b.position == offset*8)
            if (limit*8 > b.capacity) assert(b.limit == b.capacity) else assert(b.limit == limit*8)
            assert(b.asDoubleBuffer equals data)
        }
      }
      case _ =>
        assert(bindingBuffer.position == offset)
        assert(bindingBuffer.limit == limit)
        assert(bindingBuffer equals data)
    }

    data.position(0)
    data.limit(data.capacity)
  }

  private def checkRawBufferSubData[F <: Format, R <: Raw](seq: ReadDataSeq[F, R], data: Buffer) {
    checkRawBuffer(0, 0, seq.bindingBufferSubData(0, 0), data)
    checkRawBuffer(0, seq.size*seq.stride, seq.bindingBufferSubData(0, seq.size), data)
    if (seq.size > 1) {
      checkRawBuffer(1*seq.stride, 1*seq.stride, seq.bindingBufferSubData(1, 0), data)
      checkRawBuffer(1*seq.stride, seq.size*seq.stride, seq.bindingBufferSubData(1, seq.size - 1), data)
    }
  }

  private def testSeq[F <: Format, R <: Raw](
    seq: ReadDataSeq[F, R],
    readOnly: Boolean,
    data: Buffer,
    descriptor: Descriptor[F, R]
  ) {
    assert(seq.formatManifest == descriptor.formatManifest)
    assert(seq.metaManifest == descriptor.metaManifest)
    assert(seq.primitives.formatManifest == descriptor.componentManifest)
    assert(seq.components == descriptor.components)
    assert(seq.rawType == descriptor.rawType)
    assert(seq.isNormalized == descriptor.normalized)
    assert(seq.isReadOnly == readOnly)

    if (data != null) checkBuffer(seq.readOnlyBuffer(), data)
    assert(seq.readOnlyBuffer().isReadOnly)
    checkOrder(seq.readOnlyBuffer())

    assert(seq.bytesPerComponent == rawLength(seq.rawType))
    if (data != null) assert(seq.byteCapacity >= seq.bytesPerComponent*data.limit)
    assert(seq.byteOffset == seq.bytesPerComponent*seq.offset)
    assert(seq.byteStride == seq.bytesPerComponent*seq.stride)

    if (data != null) assert(seq.size == dataSeqSize(data.limit, seq.offset, seq.stride, seq.components))
    assert(seq.size == seq.length)

    if (seq.isReadOnly) {
      assert(seq.primitives.isReadOnly)
      assert(seq.asReadOnly() eq seq)
    }
    else {
      assert(!seq.primitives.isReadOnly)
    }

    // Check bindingBuffer.
    assert(seq.bindingBuffer().isReadOnly == seq.isReadOnly)
    assert(seq.bindingBufferWithOffset().isReadOnly == seq.isReadOnly)
    assert(seq.bindingBufferSubData(0, seq.size).isReadOnly == seq.isReadOnly)

    if (data != null) {
      checkRawBuffer(0, seq.bindingBuffer(), data)
      checkRawBuffer(seq.offset, seq.bindingBufferWithOffset(), data)
      checkRawBufferSubData(seq, data)
    }


    // Check DataSeq methods.
    val ds = seq.asInstanceOf[DataSeq[F, R]]
    if (data != null) checkBuffer(ds.buffer(), data)
    assert(ds.buffer().isReadOnly == readOnly)
    checkOrder(ds.buffer())
  }

  def testArray[F <: Format, R <: Raw](
    seq: ReadDataArray[F, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[F, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(!seq.readOnlyBuffer.isDirect)
    assert(seq.primitives.isInstanceOf[ReadDataArray[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataArray[_, _]])

    val ds = seq.asInstanceOf[DataArray[F, R]]
    assert(!ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.primitives.isInstanceOf[DataArray[_, _]])

      assert(ds.array != null)
      if (data != null) checkBuffer(wrapArray(ds.array), data)
    }
    else {
      intercept[Exception] {
        ds.array match {
          case a: Array[Byte] => a(0) = 1
          case a: Array[Short] => a(0) = 1
          case a: Array[Char] => a(0) = 1
          case a: Array[Int] => a(0) = 1
          case a: Array[Float] => a(0) = 1
          case a: Array[Double] => a(0) = 1
        }
      }
    }

    if (seq.formatManifest == PrimitiveFormat.SInt) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexArray[_]])
        if (!seq.isReadOnly) assert(seq.isInstanceOf[IndexArray[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // primitives
    if (seq.components == 1) {
      assert(seq eq seq.primitives)
    }
    else {
      val primitiveDesc = descriptor.copy(
        formatManifest = seq.primitives.formatManifest,
        metaManifest = seq.primitives.metaManifest,
        components = 1
      )
      testArray(seq.primitives, readOnly, data)(primitiveDesc.asInstanceOf[Descriptor[F#Component, R]])
    }

    //asReadOnly
    if (!readOnly) testArray(seq.asReadOnly, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }

  def testBuffer[F <: Format, R <: Raw](
    seq: ReadDataBuffer[F, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[F, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.primitives.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataBuffer[_, _]])

    val ds = seq.asInstanceOf[DataBuffer[F, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.primitives.isInstanceOf[DataBuffer[_, _]])
    }

    if (seq.formatManifest == PrimitiveFormat.SInt) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexBuffer[_]])
        if (!seq.isReadOnly) assert(seq.isInstanceOf[IndexBuffer[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // primitives
    if (seq.components == 1) {
      assert(seq eq seq.primitives)
    }
    else {
      val primitiveDesc = descriptor.copy(
        formatManifest = seq.primitives.formatManifest,
        metaManifest = seq.primitives.metaManifest,
        components = 1
      )
      testBuffer(seq.primitives, readOnly, data)(primitiveDesc.asInstanceOf[Descriptor[F#Component, R]])
    }

    //asReadOnly
    if (!readOnly) testBuffer(seq.asReadOnly, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }

  def testView[F <: Format, R <: Raw](
    seq: ReadDataView[F, R],
    offset: Int,
    stride: Int,
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[F, R]) {
    assert(seq.offset == offset)
    assert(seq.stride == stride)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.primitives.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataView[_, _]])

    val ds = seq.asInstanceOf[DataView[F, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.primitives.isInstanceOf[DataBuffer[_, _]])
    }

    testSeq(seq, readOnly, data, descriptor)

    // primitives
    val primitiveDesc = descriptor.copy(
      formatManifest = seq.primitives.formatManifest,
      metaManifest = seq.primitives.metaManifest,
      components = 1
    )
    testBuffer(seq.primitives, readOnly, data)(primitiveDesc.asInstanceOf[Descriptor[F#Component, R]])

    //asReadOnly
    if (!readOnly) testView(seq.asReadOnly, offset, stride, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }
}
