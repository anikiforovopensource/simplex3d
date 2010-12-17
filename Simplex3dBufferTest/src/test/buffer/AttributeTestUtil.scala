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
import simplex3d.buffer._
import simplex3d.buffer.RawType._

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

  private def checkRawBuffer(offset: Int, rawBuffer: Buffer, data: Buffer) {
    checkOrder(rawBuffer)

    data.position(offset)

    assert(rawBuffer ne data)
    assert(rawBuffer.capacity == rawBuffer.limit)

    rawBuffer match {
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
      case _ => assert(rawBuffer.position == offset); assert(rawBuffer equals data)
    }

    data.position(0)
  }

  private def checkRawBuffer(offset: Int, limit: Int, rawBuffer: Buffer, data: Buffer) {
    checkOrder(rawBuffer)

    data.position(offset)
    if (limit > data.capacity) data.limit(data.capacity) else data.limit(limit)

    assert(rawBuffer ne data)

    rawBuffer match {
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
        assert(rawBuffer.position == offset)
        assert(rawBuffer.limit == limit)
        assert(rawBuffer equals data)
    }

    data.position(0)
    data.limit(data.capacity)
  }

  private def checkRawBufferSubData[E <: Meta, R <: Raw](seq: ReadDataSeq[E, R], data: Buffer) {
    checkRawBuffer(0, 0, seq.rawBufferSubData(0, 0), data)
    checkRawBuffer(0, seq.size*seq.stride, seq.rawBufferSubData(0, seq.size), data)
    if (seq.size > 1) {
      checkRawBuffer(1*seq.stride, 1*seq.stride, seq.rawBufferSubData(1, 0), data)
      checkRawBuffer(1*seq.stride, seq.size*seq.stride, seq.rawBufferSubData(1, seq.size - 1), data)
    }
  }

  private def testSeq[E <: Meta, R <: Raw](
    seq: ReadDataSeq[E, R],
    readOnly: Boolean,
    data: Buffer,
    descriptor: Descriptor[E, R]
  ) {
    assert(seq.elemManifest == descriptor.elemManifest)
    assert(seq.readManifest == descriptor.readManifest)
    assert(seq.backing.elemManifest == descriptor.componentManifest)
    assert(seq.components == descriptor.components)
    assert(seq.rawType == descriptor.rawType)
    assert(seq.normalized == descriptor.normalized)
    assert(seq.readOnly == readOnly)

    if (data != null) checkBuffer(seq.readOnlyBuffer(), data)
    assert(seq.readOnlyBuffer().isReadOnly)
    checkOrder(seq.readOnlyBuffer())

    assert(seq.bytesPerComponent == rawLength(seq.rawType))
    if (data != null) assert(seq.byteCapacity >= seq.bytesPerComponent*data.limit)
    assert(seq.byteOffset == seq.bytesPerComponent*seq.offset)
    assert(seq.byteStride == seq.bytesPerComponent*seq.stride)

    if (data != null) assert(seq.size == size(data.limit, seq.offset, seq.stride, seq.components))
    assert(seq.size == seq.length)

    if (seq.readOnly) {
      assert(seq.backing.readOnly)
      assert(seq.asReadOnly() eq seq)
    }
    else {
      assert(!seq.backing.readOnly)
    }

    // Check rawBuffer.
    if (seq.isInstanceOf[DataArray[_, _]]) {
      assert(seq.rawBuffer().array != null)
      assert(seq.rawBufferWithOffset().array != null)
      assert(seq.rawBufferSubData(0, seq.size).array != null)
    }
    else {
      assert(seq.rawBuffer().isReadOnly)
      assert(seq.rawBufferWithOffset().isReadOnly)
      assert(seq.rawBufferSubData(0, seq.size).isReadOnly)
    }

    if (data != null) {
      checkRawBuffer(0, seq.rawBuffer(), data)
      checkRawBuffer(seq.offset, seq.rawBufferWithOffset(), data)
      checkRawBufferSubData(seq, data)
    }


    // Check DataSeq methods.
    val ds = seq.asInstanceOf[DataSeq[E, R]]
    if (data != null) checkBuffer(ds.buffer(), data)
    assert(ds.buffer().isReadOnly == readOnly)
    checkOrder(ds.buffer())
  }

  def testArray[E <: Meta, R <: Raw](
    seq: ReadDataArray[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(!seq.readOnlyBuffer.isDirect)
    assert(seq.backing.isInstanceOf[ReadDataArray[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataArray[_, _]])

    val ds = seq.asInstanceOf[DataArray[E, R]]
    assert(!ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backing.isInstanceOf[DataArray[_, _]])

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

    if (seq.elemManifest == MetaManifest.SInt) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexArray[_]])
        if (!seq.readOnly) assert(seq.isInstanceOf[IndexArray[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backing
    if (seq.components == 1) {
      assert(seq eq seq.backing)
    }
    else {
      val backingDesc = descriptor.copy(
        elemManifest = seq.backing.elemManifest,
        readManifest = seq.backing.readManifest,
        components = 1
      )
      testArray(seq.backing, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])
    }

    //asReadOnly
    if (!readOnly) testArray(seq.asReadOnly, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }

  def testBuffer[E <: Meta, R <: Raw](
    seq: ReadDataBuffer[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.backing.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataBuffer[_, _]])

    val ds = seq.asInstanceOf[DataBuffer[E, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backing.isInstanceOf[DataBuffer[_, _]])
    }

    if (seq.elemManifest == MetaManifest.SInt) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexBuffer[_]])
        if (!seq.readOnly) assert(seq.isInstanceOf[IndexBuffer[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backing
    if (seq.components == 1) {
      assert(seq eq seq.backing)
    }
    else {
      val backingDesc = descriptor.copy(
        elemManifest = seq.backing.elemManifest,
        readManifest = seq.backing.readManifest,
        components = 1
      )
      testBuffer(seq.backing, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])
    }

    //asReadOnly
    if (!readOnly) testBuffer(seq.asReadOnly, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }

  def testView[E <: Meta, R <: Raw](
    seq: ReadDataView[E, R],
    offset: Int,
    stride: Int,
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == offset)
    assert(seq.stride == stride)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.backing.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnly.isInstanceOf[ReadDataView[_, _]])

    val ds = seq.asInstanceOf[DataView[E, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backing.isInstanceOf[DataBuffer[_, _]])
    }

    testSeq(seq, readOnly, data, descriptor)

    // backing
    val backingDesc = descriptor.copy(
      elemManifest = seq.backing.elemManifest,
      readManifest = seq.backing.readManifest,
      components = 1
    )
    testBuffer(seq.backing, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])

    //asReadOnly
    if (!readOnly) testView(seq.asReadOnly, offset, stride, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnly))
  }
}
