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
object AttributeTest extends FunSuite {

  private def rawLength(rawType: Int) :Int = {
    rawType match {
      case SByte => 1
      case UByte => 1
      case SShort => 2
      case UShort => 2
      case SInt => 4
      case UInt => 4
      case HalfFloat => 2
      case RawFloat => 4
      case RawDouble => 8
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

  private def checkRawBufferSubData[E <: MetaElement, R <: RawData](seq: ReadDataSeq[E, R], data: Buffer) {
    checkRawBuffer(0, 0, seq.rawBufferSubData(0, 0), data)
    checkRawBuffer(0, seq.size*seq.stride, seq.rawBufferSubData(0, seq.size), data)
    if (seq.size > 1) {
      checkRawBuffer(1*seq.stride, 1*seq.stride, seq.rawBufferSubData(1, 0), data)
      checkRawBuffer(1*seq.stride, seq.size*seq.stride, seq.rawBufferSubData(1, seq.size - 1), data)
    }
  }

  private def testSeq[E <: MetaElement, R <: RawData](
    seq: ReadDataSeq[E, R],
    readOnly: Boolean,
    data: Buffer,
    descriptor: Descriptor[E, R]
  ) {
    assert(seq.elementManifest == descriptor.elementManifest)
    assert(seq.readManifest == descriptor.readManifest)
    assert(seq.backingSeq.elementManifest == descriptor.componentManifest)
    assert(seq.components == descriptor.components)
    assert(seq.rawType == descriptor.rawType)
    assert(seq.normalized == descriptor.normalized)
    assert(seq.readOnly == readOnly)

    checkBuffer(seq.readOnlyBuffer(), data)
    assert(seq.readOnlyBuffer().isReadOnly)
    checkOrder(seq.readOnlyBuffer())

    assert(seq.bytesPerRawComponent == rawLength(seq.rawType))
    assert(seq.byteCapacity >= seq.bytesPerRawComponent*data.limit)
    assert(seq.byteOffset == seq.bytesPerRawComponent*seq.offset)
    assert(seq.byteStride == seq.bytesPerRawComponent*seq.stride)

    assert(seq.size == size(data.limit, seq.offset, seq.stride, seq.components))
    assert(seq.size == seq.length)

    if (seq.readOnly) {
      assert(seq.backingSeq.readOnly)
      assert(seq.asReadOnlySeq() eq seq)
    }
    else {
      assert(!seq.backingSeq.readOnly)
    }

    // Check rawBuffer.
    if (seq.isInstanceOf[DataArray[_, _]]) {
      assert(seq.rawBuffer().array != null)
      assert(seq.rawBufferWithOffset().array != null)
      assert(seq.rawBufferSubData(0, seq.size).array != null)
    }
    else {
      if (seq.readOnly) {
        assert(seq.rawBuffer().isReadOnly)
        assert(seq.rawBufferWithOffset().isReadOnly)
        assert(seq.rawBufferSubData(0, seq.size).isReadOnly)
      }
      else {
        assert(!seq.rawBuffer().isReadOnly)
        assert(!seq.rawBufferWithOffset().isReadOnly)
        assert(!seq.rawBufferSubData(0, seq.size).isReadOnly)
      }
    }

    checkRawBuffer(0, seq.rawBuffer(), data)
    checkRawBuffer(seq.offset, seq.rawBufferWithOffset(), data)
    checkRawBufferSubData(seq, data)


    // Check DataSeq methods.
    val ds = seq.asInstanceOf[DataSeq[E, R]]
    checkBuffer(ds.buffer(), data)
    assert(ds.buffer().isReadOnly == readOnly)
    checkOrder(ds.buffer())
  }

  def testArray[E <: MetaElement, R <: RawData](
    seq: ReadDataArray[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(!seq.readOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataArray[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataArray[_, _]])

    val ds = seq.asInstanceOf[DataArray[E, R]]
    assert(!ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backingSeq.isInstanceOf[DataArray[_, _]])

      assert(ds.array != null)
      checkBuffer(wrapArray(ds.array), data)
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

    if (seq.elementManifest == MetaManifest.Int1) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexArray[_]])
        if (!seq.readOnly) assert(seq.isInstanceOf[IndexArray[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    if (seq.components == 1) {
      assert(seq eq seq.backingSeq)
    }
    else {
      val backingDesc = descriptor.copy(
        elementManifest = seq.backingSeq.elementManifest,
        readManifest = seq.backingSeq.readManifest,
        components = 1
      )
      testArray(seq.backingSeq, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])
    }

    //asReadOnlySeq
    if (!readOnly) testArray(seq.asReadOnlySeq, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnlySeq))
  }

  def testBuffer[E <: MetaElement, R <: RawData](
    seq: ReadDataBuffer[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataBuffer[_, _]])

    val ds = seq.asInstanceOf[DataBuffer[E, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backingSeq.isInstanceOf[DataBuffer[_, _]])
    }

    if (seq.elementManifest == MetaManifest.Int1) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexBuffer[_]])
        if (!seq.readOnly) assert(seq.isInstanceOf[IndexBuffer[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    if (seq.components == 1) {
      assert(seq eq seq.backingSeq)
    }
    else {
      val backingDesc = descriptor.copy(
        elementManifest = seq.backingSeq.elementManifest,
        readManifest = seq.backingSeq.readManifest,
        components = 1
      )
      testBuffer(seq.backingSeq, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])
    }

    //asReadOnlySeq
    if (!readOnly) testBuffer(seq.asReadOnlySeq, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnlySeq))
  }

  def testView[E <: MetaElement, R <: RawData](
    seq: ReadDataView[E, R],
    offset: Int,
    stride: Int,
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == offset)
    assert(seq.stride == stride)

    assert(seq.readOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataView[_, _]])

    val ds = seq.asInstanceOf[DataView[E, R]]
    assert(ds.buffer.isDirect)

    if (!readOnly) {
      assert(ds.backingSeq.isInstanceOf[DataBuffer[_, _]])
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    val backingDesc = descriptor.copy(
      elementManifest = seq.backingSeq.elementManifest,
      readManifest = seq.backingSeq.readManifest,
      components = 1
    )
    testBuffer(seq.backingSeq, readOnly, data)(backingDesc.asInstanceOf[Descriptor[E#Component, R]])

    //asReadOnlySeq
    if (!readOnly) testView(seq.asReadOnlySeq, offset, stride, true, data)
    assert(seq.sharesStoreObject(seq.asReadOnlySeq))
  }
/*
// Test Copy
  copyAsDataArray()
  copyAsDataBuffer()
  copyAsDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int)

  put(index: Int, seq: Seq[E#Element], first: Int, count: Int)
  put(index: Int, seq: Seq[E#Element])
  put(seq: Seq[E#Element])

  put(index: Int, src: inContiguousSeq[E#Component, _], srcOffset: Int, srcStride: Int, count: Int)
  put(index: Int, src: inContiguousSeq[E#Component, _])
  put(src: inContiguousSeq[E#Component, _])

  put(index: Int, src: inDataSeq[E, _], first: Int, count: Int)
  put(index: Int, src: inDataSeq[E, _])
  put(src: inDataSeq[E, _])
*/
}
