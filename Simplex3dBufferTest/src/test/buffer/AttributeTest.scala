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

  private def checkBindingBuffer(offset: Int, binding: Buffer, data: Buffer) {
    data.position(offset)

    assert(binding ne data)
    assert(binding.capacity == binding.limit)

    binding match {
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
      case b: ShortBuffer => assert(b.position == offset); assert(b equals data)
      case b: CharBuffer => assert(b.position == offset); assert(b equals data)
      case b: IntBuffer => assert(b.position == offset); assert(b equals data)
      case b: FloatBuffer => assert(b.position == offset); assert(b equals data)
      case b: DoubleBuffer => assert(b.position == offset); assert(b equals data)
    }

    data.position(0)
  }

  private def testSeq[E <: MetaElement, R <: RawData](
    seq: ReadDataSeq[E, R],
    readOnly: Boolean,
    data: Buffer,
    descriptor: Descriptor[E, R]
  ) {
    assert(seq.elementManifest == descriptor.elementManifest)
    assert(seq.componentManifest == descriptor.componentManifest)
    assert(seq.components == descriptor.components)
    assert(seq.rawType == descriptor.rawType)
    assert(seq.normalized == descriptor.normalized)
    assert(seq.isReadOnly == readOnly)

    checkBuffer(seq.asReadOnlyBuffer, data)
    assert(seq.asReadOnlyBuffer.isReadOnly)
    checkOrder(seq.asReadOnlyBuffer)

    assert(seq.bytesPerRawComponent == rawLength(seq.rawType))
    assert(seq.byteCapacity >= seq.bytesPerRawComponent*data.limit)
    assert(seq.byteOffset == seq.bytesPerRawComponent*seq.offset)
    assert(seq.byteStride == seq.bytesPerRawComponent*seq.stride)

    assert(seq.size == size(data.limit, seq.offset, seq.stride, seq.components))
    assert(seq.size == seq.length)

    if (seq.isReadOnly) {
      assert(seq.backingSeq.isReadOnly)
      assert(seq.asReadOnlySeq eq seq)
    }
    else {
      assert(!seq.backingSeq.isReadOnly)
    }


    if (seq.isInstanceOf[DataArray[_, _]]) {
      assert(seq.bindingBuffer(0).array != null)
    }
    else {
      assert(seq.bindingBuffer(0).isReadOnly)
    }

    checkOrder(seq.bindingBuffer(0))

    checkBindingBuffer(0, seq.bindingBuffer(0), data)
    if (data.limit >= 2) {
      checkBindingBuffer(1, seq.bindingBuffer(1), data)
      checkBindingBuffer(2, seq.bindingBuffer(2), data)
    }


    val ds = seq.asInstanceOf[DataSeq[E, R]]
    checkBuffer(ds.asBuffer, data)
    assert(ds.asBuffer.isReadOnly == readOnly)
    checkOrder(ds.asBuffer)
  }

  def testArray[E <: MetaElement, R <: RawData](
    seq: ReadDataArray[E, R],
    readOnly: Boolean,
    data: Buffer
  )(implicit descriptor: Descriptor[E, R]) {
    assert(seq.offset == 0)
    assert(seq.stride == seq.components)

    assert(!seq.asReadOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataArray[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataArray[_, _]])

    val ds = seq.asInstanceOf[DataArray[E, R]]
    assert(!ds.asBuffer.isDirect)

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

    if (seq.elementManifest == scala.reflect.Manifest.Int) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexArray[_]])
        if (!seq.isReadOnly) assert(seq.isInstanceOf[IndexArray[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    if (seq.components == 1) {
      assert(seq eq seq.backingSeq)
    }
    else {
      val backingDesc = descriptor.copy(elementManifest = descriptor.componentManifest, components = 1)
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

    assert(seq.asReadOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataBuffer[_, _]])

    val ds = seq.asInstanceOf[DataBuffer[E, R]]
    assert(ds.asBuffer.isDirect)

    if (!readOnly) {
      assert(ds.backingSeq.isInstanceOf[DataBuffer[_, _]])
    }

    if (seq.elementManifest == scala.reflect.Manifest.Int) {
      if (isUnsigned(seq.rawType)) {
        assert(seq.isInstanceOf[ReadIndexBuffer[_]])
        if (!seq.isReadOnly) assert(seq.isInstanceOf[IndexBuffer[_]])
      }
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    if (seq.components == 1) {
      assert(seq eq seq.backingSeq)
    }
    else {
      val backingDesc = descriptor.copy(elementManifest = descriptor.componentManifest, components = 1)
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

    assert(seq.asReadOnlyBuffer.isDirect)
    assert(seq.backingSeq.isInstanceOf[ReadDataBuffer[_, _]])
    assert(seq.asReadOnlySeq.isInstanceOf[ReadDataView[_, _]])

    val ds = seq.asInstanceOf[DataView[E, R]]
    assert(ds.asBuffer.isDirect)

    if (!readOnly) {
      assert(ds.backingSeq.isInstanceOf[DataBuffer[_, _]])
    }

    testSeq(seq, readOnly, data, descriptor)

    // backingSeq
    val backingDesc = descriptor.copy(elementManifest = descriptor.componentManifest, components = 1)
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
