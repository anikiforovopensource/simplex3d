/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer

import java.nio._
import scala.annotation._
import scala.annotation.unchecked._
import scala.collection._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class ReadOnlyBaseSeq[
  T <: ElemType, @specialized(Int, Float, Double) E, +D <: RawType
] (
  protected[buffer] final val buffer: D#BufferType
) extends IndexedSeq[E] with IndexedSeqOptimized[E, IndexedSeq[E]] {

  if (stride <= 0)
    throw new IllegalArgumentException(
      "Stride must be greater than zero."
    )
  if (offset < 0)
    throw new IllegalArgumentException(
      "Offset must be greater than or equal to zero."
    )

  def elementManifest: ClassManifest[T#Element]
  def componentManifest: ClassManifest[T#Component#Element]

  final val bytesPerRawComponent: Int = RawType.byteLength(bindingType)
  final def byteSize = buffer.capacity*bytesPerRawComponent
  final def byteOffset = offset*bytesPerRawComponent
  final val byteStride = stride*bytesPerRawComponent

  def asReadOnlyBuffer() :D#BufferType
  def sharesContent(seq: ReadOnlyDataSeq[_ <: ElemType, _ <: RawType])

  final def bindingBuffer(offset: Int) = {
    val buff = asReadOnlyBuffer()
    buff.position(offset)
    buff
  }

  final override val size: Int =
    (buffer.capacity - offset + stride - components)/stride
  final def length = size

  def apply(i: Int) :E

  def mkDataArray(size: Int) :DataArray[T, D]
  def mkDataArray(array: D#ArrayType @uncheckedVariance) :DataArray[T, D]
  def mkDataBuffer(size: Int) :DataBuffer[T, D]
  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[T, D]
  def mkDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int):DataView[T, D]

  def components: Int
  def bindingType: Int
  def normalized: Boolean

  def offset: Int
  def stride: Int = components

  def backingSeq: ReadOnlyContiguousSeq[T#Component, D]
  def asReadOnly() :ReadOnlyDataSeq[T, D]

  final def copyAsDataArray() :DataArray[T, D] = {
    val copy = mkDataArray(size)
    copy.put(
      0,
      backingSeq,
      this.offset,
      this.stride,
      size
    )
    copy
  }
  final def copyAsDataBuffer() :DataBuffer[T, D] = {
    val copy = mkDataBuffer(size)
    copy.put(
      0,
      backingSeq,
      this.offset,
      this.stride,
      size
    )
    copy
  }
  final def copyAsDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int)
  :DataView[T, D] = {
    val copy = mkDataView(byteBuffer, offset, stride)
    copy.put(
      0,
      backingSeq,
      this.offset,
      this.stride,
      size
    )
    copy
  }
}


private[buffer] abstract class BaseSeq[
  T <: ElemType, @specialized(Int, Float, Double) E, +D <: RawType
] (
  buff: D#BufferType
) extends ReadOnlyBaseSeq[T, E, D](buff) {

  def asBuffer() :D#BufferType

  override def apply(i: Int) :E
  def update(i: Int, v: E)

  def backingSeq: ContiguousSeq[T#Component, D]

  private final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buffer.isInstanceOf[IntBuffer]) {
      val b = asBuffer().asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: ElemType, Int, _ <: RawType]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[Float], first: Int, count: Int
  ) {
    if (stride == components && buffer.isInstanceOf[FloatBuffer]) {
      val b = asBuffer().asInstanceOf[FloatBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: ElemType, Float, _ <: RawType]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[Double], first: Int, count: Int
  ) {
    if (stride == components && buffer.isInstanceOf[DoubleBuffer]) {
      val b = asBuffer().asInstanceOf[DoubleBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: ElemType, Double, _ <: RawType]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[_], first: Int, count: Int
  ) {
    val arr = array.asInstanceOf[Array[E]]
    var i = 0; while (i < count) {
      this(index + i) = arr(first + i)
      i += 1
    }
  }
  private def putSeq(index: Int, seq: Seq[E], first: Int, count: Int) {
    var i = index
    val iter = seq.iterator
    iter.drop(first)
    while (iter.hasNext) {
      this(i) = iter.next
      i += 1
    }
  }

  final def put(index: Int, seq: Seq[T#Element], first: Int, count: Int) {
    if (index + count > size) throw new BufferOverflowException()

    import scala.collection.mutable._
    import scala.reflect._

    seq match {

      case wrapped: WrappedArray[_] =>

        if (first + count > wrapped.array.length)
          throw new IndexOutOfBoundsException(
            "Source sequence is not large enough."
          )

        wrapped.elemManifest match {
          case ClassManifest.Int => putArray(
              index, wrapped.array.asInstanceOf[Array[Int]], first, count
            )
          case ClassManifest.Float => putArray(
              index, wrapped.array.asInstanceOf[Array[Float]], first, count
            )
          case ClassManifest.Double => putArray(
              index, wrapped.array.asInstanceOf[Array[Double]], first, count
            )
          case _ => putArray(
              index, wrapped.array, first, count
            )
        }

      case _ => putSeq(index, seq.asInstanceOf[Seq[E]], first, count)
    }
  }

  final def put(index: Int, seq: Seq[T#Element]) {
    put(index, seq, 0, seq.size)
  }

  final def put(seq: Seq[T#Element]) {
    put(0, seq, 0, seq.size)
  }


  final def put(
    index: Int,
    src: ReadOnlyContiguousSeq[T#Component, _],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    def grp(binding: Int) = {
      (binding: @switch) match {
        case RawType.SByte => 0
        case RawType.UByte => 0
        case RawType.SShort => 1
        case RawType.UShort => 2
        case RawType.SInt => 3
        case RawType.UInt => 3
        case RawType.HalfFloat => 4
        case RawType.RawFloat => 5
        case RawType.RawDouble => 6
        case _ => throw new AssertionError("Binding not found.")
      }
    }

    val destOffset = index*stride + offset
    val srcLim = srcOffset + count*srcStride

    if (index + count > size) throw new BufferOverflowException()
    if (srcLim > src.buffer.capacity) throw new BufferUnderflowException()

    val group = grp(bindingType)
    val noConversion = (
      bindingType == src.bindingType ||
      (!normalized && !src.normalized && group == grp(src.bindingType))
    )

    if (stride == components && srcStride == components && noConversion) {
      val destBuff = asBuffer()
      val srcBuff = src.asReadOnlyBuffer()

      destBuff.position(destOffset)
      srcBuff.position(srcOffset)
      srcBuff.limit(srcLim)

      (group: @switch) match {
        case 0 =>
          destBuff.asInstanceOf[ByteBuffer].put(
            srcBuff.asInstanceOf[ByteBuffer]
          )
        case 1 =>
          destBuff.asInstanceOf[ShortBuffer].put(
            srcBuff.asInstanceOf[ShortBuffer]
          )
        case 2 =>
          destBuff.asInstanceOf[CharBuffer].put(
            srcBuff.asInstanceOf[CharBuffer]
          )
        case 3 =>
          destBuff.asInstanceOf[IntBuffer].put(
            srcBuff.asInstanceOf[IntBuffer]
          )
        case 4 =>
          destBuff.asInstanceOf[ShortBuffer].put(
            srcBuff.asInstanceOf[ShortBuffer]
        )
        case 5 =>
          destBuff.asInstanceOf[FloatBuffer].put(
            srcBuff.asInstanceOf[FloatBuffer]
          )
        case 6 =>
          destBuff.asInstanceOf[DoubleBuffer].put(
            srcBuff.asInstanceOf[DoubleBuffer]
          )
      }
    }
    else if (noConversion && group < 5) {
      (group: @switch) match {
        case 0 => Copying.copyBuffer(
            components,
            asBuffer().asInstanceOf[ByteBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ByteBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 1 => Copying.copyBuffer(
            components,
            asBuffer().asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 2 => Copying.copyBuffer(
            components,
            asBuffer().asInstanceOf[CharBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[CharBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 3 => Copying.copyBuffer(
            components,
            asBuffer().asInstanceOf[IntBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[IntBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 4 => Copying.copyBuffer(
            components,
            asBuffer().asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
      }
    }
    else {
      componentManifest match {
        case scala.reflect.ClassManifest.Int => Copying.copySeqInt(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Int1, _]],
            destOffset,
            stride,
            src.asInstanceOf[ReadOnlyContiguousSeq[Int1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case scala.reflect.ClassManifest.Float => Copying.copySeqFloat(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Float1, _]],
            destOffset,
            stride,
            src.asInstanceOf[ReadOnlyContiguousSeq[Float1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case scala.reflect.ClassManifest.Double => Copying.copySeqDouble(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Double1, _]],
            destOffset,
            stride,
            src.asInstanceOf[ReadOnlyContiguousSeq[Double1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case _ => throw new AssertionError("Unsupported component type.")
      }
    }
  }

  final def put(
    index: Int,
    src: ReadOnlyContiguousSeq[T#Component, _],
    srcOffset: Int, count: Int
  ) {
    put(
      index,
      src,
      srcOffset,
      components,
      count
    )
  }

  final def put(
    index: Int,
    src: ReadOnlyContiguousSeq[T#Component, _]
  ) {
    put(
      index,
      src,
      0,
      components,
      src.size/components
    )
  }

  final def put(
    src: ReadOnlyContiguousSeq[T#Component, _]
  ) {
    put(
      0,
      src,
      0,
      components,
      src.size/components
    )
  }

  final def put(
    index: Int,
    src: ReadOnlyDataSeq[T, _],
    first: Int, count: Int
  ) {
    put(
      index,
      src.backingSeq,
      src.offset + first*src.stride,
      src.stride,
      count
    )
  }

  final def put(
    index: Int,
    src: ReadOnlyDataSeq[T, _]
  ) {
    put(
      index,
      src.backingSeq,
      src.offset,
      src.stride,
      src.size
    )
  }

  final def put(
    src: ReadOnlyDataSeq[T, _]
  ) {
    put(
      0,
      src.backingSeq,
      src.offset,
      src.stride,
      src.size
    )
  }
}

// Extend this, add implicit tuples to your package object to enable constructor
abstract class GenericSeq[T <: Composite, +D <: RawType](
  val backingSeq: ContiguousSeq[T#Component, D]
) extends BaseSeq[T, T#Element, D](backingSeq.buffer) {
  final def componentManifest = backingSeq.componentManifest.asInstanceOf[
    ClassManifest[T#Component#Element]
  ]

  final def asReadOnlyBuffer() :D#BufferType = backingSeq.asReadOnlyBuffer()
  final def asBuffer() :D#BufferType = backingSeq.asBuffer()
  
  final def bindingType = backingSeq.bindingType
  final def normalized: Boolean = backingSeq.normalized
}
