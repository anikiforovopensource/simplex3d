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
import scala.collection.mutable.{IndexedSeq, ArrayLike}
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseSeq[
  T <: MetaType, @specialized(Int, Float, Double) E, +D <: RawType
] (
  val buffer: D#BufferType
) extends IndexedSeq[E] with ArrayLike[E, IndexedSeq[E]] {

  if (stride <= 0)
    throw new IllegalArgumentException(
      "Stride must be greater than zero."
    )
  if (offset < 0)
    throw new IllegalArgumentException(
      "Offset must be greater than or equal to zero."
    )

  def componentManifest: ClassManifest[T#Component#Element]
  
  final val componentBytes: Int = Binding.byteLength(componentBinding)
  final def byteSize = buffer.capacity*componentBytes
  final def byteOffset = offset*componentBytes
  final def byteStride = stride*componentBytes
  def bindingBuffer: Buffer

  final override val size: Int =
    (buffer.capacity - offset + stride - components)/stride
  final def length = size

  def apply(i: Int) :E
  def update(i: Int, v: E)

  def mkDataArray(size: Int) :DataArray[T, D]
  def mkDataArray(array: D#ArrayType @uncheckedVariance) :DataArray[T, D]
  def mkDataBuffer(size: Int) :DataBuffer[T, D]
  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[T, D]
  def mkDataView(byteBuffer: ByteBuffer, offset: Int,stride: Int):DataView[T, D]

  def components: Int
  def componentBinding: Int
  def normalized: Boolean

  def offset: Int
  def stride: Int = components

  def backingSeq: ContiguousSeq[T#Component, D]

  private final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buffer.isInstanceOf[IntBuffer]) {
      val b = buffer.asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaType, Int, _ <: RawType]]
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
      val b = buffer.asInstanceOf[FloatBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaType, Float, _ <: RawType]]
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
      val b = buffer.asInstanceOf[DoubleBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaType, Double, _ <: RawType]]
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
    src: ContiguousSeq[T#Component, _],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    def grp(binding: Int) = {
      (binding: @switch) match {
        case Binding.SByte => 0
        case Binding.UByte => 0
        case Binding.SShort => 1
        case Binding.UShort => 2
        case Binding.SInt => 3
        case Binding.UInt => 3
        case Binding.HalfFloat => 4
        case Binding.RawFloat => 5
        case Binding.RawDouble => 6
        case _ => throw new AssertionError("Binding not found.")
      }
    }

    val destOffset = index*stride + offset
    val srcLim = srcOffset + count*srcStride

    if (index + count > size) throw new BufferOverflowException()
    if (srcLim > src.buffer.capacity) throw new BufferUnderflowException()

    val group = grp(componentBinding)
    val noConversion = (
      componentBinding == src.componentBinding ||
      (!normalized && !src.normalized && group == grp(src.componentBinding))
    )


    if (stride == components && srcStride == components && noConversion) {
      buffer.position(destOffset)
      src.buffer.position(srcOffset)
      src.buffer.limit(srcLim)

      try {
        (group: @switch) match {
          case 0 =>
            buffer.asInstanceOf[ByteBuffer].put(
              src.buffer.asInstanceOf[ByteBuffer]
            )
          case 1 =>
            buffer.asInstanceOf[ShortBuffer].put(
              src.buffer.asInstanceOf[ShortBuffer]
            )
          case 2 =>
            buffer.asInstanceOf[CharBuffer].put(
              src.buffer.asInstanceOf[CharBuffer]
            )
          case 3 =>
            buffer.asInstanceOf[IntBuffer].put(
              src.buffer.asInstanceOf[IntBuffer]
            )
          case 4 =>
            buffer.asInstanceOf[ShortBuffer].put(
              src.buffer.asInstanceOf[ShortBuffer]
          )
          case 5 =>
            buffer.asInstanceOf[FloatBuffer].put(
              src.buffer.asInstanceOf[FloatBuffer]
            )
          case 6 =>
            buffer.asInstanceOf[DoubleBuffer].put(
              src.buffer.asInstanceOf[DoubleBuffer]
            )
        }
      }
      finally {
        // Always restore the limit, since Buffer.get(index) depends on it.
        src.buffer.limit(src.buffer.capacity)
      }
    }
    else if (noConversion && group < 5) {
      (group: @switch) match {
        case 0 => Copying.copyBuffer(
            components,
            buffer.asInstanceOf[ByteBuffer],
            destOffset,
            stride,
            src.buffer.asInstanceOf[ByteBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 1 => Copying.copyBuffer(
            components,
            buffer.asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.buffer.asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 2 => Copying.copyBuffer(
            components,
            buffer.asInstanceOf[CharBuffer],
            destOffset,
            stride,
            src.buffer.asInstanceOf[CharBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 3 => Copying.copyBuffer(
            components,
            buffer.asInstanceOf[IntBuffer],
            destOffset,
            stride,
            src.buffer.asInstanceOf[IntBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 4 => Copying.copyBuffer(
            components,
            buffer.asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.buffer.asInstanceOf[ShortBuffer],
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
            src.asInstanceOf[ContiguousSeq[Int1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case scala.reflect.ClassManifest.Float => Copying.copySeqFloat(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Float1, _]],
            destOffset,
            stride,
            src.asInstanceOf[ContiguousSeq[Float1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case scala.reflect.ClassManifest.Double => Copying.copySeqDouble(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Double1, _]],
            destOffset,
            stride,
            src.asInstanceOf[ContiguousSeq[Double1, _]],
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
    src: ContiguousSeq[T#Component, _],
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
    src: ContiguousSeq[T#Component, _]
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
    src: ContiguousSeq[T#Component, _]
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
    src: DataSeq[T, _],
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
    src: DataSeq[T, _]
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
    src: DataSeq[T, _]
  ) {
    put(
      0,
      src.backingSeq,
      src.offset,
      src.stride,
      src.size
    )
  }

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

trait DataSeq[T <: MetaType, +D <: RawType] extends BaseSeq[T, T#Element, D]

trait ContiguousSeq[T <: MetaType, +D <: RawType] extends DataSeq[T, D] {
  final val offset = 0
  assert(stride == components)
}

trait DataArray[T <: MetaType, +D <: RawType]
extends DataSeq[T, D] with ContiguousSeq[T, D] {
  final def bindingBuffer = buffer

  def array: D#ArrayType = backingSeq.array
  def backingSeq: DataArray[T#Component, D]
}

trait DataBuffer[T <: MetaType, +D <: RawType]
extends DataView[T, D] with ContiguousSeq[T, D]

trait DataView[T <: MetaType, +D <: RawType] extends DataSeq[T, D] {
  final def bindingBuffer = byteBuffer
  
  def byteBuffer: ByteBuffer = backingSeq.byteBuffer
  def backingSeq: DataBuffer[T#Component, D]

  if (!buffer.isDirect)
    throw new IllegalArgumentException(
      "The buffer must be direct."
    )
  if (byteBuffer.order != ByteOrder.nativeOrder)
    throw new IllegalArgumentException(
      "The buffer must have native byte order."
    )
  if (byteBuffer.isReadOnly)
    throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
}


object DataArray {
  def apply[T <: MetaType, D <: ReadType](array: D#ArrayType)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    ref.factory.mkDataArray(array)
  }

  def apply[T <: MetaType, D <: ReadType](size: Int)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    ref.factory.mkDataArray(size)
  }

  def apply[T <: MetaType, D <: ReadType](vals: T#Element*)(
    implicit ref: FactoryRef[T, D]
  ) :DataArray[T, D] = {
    val data = ref.factory.mkDataArray(vals.size)
    data.put(vals)
    data
  }
}

object DataBuffer {
  def apply[T <: MetaType, D <: ReadType](buffer: ByteBuffer)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    ref.factory.mkDataBuffer(buffer)
  }

  def apply[T <: MetaType, D <: ReadType](size: Int)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    ref.factory.mkDataBuffer(size)
  }

  def apply[T <: MetaType, D <: ReadType](vals: T#Element*)(
    implicit ref: FactoryRef[T, D]
  ) :DataBuffer[T, D] = {
    val data = ref.factory.mkDataBuffer(vals.size)
    data.put(vals)
    data
  }
}

object DataView {
  def apply[T <: MetaType, D <: ReadType](
    buffer: ByteBuffer, offset: Int, stride: Int
  )(implicit ref: FactoryRef[T, D]) :DataView[T, D] = {
    ref.factory.mkDataView(buffer, offset, stride)
  }
}

object DataSeq {
  def apply[T <: MetaType, D <: ReadType](
    implicit ref: FactoryRef[T, D]
  ) :DataSeq[T, D] = {
    ref.factory
  }
}

abstract class FactoryRef[T <: MetaType, D <: RawType] {
  def factory: DataSeq[T, D]
}

class SimpleFactoryRef[T <: MetaType, D <: RawType](
  val factory: DataSeq[T, D]
) extends FactoryRef[T, D]

// Extend this, add implicit tuples to your package object to enable constructor
abstract class GenericSeq[T <: Composite, +D <: RawType](
  val backingSeq: ContiguousSeq[T#Component, D]
) extends BaseSeq[T, T#Element, D](backingSeq.buffer) {
  final def componentManifest = backingSeq.componentManifest.asInstanceOf[
    ClassManifest[T#Component#Element]
  ]
  
  final def componentBinding = backingSeq.componentBinding
  final def normalized: Boolean = backingSeq.normalized
}
