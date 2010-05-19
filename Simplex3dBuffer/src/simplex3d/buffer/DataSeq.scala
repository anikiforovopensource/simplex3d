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

  final def componentBytes: Int = Binding.byteLength(componentBinding)
  final val byteSize = buffer.capacity*componentBytes
  final val byteOffset = offset*componentBytes
  final val byteStride = stride*componentBytes
  def bindingBuffer: Buffer

  final override val size: Int =
    (buffer.capacity - offset + stride)/(components +stride)
  final def length = size

  protected[buffer] final val step = components + stride

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
  def stride: Int

  def backingSeq: ContiguousSeq[T#Component, D]

  protected def translatePut(
    destOffset: Int,
    src: ContiguousSeq[T#Component, _],
    srcOffset: Int,
    srcStep: Int,
    srcLim: Int
  )

  private final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == 0 && buffer.isInstanceOf[IntBuffer]) {
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
    if (stride == 0 && buffer.isInstanceOf[FloatBuffer]) {
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
    if (stride == 0 && buffer.isInstanceOf[DoubleBuffer]) {
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
  private final def putBuffer(
    destOffset: Int, src: ByteBuffer, srcOffset: Int, srcStep: Int, srcLim: Int
  ) {
    val dest = buffer.asInstanceOf[ByteBuffer]

    (components: @switch) match {
      case 1 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          desti += step
          srci += srcStep
        }
      case 2 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          desti += step
          srci += srcStep
        }
      case 3 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          desti += step
          srci += srcStep
        }
      case 4 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          dest.put(desti + 3, src.get(srci + 3))
          desti += step
          srci += srcStep
        }
      case _ =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {

          var j = 0; while (j < components) {
            dest.put(desti + j, src.get(srci + j))
          }

          desti += step
          srci += srcStep
        }
    }
  }
  private final def putBuffer(
    destOffset: Int, src: ShortBuffer, srcOffset: Int, srcStep: Int, srcLim: Int
  ) {
    val dest = buffer.asInstanceOf[ShortBuffer]

    (components: @switch) match {
      case 1 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          desti += step
          srci += srcStep
        }
      case 2 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          desti += step
          srci += srcStep
        }
      case 3 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          desti += step
          srci += srcStep
        }
      case 4 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          dest.put(desti + 3, src.get(srci + 3))
          desti += step
          srci += srcStep
        }
      case _ =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {

          var j = 0; while (j < components) {
            dest.put(desti + j, src.get(srci + j))
          }

          desti += step
          srci += srcStep
        }
    }
  }
  private final def putBuffer(
    destOffset: Int, src: CharBuffer, srcOffset: Int, srcStep: Int, srcLim: Int
  ) {
    val dest = buffer.asInstanceOf[CharBuffer]

    (components: @switch) match {
      case 1 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          desti += step
          srci += srcStep
        }
      case 2 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          desti += step
          srci += srcStep
        }
      case 3 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          desti += step
          srci += srcStep
        }
      case 4 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          dest.put(desti + 3, src.get(srci + 3))
          desti += step
          srci += srcStep
        }
      case _ =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {

          var j = 0; while (j < components) {
            dest.put(desti + j, src.get(srci + j))
          }

          desti += step
          srci += srcStep
        }
    }
  }
  private final def putBuffer(
    destOffset: Int, src: IntBuffer, srcOffset: Int, srcStep: Int, srcLim: Int
  ) {
    val dest = buffer.asInstanceOf[IntBuffer]

    (components: @switch) match {
      case 1 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          desti += step
          srci += srcStep
        }
      case 2 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          desti += step
          srci += srcStep
        }
      case 3 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          desti += step
          srci += srcStep
        }
      case 4 =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {
          dest.put(desti, src.get(srci))
          dest.put(desti + 1, src.get(srci + 1))
          dest.put(desti + 2, src.get(srci + 2))
          dest.put(desti + 3, src.get(srci + 3))
          desti += step
          srci += srcStep
        }
      case _ =>
        var desti = destOffset
        var srci = srcOffset

        while (srci < srcLim) {

          var j = 0; while (j < components) {
            dest.put(desti + j, src.get(srci + j))
          }

          desti += step
          srci += srcStep
        }
    }
  }

  final def put(index: Int, seq: Seq[E], first: Int, count: Int) {
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
          
          case _ =>
            var i = 0; while (i < count) {
              this(index + i) = wrapped(first + i)
              i += 1
            }
        }

      case _ =>
        var i = index
        val iter = seq.iterator
        iter.drop(first)
        while (iter.hasNext) {
          this(i) = iter.next
          i += 1
        }
    }
  }

  final def put(index: Int, seq: Seq[E]) {
    put(index, seq, 0, seq.size)
  }

  final def put(seq: Seq[E]) {
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
    val group = grp(componentBinding)
    val noConversion = (
      componentBinding == src.componentBinding ||
      (!normalized && !src.normalized && group == grp(src.componentBinding))
    )

    if (stride == 0 && srcStride == 0 && noConversion) {
      val srcLim = srcOffset + count*components
      if (srcLim < src.buffer.capacity) throw new BufferUnderflowException()

      buffer.position(index*step + offset)
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
    else {
      val destOffset = index*step + offset
      val srcStep = srcStride + components
      val srcLim = srcOffset + count*srcStep

      if (index + count > size) throw new BufferOverflowException()
      if (srcLim > src.buffer.capacity) throw new BufferUnderflowException()

      if (noConversion && group < 4) {
        (group: @switch) match {
          case 0 =>
            putBuffer(
              destOffset,
              src.buffer.asInstanceOf[ByteBuffer],
              srcOffset,
              srcStep,
              srcLim
            )
          case 1 =>
            putBuffer(
              destOffset,
              src.buffer.asInstanceOf[ShortBuffer],
              srcOffset,
              srcStep,
              srcLim
            )
          case 2 =>
            putBuffer(
              destOffset,
              src.buffer.asInstanceOf[CharBuffer],
              srcOffset,
              srcStep,
              srcLim
            )
          case 3 =>
            putBuffer(
              destOffset,
              src.buffer.asInstanceOf[IntBuffer],
              srcOffset,
              srcStep,
              srcLim
            )
        }
      }
      else {
        translatePut(
          destOffset,
          src,
          srcOffset,
          srcStep,
          srcLim
        )
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
      0,
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
      0,
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
      0,
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
      src.offset + first*src.step,
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
  final val stride = 0
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
  
  final def componentBinding = backingSeq.componentBinding
  final def normalized: Boolean = backingSeq.normalized
}
