/*
 * Simplex3d, CoreBuffer module
 * Copyright (C) 2010, Simplex3d Team
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
import scala.reflect._
import StoreType._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseSeq[
  E <: MetaElement,
  @specialized(Int, Float, Double) SRead <: SWrite,
  @specialized(Int, Float, Double) SWrite,
  +R <: RawData
](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  offset: Int, stride: Int
) extends ReadBaseSeq[E, SRead, R](
  shared, backing, ro,
  offset, stride
) {

  type BackingSeq <: ContiguousSeq[E#Component, R]
  final def buffer() :R#BufferType = {
    ((storeType: @switch) match {
      case ByteStore =>
        buff.asInstanceOf[ByteBuffer].duplicate().order(ByteOrder.nativeOrder)
      case ShortStore =>
        buff.asInstanceOf[ShortBuffer].duplicate()
      case CharStore =>
        buff.asInstanceOf[CharBuffer].duplicate()
      case IntStore =>
        buff.asInstanceOf[IntBuffer].duplicate()
      case FloatStore =>
        buff.asInstanceOf[FloatBuffer].duplicate()
      case DoubleStore =>
        buff.asInstanceOf[DoubleBuffer].duplicate()
    }).asInstanceOf[R#BufferType]
  }

  override def apply(i: Int) :SRead
  def update(i: Int, v: SWrite)


  private final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[IntBuffer]) {
      val b = buffer().asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Int, Int, _ <: RawData]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[Float], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[FloatBuffer]) {
      val b = buffer().asInstanceOf[FloatBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Float, Float, _ <: RawData]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[Double], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[DoubleBuffer]) {
      val b = buffer().asInstanceOf[DoubleBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Double, Double, _ <: RawData]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[_], first: Int, count: Int
  ) {
    val arr = array.asInstanceOf[Array[SWrite]]
    var i = 0; while (i < count) {
      this(index + i) = arr(first + i)
      i += 1
    }
  }
  private final def putIndexedSeq(
    index: Int, seq: IndexedSeq[SWrite], first: Int, count: Int
  ) {
    var i = 0; while (i < count) {
      this(index + i) = seq(first + i)
      i += 1
    }
  }
  private final def putSeq(index: Int, seq: Seq[SWrite], first: Int, count: Int) {
    val iter = seq.iterator
    iter.drop(first)
    val lim = index + count
    var i = index; while (i < lim) {
      this(i) = iter.next
      i += 1
    }
  }

  final def put(index: Int, seq: Seq[E#Read], first: Int, count: Int) {
    if (index + count > size) throw new BufferOverflowException()
    if (first + count > seq.size) throw new BufferUnderflowException()

    import scala.collection.mutable.{WrappedArray}
    seq match {

      case wrapped: WrappedArray[_] =>

        if (first + count > wrapped.array.length)
          throw new IndexOutOfBoundsException(
            "Source sequence is not large enough."
          )

        wrapped.elemManifest match {
          case Manifest.Int => putArray(
              index, wrapped.array.asInstanceOf[Array[Int]], first, count
            )
          case Manifest.Float => putArray(
              index, wrapped.array.asInstanceOf[Array[Float]], first, count
            )
          case Manifest.Double => putArray(
              index, wrapped.array.asInstanceOf[Array[Double]], first, count
            )
          case _ => putArray(
              index, wrapped.array, first, count
            )
        }

      case is: IndexedSeq[_] =>
        if (first + count > is.length)
          throw new IndexOutOfBoundsException(
            "Source sequence is not large enough."
          )
        putIndexedSeq(index, is.asInstanceOf[IndexedSeq[SWrite]], first, count)
      case _ =>
        putSeq(index, seq.asInstanceOf[Seq[SWrite]], first, count)
    }
  }

  final def put(index: Int, seq: Seq[E#Read]) {
    put(index, seq, 0, seq.size)
  }

  final def put(seq: Seq[E#Read]) {
    put(0, seq, 0, seq.size)
  }


  final def put(
    index: Int,
    src: inContiguousSeq[E#Component, RawData],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    def group(rawType: Int) = {
      (rawType: @switch) match {
        case SByte | UByte => 0
        case SShort => 1
        case UShort => 2
        case SInt | UInt => 3
        case HalfFloat => 4
        case RawFloat => 5
        case RawDouble => 6
      }
    }

    if (srcStride < 1) throw new IllegalArgumentException("'srcStride' must be greater than or equal to one.")

    val destOffset = offset + index*stride
    val srcLim = srcOffset + (count - 1)*srcStride + components

    if (index + count > size) throw new BufferOverflowException()
    if (srcLim > src.buff.capacity) throw new BufferUnderflowException()

    val noConversion = (
      (rawType == src.rawType) ||
      (!normalized && group(rawType) == group(src.rawType))
    )

    if (stride == components && srcStride == components && noConversion) {
      val destBuff = buffer()
      val srcBuff = src.readOnlyBuffer()

      destBuff.position(destOffset)
      srcBuff.position(srcOffset)
      srcBuff.limit(srcLim)

      (storeType: @switch) match {
        case ByteStore =>
          destBuff.asInstanceOf[ByteBuffer].put(
            srcBuff.asInstanceOf[ByteBuffer]
          )
        case ShortStore =>
          destBuff.asInstanceOf[ShortBuffer].put(
            srcBuff.asInstanceOf[ShortBuffer]
          )
        case CharStore =>
          destBuff.asInstanceOf[CharBuffer].put(
            srcBuff.asInstanceOf[CharBuffer]
          )
        case IntStore =>
          destBuff.asInstanceOf[IntBuffer].put(
            srcBuff.asInstanceOf[IntBuffer]
          )
        case FloatStore =>
          destBuff.asInstanceOf[FloatBuffer].put(
            srcBuff.asInstanceOf[FloatBuffer]
          )
        case DoubleStore =>
          destBuff.asInstanceOf[DoubleBuffer].put(
            srcBuff.asInstanceOf[DoubleBuffer]
          )
      }
    }
    else if (noConversion) {
      (storeType: @switch) match {
        case ByteStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[ByteBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[ByteBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case ShortStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case CharStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[CharBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[CharBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case IntStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[IntBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[IntBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case FloatStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[FloatBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[FloatBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case DoubleStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[DoubleBuffer],
            destOffset,
            stride,
            src.buff.asInstanceOf[DoubleBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
      }
    }
    else {
      backingSeq.elementManifest match {
        case MetaManifest.Int1 => Util.copySeqInt(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Int1, _]],
            destOffset,
            stride,
            src.asInstanceOf[inContiguousSeq[Int1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case MetaManifest.Float1 => Util.copySeqFloat(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Float1, _]],
            destOffset,
            stride,
            src.asInstanceOf[inContiguousSeq[Float1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case MetaManifest.Double1 => Util.copySeqDouble(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Double1, _]],
            destOffset,
            stride,
            src.asInstanceOf[inContiguousSeq[Double1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
      }
    }
  }

  final def put(index: Int, src: inContiguousSeq[E#Component, RawData]) {
    put(index, src, 0, components, src.size/components)
  }

  final def put(src: inContiguousSeq[E#Component, RawData]) {
    put(0, src, 0, components, src.size/components)
  }

  final def put(index: Int, src: inData[E], first: Int, count: Int) {
    if ((elementManifest ne src.elementManifest) && (elementManifest != src.elementManifest))
      throw new ClassCastException()

    put(index, src.backingSeq, src.offset + first*src.stride, src.stride, count)
  }

  final def put(index: Int, src: inData[E]) {
    if ((elementManifest ne src.elementManifest) && (elementManifest != src.elementManifest))
      throw new ClassCastException()

    put(index, src.backingSeq, src.offset, src.stride, src.size)
  }

  final def put(src: inData[E]) {
    if ((elementManifest ne src.elementManifest) && (elementManifest != src.elementManifest))
      throw new ClassCastException()

    put(0, src.backingSeq, src.offset, src.stride, src.size)
  }
}
