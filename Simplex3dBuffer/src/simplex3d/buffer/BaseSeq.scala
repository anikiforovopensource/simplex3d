/*
 * Simplex3d, BaseBuffer module
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


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseSeq[
  E <: MetaElement, @specialized(Int, Float, Double) S, +R <: RawData
](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  offset: Int, stride: Int, sz: java.lang.Integer
) extends ReadBaseSeq[E, S, R](
  shared, backing, ro,
  offset, stride, sz
) {

  type BackingSeqType <: ContiguousSeq[E#Component, R]
  final def asBuffer() :R#BufferType = {
    ((storeType: @switch) match {
      case ByteStore =>
        buffer.asInstanceOf[ByteBuffer].duplicate()
      case ShortStore =>
        buffer.asInstanceOf[ShortBuffer].duplicate()
      case CharStore =>
        buffer.asInstanceOf[CharBuffer].duplicate()
      case IntStore =>
        buffer.asInstanceOf[IntBuffer].duplicate()
      case FloatStore =>
        buffer.asInstanceOf[FloatBuffer].duplicate()
      case DoubleStore =>
        buffer.asInstanceOf[DoubleBuffer].duplicate()
    }).asInstanceOf[R#BufferType]
  }

  override def apply(i: Int) :S
  def update(i: Int, v: S)


  private final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buffer.isInstanceOf[IntBuffer]) {
      val b = asBuffer().asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Int, _ <: RawData]]
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
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Float, _ <: RawData]]
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
      val t = this.asInstanceOf[BaseSeq[_ <: MetaElement, Double, _ <: RawData]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private final def putArray(
    index: Int, array: Array[_], first: Int, count: Int
  ) {
    val arr = array.asInstanceOf[Array[S]]
    var i = 0; while (i < count) {
      this(index + i) = arr(first + i)
      i += 1
    }
  }
  private final def putIndexedSeq(
    index: Int, seq: IndexedSeq[S], first: Int, count: Int
  ) {
    var i = 0; while (i < count) {
      this(index + i) = seq(first + i)
      i += 1
    }
  }
  private final def putSeq(index: Int, seq: Seq[S], first: Int, count: Int) {
    val iter = seq.iterator
    iter.drop(first)
    val lim = index + count
    var i = index; while (i < lim) {
      this(i) = iter.next
      i += 1
    }
  }
  private final def putSeq(index: Int, seq: Seq[S]) {
    val iter = seq.iterator
    var i = index; while (iter.hasNext) {
      this(i) = iter.next
      i += 1
    }
  }

  final def put(index: Int, seq: Seq[E#Element], first: Int, count: Int) {
    if (index + count > size) throw new BufferOverflowException()

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
        putIndexedSeq(index, is.asInstanceOf[IndexedSeq[S]], first, count)
      case _ =>
        putSeq(index, seq.asInstanceOf[Seq[S]], first, count)
    }
  }

  final def put(index: Int, seq: Seq[E#Element]) {
    seq match {
      case is: IndexedSeq[_] => put(index, seq, 0, seq.size)
      case _ => putSeq(index, seq.asInstanceOf[Seq[S]])
    }
  }

  final def put(seq: Seq[E#Element]) {
    put(0, seq)
  }


  final def put(
    index: Int,
    src: inContiguousSeq[E#Component, _],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    def grp(rawType: Int) = {
      (rawType: @switch) match {
        case RawData.SByte => 0
        case RawData.UByte => 0
        case RawData.SShort => 1
        case RawData.UShort => 2
        case RawData.SInt => 3
        case RawData.UInt => 3
        case RawData.HalfFloat => 4
        case RawData.RawFloat => 5
        case RawData.RawDouble => 6
      }
    }

    val destOffset = index*stride + offset
    val srcLim = srcOffset + count*srcStride

    if (index + count > size) throw new BufferOverflowException()
    if (srcLim > src.buffer.capacity) throw new BufferUnderflowException()

    val group = grp(rawType)
    val noConversion = (
      (rawType == src.rawType) ||
      (!normalized && group == grp(src.rawType))
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
    else if (noConversion) {
      (group: @switch) match {
        case 0 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[ByteBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ByteBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 1 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 2 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[CharBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[CharBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 3 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[IntBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[IntBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 4 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[ShortBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[ShortBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 5 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[FloatBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[FloatBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
        case 6 => Util.copyBuffer(
            components,
            asBuffer().asInstanceOf[DoubleBuffer],
            destOffset,
            stride,
            src.asReadOnlyBuffer().asInstanceOf[DoubleBuffer],
            srcOffset,
            srcStride,
            srcLim
          )
      }
    }
    else {
      componentManifest match {
        case Manifest.Int => Util.copySeqInt(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Int1, _]],
            destOffset,
            stride,
            src.asInstanceOf[inContiguousSeq[Int1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case Manifest.Float => Util.copySeqFloat(
            components,
            backingSeq.asInstanceOf[ContiguousSeq[Float1, _]],
            destOffset,
            stride,
            src.asInstanceOf[inContiguousSeq[Float1, _]],
            srcOffset,
            srcStride,
            srcLim
          )
        case Manifest.Double => Util.copySeqDouble(
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

  final def put(
    index: Int,
    src: inContiguousSeq[E#Component, _]
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
    src: inContiguousSeq[E#Component, _]
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
    src: inDataSeq[E, _],
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
    src: inDataSeq[E, _]
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
    src: inDataSeq[E, _]
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
