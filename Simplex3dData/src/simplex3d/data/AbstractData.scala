/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data

import java.nio._
import scala.annotation._
import scala.reflect._
import scala.collection._
import scala.collection.mutable.WrappedArray
import StoreType._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class AbstractData[
  F <: Meta,
  @specialized(Int, Float, Double) ReadAs <: WriteAs,
  @specialized(Int, Float, Double) WriteAs,
  +R <: Raw
] private[data] (
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends ReadAbstractData[F, ReadAs, R](
  shared, prim, ro,
  off, str
) with Batch[WriteAs] {

  type PrimitiveSeq <: Contiguous[F#Component, R]

  final def buffer() :R#Buffer = Util.duplicateBuff(storeType, buff).asInstanceOf[R#Buffer]

  override def apply(i: Int) :ReadAs
  def update(i: Int, v: WriteAs)


  private[this] final def putArray(
    index: Int, array: Array[Int], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[IntBuffer]) {
      val b = buffer().asInstanceOf[IntBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[_ <: Meta, Int, Int, _ <: Raw]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putArray(
    index: Int, array: Array[Float], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[FloatBuffer]) {
      val b = buffer().asInstanceOf[FloatBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[_ <: Meta, Float, Float, _ <: Raw]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putArray(
    index: Int, array: Array[Double], first: Int, count: Int
  ) {
    if (stride == components && buff.isInstanceOf[DoubleBuffer]) {
      val b = buffer().asInstanceOf[DoubleBuffer]
      b.position(index + offset)
      b.put(array, first, count)
    }
    else {
      val t = this.asInstanceOf[AbstractData[_ <: Meta, Double, Double, _ <: Raw]]
      var i = 0; while (i < count) {
        t(i + index) = array(i + first)
        i += 1
      }
    }
  }
  private[this] final def putIndexedSeq(
    index: Int, seq: IndexedSeq[WriteAs], first: Int, count: Int
  ) {
    var i = 0; while (i < count) {
      this(index + i) = seq(first + i)
      i += 1
    }
  }
  private[this] final def putSeq(index: Int, seq: Seq[WriteAs], first: Int, count: Int) {
    val iter = seq.iterator
    iter.drop(first)
    val lim = index + count
    var i = index; while (i < lim) {
      this(i) = iter.next
      i += 1
    }
  }

  private[this] final def put(index: Int, src: Seq[WriteAs], srcSize: Int, first: Int, count: Int) {
    
    if (isReadOnly) throw new ReadOnlyBufferException()
    if (count < 0) throw new IllegalArgumentException("'count' is less than 0.")
    if (index < 0) throw new IndexOutOfBoundsException("'index' is less than 0.")
    if (first < 0) throw new IndexOutOfBoundsException("'first' is less than 0.")

    if (index + count > size) {
      if (index > size) throw new IndexOutOfBoundsException("'index' exceeds size.")
      else throw new BufferOverflowException()
    }
    if (first + count > srcSize) {
      if (first > srcSize) throw new IndexOutOfBoundsException("'first' exceeds src.size.")
      else throw new BufferUnderflowException()
    }

    src match {
      case wrapped: WrappedArray[_] => wrapped.elemManifest match {
        case Manifest.Int =>
          if (readManifest != Manifest.Int) throw new ClassCastException(
            "Seq[Int] cannot be cast to Seq[" + readManifest + "]."
          )
          putArray(
            index, wrapped.array.asInstanceOf[Array[Int]], first, count
          )
        case Manifest.Float =>
          if (readManifest != Manifest.Float) throw new ClassCastException(
            "Seq[Float] cannot be cast to Seq[" + readManifest + "]."
          )
          putArray(
            index, wrapped.array.asInstanceOf[Array[Float]], first, count
          )
        case Manifest.Double =>
          if (readManifest != Manifest.Double) throw new ClassCastException(
            "Seq[Double] cannot be cast to Seq[" + readManifest + "]."
          )
          putArray(
            index, wrapped.array.asInstanceOf[Array[Double]], first, count
          )
        case _ =>
          putIndexedSeq(
            index, wrapped.asInstanceOf[IndexedSeq[WriteAs]], first, count
          )
      }
      case ds: ReadDataSeq[_, _]
      if ((ds.formatManifest eq formatManifest) || (ds.formatManifest == formatManifest)) => {
        val dsf = ds.asInstanceOf[ReadDataSeq[F, Raw]]
        put(index, dsf.primitives, dsf.offset + first*dsf.stride, dsf.stride, count)
      }
      case is: IndexedSeq[_] => {
        putIndexedSeq(index, is.asInstanceOf[IndexedSeq[WriteAs]], first, count)
      }
      case _ => {
        putSeq(index, src.asInstanceOf[Seq[WriteAs]], first, count)
      }
    }
  }

  final def put(index: Int, src: Seq[WriteAs], first: Int, count: Int) {
    put(index, src, src.size, first, count)
  }
  
  final def put(index: Int, src: Seq[WriteAs]) {
    put(index, src, src.size, 0, size)
  }

  final def put(src: Seq[WriteAs]) {
    put(0, src, src.size, 0, size)
  }


  final def put(
    index: Int,
    src: inContiguous[F#Component, Raw],
    srcOffset: Int, srcStride: Int, count: Int
  ) {
    def group(rawType: Int) = {
      (rawType: @switch) match {
        case SByte | UByte => 0
        case SShort => 1
        case UShort => 2
        case SInt | UInt => 3
        case HFloat => 4
        case RFloat => 5
        case RDouble => 6
      }
    }

    if ((primitives.readManifest ne src.readManifest) && (primitives.readManifest != src.readManifest))
      throw new ClassCastException(
        "DataSeq[" + src.readManifest + "] cannot be cast to DataSeq[" + primitives.readManifest + "]."
      )

    if (isReadOnly) throw new ReadOnlyBufferException()
    if (count < 0) throw new IllegalArgumentException("'count' is less than 0.")
    if (srcStride < 1) throw new IllegalArgumentException("'srcStride' is less than 1.")
    if (index < 0) throw new IndexOutOfBoundsException("'index' is less than 0.")
    if (srcOffset < 0) throw new IndexOutOfBoundsException("'first' is less than 0.")


    val destOffset = offset + index*stride
    val srcLim = srcOffset + (count - 1)*srcStride + components

    if (index + count > size) {
      if (index > size) throw new IndexOutOfBoundsException("'index' exceeds size.")
      else throw new BufferOverflowException()
    }
    if (srcLim > src.buff.capacity) {
      if (srcOffset > src.size) throw new IndexOutOfBoundsException("'srcOffset' exceeds src.size.")
      else throw new BufferUnderflowException()
    }

    val noConversion = (
      (rawType == src.rawType) ||
      (!isNormalized && group(rawType) == group(src.rawType))
    )

    if (stride == components && srcStride == components && noConversion) {
      val destBuff = buffer()
      val srcBuff = src.readOnlyBuffer()

      destBuff.position(destOffset)
      srcBuff.position(srcOffset)
      srcBuff.limit(srcLim)

      (storeType: @switch) match {
        case ByteStore => destBuff.asInstanceOf[ByteBuffer].put(
          srcBuff.asInstanceOf[ByteBuffer]
        )
        case ShortStore => destBuff.asInstanceOf[ShortBuffer].put(
          srcBuff.asInstanceOf[ShortBuffer]
        )
        case CharStore => destBuff.asInstanceOf[CharBuffer].put(
          srcBuff.asInstanceOf[CharBuffer]
        )
        case IntStore => destBuff.asInstanceOf[IntBuffer].put(
          srcBuff.asInstanceOf[IntBuffer]
        )
        case FloatStore => destBuff.asInstanceOf[FloatBuffer].put(
          srcBuff.asInstanceOf[FloatBuffer]
        )
        case DoubleStore => destBuff.asInstanceOf[DoubleBuffer].put(
          srcBuff.asInstanceOf[DoubleBuffer]
        )
      }
    }
    else if (noConversion) {
      (storeType: @switch) match {
        case ByteStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[ByteBuffer], destOffset, stride,
            src.buff.asInstanceOf[ByteBuffer], srcOffset, srcStride, srcLim
          )
        case ShortStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[ShortBuffer], destOffset, stride,
            src.buff.asInstanceOf[ShortBuffer], srcOffset, srcStride, srcLim
          )
        case CharStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[CharBuffer], destOffset, stride,
            src.buff.asInstanceOf[CharBuffer], srcOffset, srcStride, srcLim
          )
        case IntStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[IntBuffer], destOffset, stride,
            src.buff.asInstanceOf[IntBuffer], srcOffset, srcStride, srcLim
          )
        case FloatStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[FloatBuffer], destOffset, stride,
            src.buff.asInstanceOf[FloatBuffer], srcOffset, srcStride, srcLim
          )
        case DoubleStore => Util.copyBuffer(
            components,
            buff.asInstanceOf[DoubleBuffer], destOffset, stride,
            src.buff.asInstanceOf[DoubleBuffer], srcOffset, srcStride, srcLim
          )
      }
    }
    else {
      primitives.formatManifest match {
        case PrimitiveFormat.SInt => Util.copySeqInt(
            components,
            primitives.asInstanceOf[Contiguous[SInt, _]], destOffset, stride,
            src.asInstanceOf[inContiguous[SInt, _]], srcOffset, srcStride, srcLim
          )
        case PrimitiveFormat.RFloat => Util.copySeqFloat(
            components,
            primitives.asInstanceOf[Contiguous[RFloat, _]], destOffset, stride,
            src.asInstanceOf[inContiguous[RFloat, _]], srcOffset, srcStride, srcLim
          )
        case PrimitiveFormat.RDouble => Util.copySeqDouble(
            components,
            primitives.asInstanceOf[Contiguous[RDouble, _]], destOffset, stride,
            src.asInstanceOf[inContiguous[RDouble, _]], srcOffset, srcStride, srcLim
          )
      }
    }
  }

  final def put(index: Int, src: inContiguous[F#Component, Raw]) {
    put(index, src, 0, components, src.size/components)
  }

  final def put(src: inContiguous[F#Component, Raw]) {
    put(0, src, 0, components, src.size/components)
  }

  final def put(index: Int, src: inDataSeq[F, Raw], first: Int, count: Int) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "DataSeq[" + src.formatManifest + "] cannot be cast to DataSeq[" + formatManifest + "]."
      )

    put(index, src.primitives, src.offset + first*src.stride, src.stride, count)
  }

  final def put(index: Int, src: inDataSeq[F, Raw]) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "DataSeq[" + src.formatManifest + "] cannot be cast to DataSeq[" + formatManifest + "]."
      )

    put(index, src.primitives, src.offset, src.stride, src.size)
  }

  final def put(src: inDataSeq[F, Raw]) {
    if ((formatManifest ne src.formatManifest) && (formatManifest != src.formatManifest))
      throw new ClassCastException(
        "DataSeq[" + src.formatManifest + "] cannot be cast to DataSeq[" + formatManifest + "]."
      )

    put(0, src.primitives, src.offset, src.stride, src.size)
  }
}
