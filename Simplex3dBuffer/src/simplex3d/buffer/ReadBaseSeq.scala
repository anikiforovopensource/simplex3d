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
import scala.reflect.Manifest
import scala.annotation._
import scala.annotation.unchecked._
import scala.collection._
import StoreType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class ReadBaseSeq[
  E <: MetaElement, @specialized(Int, Float, Double) S, +R <: RawData
](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  final val offset: Int, final val stride: Int, sz: java.lang.Integer
) extends Protected[R#ArrayType @uncheckedVariance](shared)
with IndexedSeq[S] with IndexedSeqOptimized[S, IndexedSeq[S]] {

  // Argument checks.
  if (offset < 0)
    throw new IllegalArgumentException(
      "Offset must be greater than or equal to zero."
    )
  if (stride <= 0)
    throw new IllegalArgumentException(
      "Stride must be greater than zero."
    )

  // Essential init.
  final val backingSeq: BackingSeqType = {
    if (backing == null) this.asInstanceOf[BackingSeqType]
    else backing.asInstanceOf[BackingSeqType]
  }
  
  protected final val storeType = storeFromRaw(rawType)

  private[buffer] final val buffer: R#BufferType = {
    (if (sharedStore.isInstanceOf[ByteBuffer]) {
      val byteBuffer =
        if (ro) sharedStore.asInstanceOf[ByteBuffer].asReadOnlyBuffer()
        else sharedStore.asInstanceOf[ByteBuffer].duplicate()

      if (!byteBuffer.isDirect) {
        throw new IllegalArgumentException(
          "The buffer must be direct."
        )
      }
      byteBuffer.clear()
      byteBuffer.order(ByteOrder.nativeOrder())

      (storeType: @switch) match {
        case ByteStore => byteBuffer
        case ShortStore => byteBuffer.asShortBuffer()
        case CharStore => byteBuffer.asCharBuffer()
        case IntStore => byteBuffer.asIntBuffer()
        case FloatStore => byteBuffer.asFloatBuffer()
        case DoubleStore => byteBuffer.asDoubleBuffer()
      }
    }
    else {
      (storeType: @switch) match {
        case ByteStore =>
          val buff = ByteBuffer.wrap(sharedStore.asInstanceOf[Array[Byte]])
          if (ro) buff.asReadOnlyBuffer() else buff
        case ShortStore =>
          val buff = ShortBuffer.wrap(sharedStore.asInstanceOf[Array[Short]])
          if (ro) buff.asReadOnlyBuffer() else buff
        case CharStore =>
          val buff = CharBuffer.wrap(sharedStore.asInstanceOf[Array[Char]])
          if (ro) buff.asReadOnlyBuffer() else buff
        case IntStore =>
          val buff = IntBuffer.wrap(sharedStore.asInstanceOf[Array[Int]])
          if (ro) buff.asReadOnlyBuffer() else buff
        case FloatStore =>
          val buff = FloatBuffer.wrap(sharedStore.asInstanceOf[Array[Float]])
          if (ro) buff.asReadOnlyBuffer() else buff
        case DoubleStore =>
          val buff = DoubleBuffer.wrap(sharedStore.asInstanceOf[Array[Double]])
          if (ro) buff.asReadOnlyBuffer() else buff
      }
    }).asInstanceOf[R#BufferType]
  }
  
  if (offset > buffer.capacity)
    throw new IllegalArgumentException(
      "Offset must not be greater than limit."
    )

  private final def sizeFrom(capacity: Int, offset: Int, stride: Int, components: Int) :Int = {
    val s = (capacity - offset + stride - components)/stride
    if (s > 0) s else 0
  }

  final override val size: Int = {
    val maxSize = sizeFrom(buffer.capacity, offset, stride, components)

    if (sz != null) {
      val size: Int = sz.intValue

      if (size < 0)
        throw new IllegalArgumentException(
          "Size cannot be negative."
        )
      if (size > maxSize)
        throw new IllegalArgumentException(
          "Size exceeds storage capacity."
        )

      size
    }
    else {
      maxSize
    }
  }
  final def length = size
  final val limit = offset + size*stride + components - 1

  // Type defenitions.
  type BindingBufferType <: Buffer
  type BackingSeqType <: ReadContiguousSeq[E#Component, R]

  // Public API.
  def elementManifest: Manifest[E#Element]
  def componentManifest: Manifest[E#Component#Element]

  def components: Int
  def rawType: Int
  def normalized: Boolean

  final val bytesPerRawComponent = RawType.byteLength(rawType)
  final def byteCapacity = {
    if (sharedStore.isInstanceOf[ByteBuffer])
      sharedStore.asInstanceOf[ByteBuffer].capacity
    else
      buffer.capacity*bytesPerRawComponent
  }
  final def byteOffset = offset*bytesPerRawComponent
  final def byteStride = stride*bytesPerRawComponent


  final def isReadOnly(): Boolean = buffer.isReadOnly()
  final def sharesStoreObject(seq: inDataSeq[_ <: MetaElement, _ <: RawData]) :Boolean = {
    sharedStore eq seq.sharedStore
  }

  def apply(i: Int) :S
  
  final def asReadOnlyBuffer() :R#BufferType = {
    ((storeType: @switch) match {
      case ByteStore =>
        buffer.asInstanceOf[ByteBuffer].asReadOnlyBuffer()
      case ShortStore =>
        buffer.asInstanceOf[ShortBuffer].asReadOnlyBuffer()
      case CharStore =>
        buffer.asInstanceOf[CharBuffer].asReadOnlyBuffer()
      case IntStore =>
        buffer.asInstanceOf[IntBuffer].asReadOnlyBuffer()
      case FloatStore =>
        buffer.asInstanceOf[FloatBuffer].asReadOnlyBuffer()
      case DoubleStore =>
        buffer.asInstanceOf[DoubleBuffer].asReadOnlyBuffer()
    }).asInstanceOf[R#BufferType]
  }

  protected def mkReadOnlyInstance() :ReadDataSeq[E, R]
  def asReadOnlySeq() :ReadDataSeq[E, R]
  private[buffer] final def toReadOnly() :AnyRef = {
    if (isReadOnly) this else mkReadOnlyInstance()
  }

  private[this] final def mkBindingBuffer() :Buffer = {
    if (sharedStore.isInstanceOf[ByteBuffer]) {
      val buff = sharedStore.asInstanceOf[ByteBuffer].asReadOnlyBuffer()
      buff.order(ByteOrder.nativeOrder())
    }
    else {
      (storeType: @switch) match {
        case ByteStore =>
          ByteBuffer.wrap(sharedStore.asInstanceOf[Array[Byte]])
        case ShortStore =>
          ShortBuffer.wrap(sharedStore.asInstanceOf[Array[Short]])
        case CharStore =>
          CharBuffer.wrap(sharedStore.asInstanceOf[Array[Char]])
        case IntStore =>
          IntBuffer.wrap(sharedStore.asInstanceOf[Array[Int]])
        case FloatStore =>
          FloatBuffer.wrap(sharedStore.asInstanceOf[Array[Float]])
        case DoubleStore =>
          DoubleBuffer.wrap(sharedStore.asInstanceOf[Array[Double]])
      }
    }
  }
  final def bindingBuffer(offset: Int, count: Int) :BindingBufferType = {
    val buff = mkBindingBuffer()
    val lim = offset + count*stride

    if (sharedStore.isInstanceOf[ByteBuffer]) {
      buff.position(offset*bytesPerRawComponent)
      buff.limit(lim*bytesPerRawComponent)
    }
    else {
      buff.position(offset)
      buff.limit(lim)
    }

    buff.asInstanceOf[BindingBufferType]
  }
  final def bindingBuffer(offset: Int) :BindingBufferType = {
    val buff = mkBindingBuffer()
    buff.limit(buff.capacity)

    if (sharedStore.isInstanceOf[ByteBuffer]) {
      buff.position(offset*bytesPerRawComponent)
    }
    else {
      buff.position(offset)
    }

    buff.asInstanceOf[BindingBufferType]
  }

  def mkDataArray(array: R#ArrayType @uncheckedVariance) :DataArray[E, R]
  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[E, R]

  protected def mkReadDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer
  ) :ReadDataView[E, R]


  private[this] final def mkViewOrBuffer(
    byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer
  ) :ReadDataView[E, R] = {
    if (
      offset == 0 && stride == components && (
        size == null ||
        size.intValue == sizeFrom(byteBuffer.capacity/bytesPerRawComponent, offset, stride, components)
      )
    ) {
      mkReadDataBuffer(byteBuffer)
    }
    else {
      mkReadDataView(byteBuffer, offset, stride, size)
    }
  }

  final def mkDataArray(size: Int) :DataArray[E, R] = {
    val array = ((storeType: @switch) match {
      case ByteStore => new Array[Byte](size*components)
      case ShortStore => new Array[Short](size*components)
      case CharStore => new Array[Char](size*components)
      case IntStore => new Array[Int](size*components)
      case FloatStore => new Array[Float](size*components)
      case DoubleStore => new Array[Double](size*components)
    }).asInstanceOf[AnyRef]

    mkDataArray(array.asInstanceOf[R#ArrayType])
  }

  final def mkDataBuffer(size: Int) :DataBuffer[E, R] = {
    mkDataBuffer(allocateDirectBuffer(size*bytesPerRawComponent*components))
  }

  final def mkDataBuffer(
    byteBuffer: ByteBuffer
  ) :DataBuffer[E, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[E, R]]
  }

  final def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int, size: Int
  ) :DataView[E, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkViewOrBuffer(byteBuffer, offset, stride, size.asInstanceOf[java.lang.Integer]).asInstanceOf[DataView[E, R]]
  }

  final def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[E, R] = {
    if (byteBuffer.isReadOnly) throw new IllegalArgumentException(
      "The buffer must not be read-only."
    )
    mkViewOrBuffer(byteBuffer, offset, stride, null).asInstanceOf[DataView[E, R]]
  }

  final def mkReadDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int, size: Int
  ) :ReadDataView[E, R] = {
    mkViewOrBuffer(byteBuffer, offset, stride, size.asInstanceOf[java.lang.Integer])
  }

  final def mkReadDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :ReadDataView[E, R] = {
    mkViewOrBuffer(byteBuffer, offset, stride, null)
  }

  
  final def copyAsDataArray() :DataArray[E, R] = {
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
  final def copyAsDataBuffer() :DataBuffer[E, R] = {
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
  :DataView[E, R] = {
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

  override def toString() :String = {
    def getElemName() = {
      val name = elementManifest.erasure.getSimpleName
      if (name.startsWith("Any")) name.substring(3) else name
    }

    var view = false

    (if (isReadOnly()) "ReadOnly" else "") +
    (this match {
      case s: DataArray[_, _] => "DataArray"
      case s: DataBuffer[_, _] => "DataBuffer"
      case s: DataView[_, _] => view = true; "DataView"
    }) +
    "[" + getElemName() + ", " + RawType.name(rawType)+ "](" +
    (if (view) "offset = " + offset + ", " else "") +
    "stride = " + stride + ", size = " + size + ")"
  }
}
