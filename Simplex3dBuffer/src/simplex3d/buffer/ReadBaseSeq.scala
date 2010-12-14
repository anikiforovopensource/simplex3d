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
import scala.annotation.unchecked._
import scala.collection._
import StoreType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class ReadBaseSeq[
  E <: MetaElement, @specialized(Int, Float, Double) SRead, +R <: RawData
](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  final val offset: Int, final val stride: Int
) extends Protected[R#ArrayType @uncheckedVariance](shared) with DataSeqFactory[E, R]
with IndexedSeq[SRead] with IndexedSeqOptimized[SRead, IndexedSeq[SRead]] {

  // Assertions
  assert(components >= 1)
  
  // Argument checks.
  if (offset < 0)
    throw new IllegalArgumentException(
      "Offset must be greater than or equal to zero."
    )
  if (stride < components)
    throw new IllegalArgumentException(
      "Stride must be greater than or equal to components."
    )
  if (offset > stride - components)
    throw new IllegalArgumentException(
      "Offset must be less than (stride - components)."
    )
  
  // Essential init.
  final val backingSeq: BackingSeq = {
    if (backing == null) this.asInstanceOf[BackingSeq]
    else backing.asInstanceOf[BackingSeq]
  }
  
  protected final val storeType = storeFromRaw(rawType)

  private[buffer] final val buff: R#BufferType = {
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
      byteBuffer.order(ByteOrder.nativeOrder)

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
          if (ro) buff.asReadOnlyBuffer().order(ByteOrder.nativeOrder) else buff.order(ByteOrder.nativeOrder)
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
  
  if (offset > buff.capacity)
    throw new IllegalArgumentException(
      "Offset must not be greater than limit."
    )

  private[buffer] final def sizeFrom(capacity: Int, offset: Int, stride: Int, components: Int) :Int = {
    val s = (capacity - offset + stride - components)/stride
    if (s > 0) s else 0
  }

  final override val size = sizeFrom(buff.capacity, offset, stride, components)
  final def length = size

  // Type definitions.
  type RawBuffer <: Buffer
  type BackingSeq <: ReadContiguousSeq[E#Component, R]

  // Public API.
  override def rawType: Int
  override def components: Int
  override def elementManifest: ClassManifest[E]
  def readManifest: ClassManifest[E#Read]
  def normalized: Boolean

  final val bytesPerRawComponent = RawType.byteLength(rawType)
  final def byteCapacity = {
    if (sharedStore.isInstanceOf[ByteBuffer])
      sharedStore.asInstanceOf[ByteBuffer].capacity
    else
      buff.capacity*bytesPerRawComponent
  }
  final def byteOffset = offset*bytesPerRawComponent
  final def byteStride = stride*bytesPerRawComponent


  final def readOnly: Boolean = buff.isReadOnly()
  final def sharesStoreObject(seq: inDataSeq[_, _]) :Boolean = {
    sharedStore eq seq.sharedStore
  }

  def apply(i: Int) :SRead

  final def readOnlyBuffer() :R#BufferType = {
    ((storeType: @switch) match {
      case ByteStore =>
        buff.asInstanceOf[ByteBuffer].asReadOnlyBuffer().order(ByteOrder.nativeOrder)
      case ShortStore =>
        buff.asInstanceOf[ShortBuffer].asReadOnlyBuffer()
      case CharStore =>
        buff.asInstanceOf[CharBuffer].asReadOnlyBuffer()
      case IntStore =>
        buff.asInstanceOf[IntBuffer].asReadOnlyBuffer()
      case FloatStore =>
        buff.asInstanceOf[FloatBuffer].asReadOnlyBuffer()
      case DoubleStore =>
        buff.asInstanceOf[DoubleBuffer].asReadOnlyBuffer()
    }).asInstanceOf[R#BufferType]
  }

  protected def mkReadOnlyInstance() :ReadDataSeq[E, R]
  def asReadOnlySeq() :ReadDataSeq[E, R]
  private[buffer] final lazy val readOnlySeq :AnyRef = {
    if (readOnly) this else mkReadOnlyInstance()
  }

  private[this] final def wrapStoreArray() :Buffer = {
    (storeType: @switch) match {
      case ByteStore =>
        val buff = ByteBuffer.wrap(sharedStore.asInstanceOf[Array[Byte]])
        buff.order(ByteOrder.nativeOrder)
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
  private[this] final def binding() :Buffer = {
    if (sharedStore.isInstanceOf[ByteBuffer]) {
      val buff =
        if (readOnly) sharedStore.asInstanceOf[ByteBuffer].asReadOnlyBuffer()
        else sharedStore.asInstanceOf[ByteBuffer].duplicate()
      
      buff.order(ByteOrder.nativeOrder)
      buff
    }
    else {
      wrapStoreArray()
    }
  }

  final def rawBuffer() :RawBuffer = {
    val buff = binding()
    buff.limit(buff.capacity)
    buff.position(0)
    buff.asInstanceOf[RawBuffer]
  }
  final def rawBufferWithOffset() :RawBuffer = {
    val buff = binding()

    if (sharedStore.isInstanceOf[ByteBuffer]) {
      buff.limit(buff.capacity)
      buff.position(offset*bytesPerRawComponent)
    }
    else {
      buff.position(offset)
    }

    buff.asInstanceOf[RawBuffer]
  }
  final def rawBufferSubData(first: Int, count: Int) :RawBuffer = {
    val buff = binding()

    if (sharedStore.isInstanceOf[ByteBuffer]) {
      val off = first*stride*bytesPerRawComponent
      var lim = off + count*stride*bytesPerRawComponent
      if (lim > buff.capacity && first + count == size) lim = buff.capacity
      buff.limit(lim)
      buff.position(off)
    }
    else {
      val off = first*stride
      buff.limit(off + count*stride)
      buff.position(off)
    }

    buff.asInstanceOf[RawBuffer]
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
      elementManifest.erasure.getSimpleName
    }

    var view = false

    (if (readOnly) "ReadOnly" else "") +
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
