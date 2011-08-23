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
import scala.collection._
import StoreType._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class ReadAbstractData[
  @specialized(Int, Float, Double) ReadAs
] private[data] (
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  final val offset: Int, final val stride: Int
) extends ProtectedData(shared) with DataSrc
with IndexedSeq[ReadAs] with IndexedSeqOptimized[ReadAs, IndexedSeq[ReadAs]] {

  // Argument checks.
  assert(components >= 1)

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
  
  // Init.
  final val primitives: PrimitiveSeq = {
    if (prim == null) this.asInstanceOf[PrimitiveSeq]
    else prim.asInstanceOf[PrimitiveSeq]
  }
  
  protected final val storeType = storeFromRaw(rawType)

  private[data] final val buff: Raw#Buffer = {
    if (prim != null) {
      primitives.buff
    }
    else {
      (if (sharedStorage.isInstanceOf[ByteBuffer]) {
        if (!sharedBuffer.isDirect) {
          throw new IllegalArgumentException(
            "The buffer must be direct."
          )
        }

        val byteBuffer = {
          if (ro) sharedBuffer.asReadOnlyBuffer()
          else sharedBuffer.duplicate()
        }

        byteBuffer.clear()
        byteBuffer.order(ByteOrder.nativeOrder)
        
        Util.wrapBuffer(storeType, byteBuffer)
      }
      else {
        val buff = Util.wrapArray(storeType, sharedStorage.asInstanceOf[Raw#Array])
        if (ro) Util.readOnlyBuff(storeType, buff) else buff
      }).asInstanceOf[Raw#Buffer]
    }
  }

  private[data] final def sizeFrom(capacity: Int, offset: Int, stride: Int, components: Int) :Int = {
    val s = (capacity - offset + stride - components)/stride
    if (s > 0) s else 0
  }

  final override val size = sizeFrom(buff.capacity, offset, stride, components)
  final def length = size
  final def isCached = true

  
  // Type definitions.
  type Format <: simplex3d.data.Format
  type Raw <: simplex3d.data.Raw
  
  type PrimitiveSeq <: ReadContiguous[Format#Component, Raw]

  
  // Public API.
  def components: Int
  def rawType: Int
  def isNormalized: Boolean
  
  def formatManifest: ClassManifest[Format]
  def metaManifest: ClassManifest[Format#Meta]
  

  final val bytesPerComponent = RawType.byteLength(rawType)
  final def byteCapacity = {
    if (buff.isDirect) sharedBuffer.capacity
    else buff.capacity*bytesPerComponent
  }
  final def byteOffset = offset*bytesPerComponent
  final def byteStride = stride*bytesPerComponent


  final def isReadOnly: Boolean = buff.isReadOnly()
  final def sharesStorageWith(src: DataSrc) :Boolean = {
    src match {
      case d: ReadData[_] => sharedStorage eq d.sharedStorage
      case _ => false
    }
  }

  def apply(i: Int) :ReadAs

  final def readOnlyBuffer() :Raw#Buffer = Util.readOnlyBuff(storeType, buff).asInstanceOf[Raw#Buffer]
  

  private[data] def mkReadOnlyInstance() :Read
  private[this] final lazy val readOnlySeq = (if (isReadOnly) this else mkReadOnlyInstance()).asInstanceOf[Read]
  final def asReadOnly() :Read = readOnlySeq


  private[this] final def binding() :Buffer = {
    if (buff.isDirect) {
      val buff = if (isReadOnly) sharedBuffer.asReadOnlyBuffer() else sharedBuffer.duplicate()
      buff.order(ByteOrder.nativeOrder)
      buff
    }
    else {
      Util.duplicateBuff(storeType, buff)
    }
  }

  final def bindingBuffer() :BindingBuffer = {
    val buff = binding()
    buff.limit(buff.capacity)
    buff.position(0)
    buff.asInstanceOf[BindingBuffer]
  }
  final def bindingBufferWithOffset() :BindingBuffer = {
    if (size == 0) bindingBuffer()
    else {
      val buff = binding()

      if (buff.isDirect) {
        buff.limit(buff.capacity)
        buff.position(offset*bytesPerComponent)
      }
      else {
        buff.position(offset)
      }

      buff.asInstanceOf[BindingBuffer]
    }
  }
  final def bindingBufferSubData(first: Int, count: Int) :BindingBuffer = {
    val buff = binding()

    if (buff.isDirect) {
      val off = first*stride*bytesPerComponent
      var lim = off + count*stride*bytesPerComponent
      if (lim > buff.capacity && first + count == size) lim = buff.capacity
      buff.limit(lim)
      buff.position(off)
    }
    else {
      val off = first*stride
      buff.limit(off + count*stride)
      buff.position(off)
    }

    buff.asInstanceOf[BindingBuffer]
  }


  override def toString() :String = {
    def getElemName() = {
      formatManifest.erasure.getSimpleName
    }

    var view = false

    (if (isReadOnly) "ReadOnly" else "") +
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
