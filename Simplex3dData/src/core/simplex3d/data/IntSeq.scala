/*
 * Simplex3dData - Core Module
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
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.data.extension._
import Util._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] sealed abstract class BaseSInt[+R <: TangibleInt](
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends AbstractData[Int, Int](shared, prim, ro, off, str)
with DataSeq[SInt, R] with CompositionFactory[SInt, TangibleInt]
{
  final def formatManifest = PrimitiveFormat.SInt
  final def accessorManifest = PrimitiveFormat.SInt
  final def components: Int = 1
  final def isNormalized = false

  final def mkReadDataArray[P <: TangibleInt](prim: ReadDataArray[SInt, P])
  :ReadDataArray[SInt, P] = prim
  final def mkReadDataBuffer[P <: TangibleInt](prim: ReadDataBuffer[SInt, P])
  :ReadDataBuffer[SInt, P] = prim
  protected final def mkReadDataViewInstance[P <: TangibleInt](
    prim: ReadDataBuffer[SInt, P], off: Int, str: Int
  ) :ReadDataView[SInt, P] = new ViewSInt(prim, off, str)

  protected final def mkReadDataViewInstance(
    byteBuffer: ByteBuffer, off: Int, str: Int
  ) :ReadDataView[SInt, R] = {
    new ViewSInt(primitives.mkReadDataBuffer(byteBuffer), off, str)
  }
  
  final override def mkSerializableInstance() = new PrimitiveSInt(rawType)
}

private[data] final class ViewSInt[+R <: TangibleInt](
  prim: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseSInt[R](prim, prim, prim.isReadOnly, off, str) with DataView[SInt, R] {
  type Read = ReadDataView[SInt, R @uncheckedVariance]

  final def rawType = primitives.rawType
  def mkReadOnlyInstance() = new ViewSInt(primitives.asReadOnly(), offset, stride)

  def apply(i: Int) :Int = primitives(offset + i*stride)
  def update(i: Int, v: Int) :Unit = primitives(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[SInt, R] =
    primitives.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[SInt, R] =
    primitives.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[data] final class ArraySIntSByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseSInt[SByte](rarray, null, warray == null, 0, 1) with DataArray[SInt, SByte]
with PrimitiveFactory[SInt, SByte]
{
  type Read = ReadDataArray[SInt, SByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArraySIntSByte(rarray, null)
  def rawType = RawType.SByte

  def mkDataArray(array: Array[Byte]) =
    new ArraySIntSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[data] final class BufferSIntSByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[SByte](shared, null, ro, 0, 1) with DataBuffer[SInt, SByte] {
  type Read = ReadDataBuffer[SInt, SByte]

  def mkReadOnlyInstance() = new BufferSIntSByte(shared, true)
  def rawType = RawType.SByte

  def mkDataArray(array: Array[Byte]) =
    new ArraySIntSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toByte)
}


// Type: UByte
private[data] final class ArraySIntUByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseSInt[UByte](rarray, null, warray == null, 0, 1) with DataArray[SInt, UByte]
with PrimitiveFactory[SInt, UByte]
{
  type Read = ReadDataArray[SInt, UByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArraySIntUByte(rarray, null)
  def rawType = RawType.UByte

  def mkDataArray(array: Array[Byte]) =
    new ArraySIntUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i) & 0xFF
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[data] final class BufferSIntUByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[UByte](shared, null, ro, 0, 1) with DataBuffer[SInt, UByte] {
  type Read = ReadDataBuffer[SInt, UByte]

  def mkReadOnlyInstance() = new BufferSIntUByte(shared, true)
  def rawType = RawType.UByte

  def mkDataArray(array: Array[Byte]) =
    new ArraySIntUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i) & 0xFF
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toByte)
}


// Type: SShort
private[data] final class ArraySIntSShort(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseSInt[SShort](rarray, null, warray == null, 0, 1) with DataArray[SInt, SShort]
with PrimitiveFactory[SInt, SShort]
{
  type Read = ReadDataArray[SInt, SShort]

  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArraySIntSShort(rarray, null)
  def rawType = RawType.SShort

  def mkDataArray(array: Array[Short]) =
    new ArraySIntSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toShort
}

private[data] final class BufferSIntSShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[SShort](shared, null, ro, 0, 1) with DataBuffer[SInt, SShort] {
  type Read = ReadDataBuffer[SInt, SShort]

  def mkReadOnlyInstance() = new BufferSIntSShort(shared, true)
  def rawType = RawType.SShort

  def mkDataArray(array: Array[Short]) =
    new ArraySIntSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toShort)
}


// Type: UShort
private[data] final class ArraySIntUShort(
  rarray: Array[Char], warray: Array[Char]
)
extends BaseSInt[UShort](rarray, null, warray == null, 0, 1) with DataArray[SInt, UShort]
with PrimitiveFactory[SInt, UShort]
{
  type Read = ReadDataArray[SInt, UShort]

  def this() = this(emptyChar, emptyChar)
  def mkReadOnlyInstance() = new ArraySIntUShort(rarray, null)
  def rawType = RawType.UShort

  def mkDataArray(array: Array[Char]) =
    new ArraySIntUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toChar
}

private[data] final class BufferSIntUShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[UShort](shared, null, ro, 0, 1) with DataBuffer[SInt, UShort] {
  type Read = ReadDataBuffer[SInt, UShort]

  def mkReadOnlyInstance() = new BufferSIntUShort(shared, true)
  def rawType = RawType.UShort

  def mkDataArray(array: Array[Char]) =
    new ArraySIntUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toChar)
}


// Type: SInt
private[data] final class ArraySIntSInt(
  rarray: Array[Int], warray: Array[Int]
) extends BaseSInt[SInt](rarray, null, warray == null, 0, 1) with DataArray[SInt, SInt]
with PrimitiveFactory[SInt, SInt]
{
  type Read = ReadDataArray[SInt, SInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArraySIntSInt(rarray, null)
  def rawType = RawType.SInt

  def mkDataArray(array: Array[Int]) =
    new ArraySIntSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[data] final class BufferSIntSInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[SInt](shared, null, ro, 0, 1) with DataBuffer[SInt, SInt]{
  type Read = ReadDataBuffer[SInt, SInt]

  def mkReadOnlyInstance() = new BufferSIntSInt(shared, true)
  def rawType = RawType.SInt

  def mkDataArray(array: Array[Int]) =
    new ArraySIntSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}


// Type: UInt
private[data] final class ArraySIntUInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseSInt[UInt](rarray, null, warray == null, 0, 1) with DataArray[SInt, UInt]
with PrimitiveFactory[SInt, UInt]
{
  type Read = ReadDataArray[SInt, UInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArraySIntUInt(rarray, null)
  def rawType = RawType.UInt

  def mkDataArray(array: Array[Int]) =
    new ArraySIntUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[data] final class BufferSIntUInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseSInt[UInt](shared, null, ro, 0, 1) with DataBuffer[SInt, UInt]{
  type Read = ReadDataBuffer[SInt, UInt]

  def mkReadOnlyInstance() = new BufferSIntUInt(shared, true)
  def rawType = RawType.UInt

  def mkDataArray(array: Array[Int]) =
    new ArraySIntUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}
