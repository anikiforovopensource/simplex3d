/*
 * Simplex3dData - Float Module
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
package float

import java.nio._
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.data.PrimitiveFormat
import simplex3d.data.extension._
import simplex3d.data.conversion.Float._
import Util._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseRFloat[+R <: TangibleFloat](
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends AbstractData[Float, Float](shared, prim, ro, off, str)
with DataSeq[RFloat, R] with CompositionFactory[RFloat, TangibleFloat]
{
  final def formatTag = PrimitiveFormat.RFloat
  final def accessorTag = PrimitiveFormat.RFloat
  final def components: Int = 1
  
  final def mkReadDataArray[P <: TangibleFloat](prim: ReadDataArray[RFloat, P])
  :ReadDataArray[RFloat, P] = prim
  final def mkReadDataBuffer[P <: TangibleFloat](prim: ReadDataBuffer[RFloat, P])
  :ReadDataBuffer[RFloat, P] = prim
  protected final def mkReadDataViewInstance[P <: TangibleFloat](
    prim: ReadDataBuffer[RFloat, P], off: Int, str: Int
  ) :ReadDataView[RFloat, P] = {
    (prim.rawType match {
      case RawType.RFloat =>
        new ViewRFloatRFloat(prim.asInstanceOf[ReadDataBuffer[RFloat, RFloat]], off, str)
      case _ =>
        new ViewRFloat(prim, off, str)
    }).asInstanceOf[ReadDataView[RFloat, P]]
  }

  protected final def mkReadDataViewInstance(
    byteBuffer: ByteBuffer, off: Int, str: Int
  ) :ReadDataView[RFloat, R] = {
    (rawType match {
      case RawType.RFloat =>
        val prim = primitives.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[RFloat, RFloat]]
        new ViewRFloatRFloat(prim, off, str)
      case _ =>
        new ViewRFloat(primitives.mkReadDataBuffer(byteBuffer), off, str)
    }).asInstanceOf[ReadDataView[RFloat, R]]
  }
  
  final override def mkSerializableInstance() = new PrimitiveRFloat(rawType)
}

private[data] final class ViewRFloat[+R <: TangibleFloat](
  prim: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseRFloat[R](prim, prim, prim.isReadOnly, off, str) with DataView[RFloat, R]
{
  type Read = ReadDataView[RFloat, R @uncheckedVariance]

  final def isNormalized = primitives.isNormalized
  final def rawType = primitives.rawType
  def mkReadOnlyInstance() = new ViewRFloat(primitives.asReadOnly(), offset, stride)

  def apply(i: Int) :Float = primitives(offset + i*stride)
  def update(i: Int, v: Float) :Unit = primitives(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[RFloat, R] =
    primitives.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[RFloat, R] =
    primitives.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[data] final class ArrayRFloatSByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRFloat[SByte](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SByte]
with PrimitiveFactory[RFloat, SByte]
{
  type Read = ReadDataArray[RFloat, SByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRFloatSByte(rarray, null)
  def rawType = RawType.SByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSByte(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSByte(v) }
}

private[data] final class BufferRFloatSByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SByte](shared, null, ro, 0, 1) with DataBuffer[RFloat, SByte] {
  type Read = ReadDataBuffer[RFloat, SByte]

  def mkReadOnlyInstance() = new BufferRFloatSByte(shared, true)
  def rawType = RawType.SByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSByte(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSByte(v)) }
}


// Type: UByte
private[data] final class ArrayRFloatUByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRFloat[UByte](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UByte]
with PrimitiveFactory[RFloat, UByte]
{
  type Read = ReadDataArray[RFloat, UByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRFloatUByte(rarray, null)
  def rawType = RawType.UByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUByte(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUByte(v) }
}

private[data] final class BufferRFloatUByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UByte](shared, null, ro, 0, 1) with DataBuffer[RFloat, UByte] {
  type Read = ReadDataBuffer[RFloat, UByte]

  def mkReadOnlyInstance() = new BufferRFloatUByte(shared, true)
  def rawType = RawType.UByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUByte(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUByte(v)) }
}


// Type: SShort
private[data] final class ArrayRFloatSShort(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRFloat[SShort](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SShort]
with PrimitiveFactory[RFloat, SShort]
{
  type Read = ReadDataArray[RFloat, SShort]

  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRFloatSShort(rarray, null)
  def rawType = RawType.SShort
  def isNormalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSShort(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSShort(v) }
}

private[data] final class BufferRFloatSShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SShort](shared, null, ro, 0, 1) with DataBuffer[RFloat, SShort] {
  type Read = ReadDataBuffer[RFloat, SShort]

  def mkReadOnlyInstance() = new BufferRFloatSShort(shared, true)
  def rawType = RawType.SShort
  def isNormalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSShort(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSShort(v)) }
}


// Type: UShort
private[data] final class ArrayRFloatUShort(
  rarray: Array[Char], warray: Array[Char]
)
extends BaseRFloat[UShort](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UShort]
with PrimitiveFactory[RFloat, UShort]
{
  type Read = ReadDataArray[RFloat, UShort]

  def this() = this(emptyChar, emptyChar)
  def mkReadOnlyInstance() = new ArrayRFloatUShort(rarray, null)
  def rawType = RawType.UShort
  def isNormalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRFloatUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUShort(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUShort(v) }
}

private[data] final class BufferRFloatUShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UShort](shared, null, ro, 0, 1) with DataBuffer[RFloat, UShort] {
  type Read = ReadDataBuffer[RFloat, UShort]

  def mkReadOnlyInstance() = new BufferRFloatUShort(shared, true)
  def rawType = RawType.UShort
  def isNormalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRFloatUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUShort(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUShort(v)) }
}


// Type: SInt
private[data] final class ArrayRFloatSInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRFloat[SInt](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SInt]
with PrimitiveFactory[RFloat, SInt]
{
  type Read = ReadDataArray[RFloat, SInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRFloatSInt(rarray, null)
  def rawType = RawType.SInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSInt(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSInt(v) }
}

private[data] final class BufferRFloatSInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SInt](shared, null, ro, 0, 1) with DataBuffer[RFloat, SInt] {
  type Read = ReadDataBuffer[RFloat, SInt]

  def mkReadOnlyInstance() = new BufferRFloatSInt(shared, true)
  def rawType = RawType.SInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSInt(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSInt(v)) }
}


// Type: UInt
private[data] final class ArrayRFloatUInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRFloat[UInt](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UInt]
with PrimitiveFactory[RFloat, UInt]
{
  type Read = ReadDataArray[RFloat, UInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRFloatUInt(rarray, null)
  def rawType = RawType.UInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUInt(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUInt(v) }
}

private[data] final class BufferRFloatUInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UInt](shared, null, ro, 0, 1) with DataBuffer[RFloat, UInt] {
  type Read = ReadDataBuffer[RFloat, UInt]

  def mkReadOnlyInstance() = new BufferRFloatUInt(shared, true)
  def rawType = RawType.UInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUInt(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUInt(v)) }
}


// Type: HFloat
private[data] final class ArrayRFloatHFloat(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRFloat[HFloat](rarray, null, warray == null, 0, 1) with DataArray[RFloat, HFloat]
with PrimitiveFactory[RFloat, HFloat]
{
  type Read = ReadDataArray[RFloat, HFloat]

  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRFloatHFloat(rarray, null)
  def rawType: Int = RawType.HFloat
  def isNormalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromHFloat(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toHFloat(v) }
}

private[data] final class BufferRFloatHFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[HFloat](shared, null, ro, 0, 1) with DataBuffer[RFloat, HFloat] {
  type Read = ReadDataBuffer[RFloat, HFloat]

  def mkReadOnlyInstance() = new BufferRFloatHFloat(shared, true)
  def rawType: Int = RawType.HFloat
  def isNormalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromHFloat(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toHFloat(v)) }
}


// Type: RFloat
private[data] final class ArrayRFloatRFloat(
  rarray: Array[Float], warray: Array[Float]
)
extends BaseRFloat[RFloat](rarray, null, warray == null, 0, 1) with DataArray[RFloat, RFloat]
with PrimitiveFactory[RFloat, RFloat]
{
  type Read = ReadDataArray[RFloat, RFloat]

  def this() = this(emptyFloat, emptyFloat)
  def mkReadOnlyInstance() = new ArrayRFloatRFloat(rarray, null)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) { warray(i) = v }
}

private[data] final class BufferRFloatRFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[RFloat](shared, null, ro, 0, 1) with DataBuffer[RFloat, RFloat] {
  type Read = ReadDataBuffer[RFloat, RFloat]

  def mkReadOnlyInstance() = new BufferRFloatRFloat(shared, true)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) { buff.put(i, v) }
}
