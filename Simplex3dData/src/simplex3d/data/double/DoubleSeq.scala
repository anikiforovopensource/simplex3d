/*
 * Simplex3d, DoubleData module
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
package double

import java.nio._
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.data.PrimitiveFormat
import simplex3d.data.Util._
import simplex3d.data.conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseRDouble[+R <: DefinedDouble](
  shared: AnyRef, prim: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends AbstractData[RDouble, Double, Double, R](shared, prim, ro, off, str)
with CompositionFactory[RDouble, DefinedDouble]
{
  final def formatManifest = PrimitiveFormat.RDouble
  final def readManifest = Manifest.Double
  final def components: Int = 1

  final def mkReadDataArray[P <: DefinedDouble](prim: ReadDataArray[RDouble, P])
  :ReadDataArray[RDouble, P] = prim
  final def mkReadDataBuffer[P <: DefinedDouble](prim: ReadDataBuffer[RDouble, P])
  :ReadDataBuffer[RDouble, P] = prim
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    prim: ReadDataBuffer[RDouble, P], off: Int, str: Int
  ) :ReadDataView[RDouble, P] = {
    (prim.rawType match {
      case RawType.RFloat =>
        new impl.ViewRDoubleRFloat(prim.asInstanceOf[ReadDataBuffer[RDouble, RFloat]], off, str)
      case _ =>
        new ViewRDouble(prim, off, str)
    }).asInstanceOf[ReadDataView[RDouble, P]]
  }

  protected final def mkReadDataViewInstance(
    byteBuffer: ByteBuffer, off: Int, str: Int
  ) :ReadDataView[RDouble, R] = {
    (rawType match {
      case RawType.RFloat =>
        val prim = primitives.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[RDouble, RFloat]]
        new impl.ViewRDoubleRFloat(prim, off, str)
      case _ =>
        new ViewRDouble(primitives.mkReadDataBuffer(byteBuffer), off, str)
    }).asInstanceOf[ReadDataView[RDouble, R]]
  }

  final override def mkSerializableInstance() = new PrimitiveRDouble(rawType)
}

private[data] final class ViewRDouble[+R <: DefinedDouble](
  prim: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseRDouble[R](prim, prim, prim.isReadOnly, off, str) with DataView[RDouble, R]
{
  type Read = ReadDataView[RDouble, R @uncheckedVariance]
  
  final def isNormalized = primitives.isNormalized
  final def rawType = primitives.rawType
  def mkReadOnlyInstance() = new ViewRDouble(primitives.asReadOnly(), offset, stride)

  def apply(i: Int) :Double = primitives(offset + i*stride)
  def update(i: Int, v: Double) :Unit = primitives(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[RDouble, R] =
    primitives.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[RDouble, R] =
    primitives.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[data] final class ArrayRDoubleSByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRDouble[SByte](rarray, null, warray == null, 0, 1) with DataArray[RDouble, SByte]
with PrimitiveFactory[RDouble, SByte]
{
  type Read = ReadDataArray[RDouble, SByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRDoubleSByte(rarray, null)
  def rawType = RawType.SByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSByte(v) }
}

private[data] final class BufferRDoubleSByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[SByte](shared, null, ro, 0, 1) with DataBuffer[RDouble, SByte] {
  type Read = ReadDataBuffer[RDouble, SByte]

  def mkReadOnlyInstance() = new BufferRDoubleSByte(shared, true)
  def rawType = RawType.SByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSByte(v)) }
}


// Type: UByte
private[data] final class ArrayRDoubleUByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRDouble[UByte](rarray, null, warray == null, 0, 1) with DataArray[RDouble, UByte]
with PrimitiveFactory[RDouble, UByte]
{
  type Read = ReadDataArray[RDouble, UByte]

  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRDoubleUByte(rarray, null)
  def rawType = RawType.UByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUByte(v) }
}

private[data] final class BufferRDoubleUByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[UByte](shared, null, ro, 0, 1) with DataBuffer[RDouble, UByte] {
  type Read = ReadDataBuffer[RDouble, UByte]

  def mkReadOnlyInstance() = new BufferRDoubleUByte(shared, true)
  def rawType = RawType.UByte
  def isNormalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUByte(v)) }
}


// Type: SShort
private[data] final class ArrayRDoubleSShort(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRDouble[SShort](rarray, null, warray == null, 0, 1) with DataArray[RDouble, SShort]
with PrimitiveFactory[RDouble, SShort]
{
  type Read = ReadDataArray[RDouble, SShort]

  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRDoubleSShort(rarray, null)
  def rawType = RawType.SShort
  def isNormalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSShort(v) }
}

private[data] final class BufferRDoubleSShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[SShort](shared, null, ro, 0, 1) with DataBuffer[RDouble, SShort] {
  type Read = ReadDataBuffer[RDouble, SShort]

  def mkReadOnlyInstance() = new BufferRDoubleSShort(shared, true)
  def rawType = RawType.SShort
  def isNormalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSShort(v)) }
}


// Type: UShort
private[data] final class ArrayRDoubleUShort(
  rarray: Array[Char], warray: Array[Char]
)
extends BaseRDouble[UShort](rarray, null, warray == null, 0, 1) with DataArray[RDouble, UShort]
with PrimitiveFactory[RDouble, UShort]
{
  type Read = ReadDataArray[RDouble, UShort]

  def this() = this(emptyChar, emptyChar)
  def mkReadOnlyInstance() = new ArrayRDoubleUShort(rarray, null)
  def rawType = RawType.UShort
  def isNormalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRDoubleUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUShort(v) }
}

private[data] final class BufferRDoubleUShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[UShort](shared, null, ro, 0, 1) with DataBuffer[RDouble, UShort] {
  type Read = ReadDataBuffer[RDouble, UShort]

  def mkReadOnlyInstance() = new BufferRDoubleUShort(shared, true)
  def rawType = RawType.UShort
  def isNormalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRDoubleUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUShort(v)) }
}


// Type: SInt
private[data] final class ArrayRDoubleSInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRDouble[SInt](rarray, null, warray == null, 0, 1) with DataArray[RDouble, SInt]
with PrimitiveFactory[RDouble, SInt]
{
  type Read = ReadDataArray[RDouble, SInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRDoubleSInt(rarray, null)
  def rawType = RawType.SInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSInt(v) }
}

private[data] final class BufferRDoubleSInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[SInt](shared, null, ro, 0, 1) with DataBuffer[RDouble, SInt] {
  type Read = ReadDataBuffer[RDouble, SInt]

  def mkReadOnlyInstance() = new BufferRDoubleSInt(shared, true)
  def rawType = RawType.SInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromSInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSInt(v)) }
}


// Type: UInt
private[data] final class ArrayRDoubleUInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRDouble[UInt](rarray, null, warray == null, 0, 1) with DataArray[RDouble, UInt]
with PrimitiveFactory[RDouble, UInt]
{
  type Read = ReadDataArray[RDouble, UInt]

  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRDoubleUInt(rarray, null)
  def rawType = RawType.UInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUInt(v) }
}

private[data] final class BufferRDoubleUInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[UInt](shared, null, ro, 0, 1) with DataBuffer[RDouble, UInt] {
  type Read = ReadDataBuffer[RDouble, UInt]

  def mkReadOnlyInstance() = new BufferRDoubleUInt(shared, true)
  def rawType = RawType.UInt
  def isNormalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromUInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUInt(v)) }
}


// Type: HFloat
private[data] final class ArrayRDoubleHFloat(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRDouble[HFloat](rarray, null, warray == null, 0, 1) with DataArray[RDouble, HFloat]
with PrimitiveFactory[RDouble, HFloat]
{
  type Read = ReadDataArray[RDouble, HFloat]

  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRDoubleHFloat(rarray, null)
  def rawType: Int = RawType.HFloat
  def isNormalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromHFloat(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toHFloat(v) }
}

private[data] final class BufferRDoubleHFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[HFloat](shared, null, ro, 0, 1) with DataBuffer[RDouble, HFloat] {
  type Read = ReadDataBuffer[RDouble, HFloat]

  def mkReadOnlyInstance() = new BufferRDoubleHFloat(shared, true)
  def rawType: Int = RawType.HFloat
  def isNormalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = fromHFloat(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toHFloat(v)) }
}


// Type: RFloat
private[data] final class ArrayRDoubleRFloat(
  rarray: Array[Float], warray: Array[Float]
)
extends BaseRDouble[RFloat](rarray, null, warray == null, 0, 1) with DataArray[RDouble, RFloat]
with PrimitiveFactory[RDouble, RFloat]
{
  type Read = ReadDataArray[RDouble, RFloat]

  def this() = this(emptyFloat, emptyFloat)
  def mkReadOnlyInstance() = new ArrayRDoubleRFloat(rarray, null)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRDoubleRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v.toFloat }
}

private[data] final class BufferRDoubleRFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[RFloat](shared, null, ro, 0, 1) with DataBuffer[RDouble, RFloat] {
  type Read = ReadDataBuffer[RDouble, RFloat]

  def mkReadOnlyInstance() = new BufferRDoubleRFloat(shared, true)
  def rawType: Int = RawType.RFloat
  def isNormalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRDoubleRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v.toFloat) }
}


// Type: RDouble
private[data] final class ArrayRDoubleRDouble(
  rarray: Array[Double], warray: Array[Double]
)
extends BaseRDouble[RDouble](rarray, null, warray == null, 0, 1) with DataArray[RDouble, RDouble]
with PrimitiveFactory[RDouble, RDouble]
{
  type Read = ReadDataArray[RDouble, RDouble]

  def this() = this(emptyDouble, emptyDouble)
  def mkReadOnlyInstance() = new ArrayRDoubleRDouble(rarray, null)
  def rawType: Int = RawType.RDouble
  def isNormalized = false

  def mkDataArray(array: Array[Double]) =
    new ArrayRDoubleRDouble(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRDouble(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v }
}

private[data] final class BufferRDoubleRDouble(
  shared: ByteBuffer, ro: Boolean
) extends BaseRDouble[RDouble](shared, null, ro, 0, 1) with DataBuffer[RDouble, RDouble] {
  type Read = ReadDataBuffer[RDouble, RDouble]

  def mkReadOnlyInstance() = new BufferRDoubleRDouble(shared, true)
  def rawType: Int = RawType.RDouble
  def isNormalized = false

  def mkDataArray(array: Array[Double]) =
    new ArrayRDoubleRDouble(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRDouble(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v) }
}
