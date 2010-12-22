/*
 * Simplex3d, FloatData module
 * Copyright (C) 2010, Simplex3d Team
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
package floatm

import java.nio._
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.data.MetaManifest
import simplex3d.data.Util._
import simplex3d.data.conversion.Float._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseRFloat[+R <: DefinedFloat](
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends BaseSeq[RFloat, Float, Float, R](shared, primitive, ro, off, str)
with CompositionFactory[RFloat, DefinedFloat]
{
  final def elemManifest = MetaManifest.RFloat
  final def readManifest = Manifest.Float
  final def components: Int = 1
  
  final def mkReadDataArray[P <: DefinedFloat](primitive: ReadDataArray[RFloat, P])
  :ReadDataArray[RFloat, P] = primitive
  final def mkReadDataBuffer[P <: DefinedFloat](primitive: ReadDataBuffer[RFloat, P])
  :ReadDataBuffer[RFloat, P] = primitive
  protected final def mkReadDataViewInstance[P <: DefinedFloat](
    primitive: ReadDataBuffer[RFloat, P], off: Int, str: Int
  ) :ReadDataView[RFloat, P] = {
    (primitive.rawType match {
      case RawType.RFloat =>
        new impl.ViewRFloatRFloat(primitive.asInstanceOf[ReadDataBuffer[RFloat, RFloat]], off, str)
      case _ =>
        new ViewRFloat(primitive, off, str)
    }).asInstanceOf[ReadDataView[RFloat, P]]
  }

  protected final def mkReadDataViewInstance(
    byteBuffer: ByteBuffer, off: Int, str: Int
  ) :ReadDataView[RFloat, R] = {
    (rawType match {
      case RawType.RFloat =>
        val primitive = backing.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[RFloat, RFloat]]
        new impl.ViewRFloatRFloat(primitive, off, str)
      case _ =>
        new ViewRFloat(backing.mkReadDataBuffer(byteBuffer), off, str)
    }).asInstanceOf[ReadDataView[RFloat, R]]
  }
  
  final override def mkSerializableInstance() = new PrimitiveRFloat(rawType)
}

private[buffer] final class ViewRFloat[+R <: DefinedFloat](
  primitive: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseRFloat[R](primitive, primitive, primitive.readOnly, off, str) with DataView[RFloat, R] {
  final def normalized = backing.normalized
  final def rawType = backing.rawType
  def mkReadOnlyInstance() = new ViewRFloat(backing.asReadOnly(), offset, stride)

  def apply(i: Int) :Float = backing(offset + i*stride)
  def update(i: Int, v: Float) :Unit = backing(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[RFloat, R] =
    backing.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[RFloat, R] =
    backing.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[buffer] final class ArrayRFloatSByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRFloat[SByte](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SByte]
with PrimitiveFactory[RFloat, SByte]
{
  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRFloatSByte(rarray, null)
  def rawType = RawType.SByte
  def normalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSByte(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSByte(v) }
}

private[buffer] final class BufferRFloatSByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SByte](shared, null, ro, 0, 1) with DataBuffer[RFloat, SByte] {
  def mkReadOnlyInstance() = new BufferRFloatSByte(shared, true)
  def rawType = RawType.SByte
  def normalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatSByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSByte(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSByte(v)) }
}


// Type: UByte
private[buffer] final class ArrayRFloatUByte(
  rarray: Array[Byte], warray: Array[Byte]
)
extends BaseRFloat[UByte](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UByte]
with PrimitiveFactory[RFloat, UByte]
{
  def this() = this(emptyByte, emptyByte)
  def mkReadOnlyInstance() = new ArrayRFloatUByte(rarray, null)
  def rawType = RawType.UByte
  def normalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUByte(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUByte(v) }
}

private[buffer] final class BufferRFloatUByte(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UByte](shared, null, ro, 0, 1) with DataBuffer[RFloat, UByte] {
  def mkReadOnlyInstance() = new BufferRFloatUByte(shared, true)
  def rawType = RawType.UByte
  def normalized = true

  def mkDataArray(array: Array[Byte]) =
    new ArrayRFloatUByte(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUByte(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUByte(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUByte(v)) }
}


// Type: SShort
private[buffer] final class ArrayRFloatSShort(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRFloat[SShort](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SShort]
with PrimitiveFactory[RFloat, SShort]
{
  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRFloatSShort(rarray, null)
  def rawType = RawType.SShort
  def normalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSShort(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSShort(v) }
}

private[buffer] final class BufferRFloatSShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SShort](shared, null, ro, 0, 1) with DataBuffer[RFloat, SShort] {
  def mkReadOnlyInstance() = new BufferRFloatSShort(shared, true)
  def rawType = RawType.SShort
  def normalized = true

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatSShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSShort(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSShort(v)) }
}


// Type: UShort
private[buffer] final class ArrayRFloatUShort(
  rarray: Array[Char], warray: Array[Char]
)
extends BaseRFloat[UShort](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UShort]
with PrimitiveFactory[RFloat, UShort]
{
  def this() = this(emptyChar, emptyChar)
  def mkReadOnlyInstance() = new ArrayRFloatUShort(rarray, null)
  def rawType = RawType.UShort
  def normalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRFloatUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUShort(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUShort(v) }
}

private[buffer] final class BufferRFloatUShort(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UShort](shared, null, ro, 0, 1) with DataBuffer[RFloat, UShort] {
  def mkReadOnlyInstance() = new BufferRFloatUShort(shared, true)
  def rawType = RawType.UShort
  def normalized = true

  def mkDataArray(array: Array[Char]) =
    new ArrayRFloatUShort(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUShort(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUShort(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUShort(v)) }
}


// Type: SInt
private[buffer] final class ArrayRFloatSInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRFloat[SInt](rarray, null, warray == null, 0, 1) with DataArray[RFloat, SInt]
with PrimitiveFactory[RFloat, SInt]
{
  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRFloatSInt(rarray, null)
  def rawType = RawType.SInt
  def normalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSInt(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toSInt(v) }
}

private[buffer] final class BufferRFloatSInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[SInt](shared, null, ro, 0, 1) with DataBuffer[RFloat, SInt] {
  def mkReadOnlyInstance() = new BufferRFloatSInt(shared, true)
  def rawType = RawType.SInt
  def normalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatSInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatSInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromSInt(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toSInt(v)) }
}


// Type: UInt
private[buffer] final class ArrayRFloatUInt(
  rarray: Array[Int], warray: Array[Int]
)
extends BaseRFloat[UInt](rarray, null, warray == null, 0, 1) with DataArray[RFloat, UInt]
with PrimitiveFactory[RFloat, UInt]
{
  def this() = this(emptyInt, emptyInt)
  def mkReadOnlyInstance() = new ArrayRFloatUInt(rarray, null)
  def rawType = RawType.UInt
  def normalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUInt(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toUInt(v) }
}

private[buffer] final class BufferRFloatUInt(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[UInt](shared, null, ro, 0, 1) with DataBuffer[RFloat, UInt] {
  def mkReadOnlyInstance() = new BufferRFloatUInt(shared, true)
  def rawType = RawType.UInt
  def normalized = true

  def mkDataArray(array: Array[Int]) =
    new ArrayRFloatUInt(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatUInt(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromUInt(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toUInt(v)) }
}


// Type: HFloat
private[buffer] final class ArrayRFloatHFloat(
  rarray: Array[Short], warray: Array[Short]
)
extends BaseRFloat[HFloat](rarray, null, warray == null, 0, 1) with DataArray[RFloat, HFloat]
with PrimitiveFactory[RFloat, HFloat]
{
  def this() = this(emptyShort, emptyShort)
  def mkReadOnlyInstance() = new ArrayRFloatHFloat(rarray, null)
  def rawType: Int = RawType.HFloat
  def normalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromHFloat(rarray(i))
  def update(i: Int, v: Float) { warray(i) = toHFloat(v) }
}

private[buffer] final class BufferRFloatHFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[HFloat](shared, null, ro, 0, 1) with DataBuffer[RFloat, HFloat] {
  def mkReadOnlyInstance() = new BufferRFloatHFloat(shared, true)
  def rawType: Int = RawType.HFloat
  def normalized = false

  def mkDataArray(array: Array[Short]) =
    new ArrayRFloatHFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatHFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = fromHFloat(buff.get(i))
  def update(i: Int, v: Float) { buff.put(i, toHFloat(v)) }
}


// Type: RFloat
private[buffer] final class ArrayRFloatRFloat(
  rarray: Array[Float], warray: Array[Float]
)
extends BaseRFloat[RFloat](rarray, null, warray == null, 0, 1) with DataArray[RFloat, RFloat]
with PrimitiveFactory[RFloat, RFloat]
{
  def this() = this(emptyFloat, emptyFloat)
  def mkReadOnlyInstance() = new ArrayRFloatRFloat(rarray, null)
  def rawType: Int = RawType.RFloat
  def normalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) { warray(i) = v }
}

private[buffer] final class BufferRFloatRFloat(
  shared: ByteBuffer, ro: Boolean
) extends BaseRFloat[RFloat](shared, null, ro, 0, 1) with DataBuffer[RFloat, RFloat] {
  def mkReadOnlyInstance() = new BufferRFloatRFloat(shared, true)
  def rawType: Int = RawType.RFloat
  def normalized = false

  def mkDataArray(array: Array[Float]) =
    new ArrayRFloatRFloat(array, array)
  def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRFloatRFloat(byteBuffer, byteBuffer.isReadOnly)
  }

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) { buff.put(i, v) }
}
