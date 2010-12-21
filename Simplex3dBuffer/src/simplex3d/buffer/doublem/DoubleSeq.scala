/*
 * Simplex3d, DoubleBuffer module
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
package doublem

import java.nio._
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.buffer.MetaManifest
import simplex3d.buffer.Util._
import simplex3d.buffer.conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseRDouble[+R <: DefinedDouble](
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends BaseSeq[RDouble, Double, Double, R](shared, primitive, ro, off, str)
with CompositionFactory[RDouble, DefinedDouble]
{
  final def elemManifest = MetaManifest.RDouble
  final def readManifest = Manifest.Double
  final def components: Int = 1

  final def mkReadDataArray[P <: DefinedDouble](primitive: ReadDataArray[RDouble, P])
  :ReadDataArray[RDouble, P] = primitive
  final def mkReadDataBuffer[P <: DefinedDouble](primitive: ReadDataBuffer[RDouble, P])
  :ReadDataBuffer[RDouble, P] = primitive
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    primitive: ReadDataBuffer[RDouble, P], off: Int, str: Int
  ) :ReadDataView[RDouble, P] = {
    (primitive.rawType match {
      case RawType.RFloat =>
        new impl.ViewRDoubleRFloat(primitive.asInstanceOf[ReadDataBuffer[RDouble, RFloat]], off, str)
      case _ =>
        new ViewRDouble(primitive, off, str)
    }).asInstanceOf[ReadDataView[RDouble, P]]
  }

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) :ReadDataView[RDouble, R] = {
    (rawType match {
      case RawType.RFloat =>
        val primitive = backing.mkReadDataBuffer(byteBuffer).asInstanceOf[ReadDataBuffer[RDouble, RFloat]]
        new impl.ViewRDoubleRFloat(primitive, off, str)
      case _ =>
        new ViewRDouble(backing.mkReadDataBuffer(byteBuffer), off, str)
    }).asInstanceOf[ReadDataView[RDouble, R]]
  }

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
}

private[buffer] final class ViewRDouble[+R <: DefinedDouble](
  primitive: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseRDouble[R](primitive, primitive, primitive.readOnly, off, str) with DataView[RDouble, R]
{
  final def normalized = backing.normalized
  final def rawType = backing.rawType
  private[buffer] def mkReadOnlyInstance() = new ViewRDouble(backing.asReadOnly(), offset, stride)

  def apply(i: Int) :Double = backing(offset + i*stride)
  def update(i: Int, v: Double) :Unit = backing(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[RDouble, R] =
    backing.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[RDouble, R] =
    backing.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[buffer] sealed abstract class SeqRDoubleSByte(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[SByte](shared, primitive, ro, off, str) {
  final def rawType = RawType.SByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleSByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSByte(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleSByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqRDoubleSByte(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SByte] {
  def this() = this(emptyByte, emptyByte)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSByte(rarray, null)

  def apply(i: Int) :Double = fromSByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSByte(v) }
}

private[buffer] final class BufferRDoubleSByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSByte(shared, null, ro, 0, 1) with DataBuffer[RDouble, SByte] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleSByte(shared, true)

  def apply(i: Int) :Double = fromSByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSByte(v)) }
}


// Type: UByte
private[buffer] sealed abstract class SeqRDoubleUByte(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[UByte](shared, primitive, ro, off, str) {
  final def rawType = RawType.UByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayRDoubleUByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUByte(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleUByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqRDoubleUByte(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UByte] {
  def this() = this(emptyByte, emptyByte)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUByte(rarray, null)

  def apply(i: Int) :Double = fromUByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUByte(v) }
}

private[buffer] final class BufferRDoubleUByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUByte(shared, null, ro, 0, 1) with DataBuffer[RDouble, UByte] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleUByte(shared, true)

  def apply(i: Int) :Double = fromUByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUByte(v)) }
}


// Type: SShort
private[buffer] sealed abstract class SeqRDoubleSShort(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[SShort](shared, primitive, ro, off, str) {
  final def rawType = RawType.SShort
  final def normalized = true

  final def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleSShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSShort(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleSShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqRDoubleSShort(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SShort] {
  def this() = this(emptyShort, emptyShort)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSShort(rarray, null)

  def apply(i: Int) :Double = fromSShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSShort(v) }
}

private[buffer] final class BufferRDoubleSShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSShort(shared, null, ro, 0, 1) with DataBuffer[RDouble, SShort] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleSShort(shared, true)

  def apply(i: Int) :Double = fromSShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSShort(v)) }
}


// Type: UShort
private[buffer] sealed abstract class SeqRDoubleUShort(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[UShort](shared, primitive, ro, off, str) {
  final def rawType = RawType.UShort
  final def normalized = true

  final def mkDataArray(array: Array[Char]) =
    new ArrayRDoubleUShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUShort(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleUShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqRDoubleUShort(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UShort] {
  def this() = this(emptyChar, emptyChar)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUShort(rarray, null)

  def apply(i: Int) :Double = fromUShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUShort(v) }
}

private[buffer] final class BufferRDoubleUShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUShort(shared, null, ro, 0, 1) with DataBuffer[RDouble, UShort] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleUShort(shared, true)

  def apply(i: Int) :Double = fromUShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUShort(v)) }
}


// Type: SInt
private[buffer] sealed abstract class SeqRDoubleSInt(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[SInt](shared, primitive, ro, off, str) {
  final def rawType = RawType.SInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleSInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleSInt(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleSInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqRDoubleSInt(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SInt] {
  def this() = this(emptyInt, emptyInt)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSInt(rarray, null)

  def apply(i: Int) :Double = fromSInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSInt(v) }
}

private[buffer] final class BufferRDoubleSInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSInt(shared, null, ro, 0, 1) with DataBuffer[RDouble, SInt] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleSInt(shared, true)

  def apply(i: Int) :Double = fromSInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSInt(v)) }
}


// Type: UInt
private[buffer] sealed abstract class SeqRDoubleUInt(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[UInt](shared, primitive, ro, off, str) {
  final def rawType = RawType.UInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayRDoubleUInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleUInt(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleUInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqRDoubleUInt(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UInt] {
  def this() = this(emptyInt, emptyInt)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUInt(rarray, null)

  def apply(i: Int) :Double = fromUInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUInt(v) }
}

private[buffer] final class BufferRDoubleUInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUInt(shared, null, ro, 0, 1) with DataBuffer[RDouble, UInt] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleUInt(shared, true)

  def apply(i: Int) :Double = fromUInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUInt(v)) }
}


// Type: HFloat
private[buffer] sealed abstract class SeqRDoubleHFloat(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[HFloat](shared, primitive, ro, off, str) {
  final def rawType: Int = RawType.HFloat
  final def normalized = false

  final def mkDataArray(array: Array[Short]) =
    new ArrayRDoubleHFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleHFloat(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleHFloat(
  rarray: Array[Short], warray: Array[Short]
) extends SeqRDoubleHFloat(rarray, null, warray == null, 0, 1) with DataArray[RDouble, HFloat] {
  def this() = this(emptyShort, emptyShort)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleHFloat(rarray, null)

  def apply(i: Int) :Double = fromHFloat(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toHFloat(v) }
}

private[buffer] final class BufferRDoubleHFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleHFloat(shared, null, ro, 0, 1) with DataBuffer[RDouble, HFloat] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleHFloat(shared, true)

  def apply(i: Int) :Double = fromHFloat(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toHFloat(v)) }
}


// Type: RFloat
private[buffer] abstract class SeqRDoubleRFloat(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[RFloat](shared, primitive, ro, off, str) {
  final def rawType: Int = RawType.RFloat
  final def normalized = false

  final def mkDataArray(array: Array[Float]) =
    new ArrayRDoubleRFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRFloat(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleRFloat(
  rarray: Array[Float], warray: Array[Float]
) extends SeqRDoubleRFloat(rarray, null, warray == null, 0, 1) with DataArray[RDouble, RFloat] {
  def this() = this(emptyFloat, emptyFloat)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleRFloat(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v.toFloat }
}

private[buffer] final class BufferRDoubleRFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleRFloat(shared, null, ro, 0, 1) with DataBuffer[RDouble, RFloat] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleRFloat(shared, true)

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v.toFloat) }
}


// Type: RDouble
private[buffer] sealed abstract class SeqRDoubleRDouble(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseRDouble[RDouble](shared, primitive, ro, off, str) {
  final def rawType: Int = RawType.RDouble
  final def normalized = false

  final def mkDataArray(array: Array[Double]) =
    new ArrayRDoubleRDouble(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferRDoubleRDouble(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArrayRDoubleRDouble(
  rarray: Array[Double], warray: Array[Double]
) extends SeqRDoubleRDouble(rarray, null, warray == null, 0, 1) with DataArray[RDouble, RDouble] {
  def this() = this(emptyDouble, emptyDouble)
  private[buffer] def mkReadOnlyInstance() = new ArrayRDoubleRDouble(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v }
}

private[buffer] final class BufferRDoubleRDouble(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleRDouble(shared, null, ro, 0, 1) with DataBuffer[RDouble, RDouble] {
  private[buffer] def mkReadOnlyInstance() = new BufferRDoubleRDouble(shared, true)

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v) }
}
