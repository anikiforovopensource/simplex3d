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
import simplex3d.buffer.MetaManifest
import simplex3d.buffer.Util._
import simplex3d.buffer.conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseRDouble[+R <: DefinedDouble](
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSeq[RDouble, Double, Double, R](shared, primitive, ro, off, str) {
  final def elemManifest = MetaManifest.RDouble
  final def readManifest = Manifest.Double
  final def components: Int = 1

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleSByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleSByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqRDoubleSByte(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSByte(rarray, null)

  def apply(i: Int) :Double = fromSByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSByte(v) }
}

private[buffer] final class BufferRDoubleSByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSByte(shared, null, ro, 0, 1) with DataBuffer[RDouble, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleSByte(shared, true)

  def apply(i: Int) :Double = fromSByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSByte(v)) }
}

private[buffer] final class ViewRDoubleSByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleSByte(
  shared, new BufferRDoubleSByte(shared, ro), ro, off, str
) with DataView[RDouble, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleSByte(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSByte(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toSByte(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleUByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleUByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqRDoubleUByte(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUByte(rarray, null)

  def apply(i: Int) :Double = fromUByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUByte(v) }
}

private[buffer] final class BufferRDoubleUByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUByte(shared, null, ro, 0, 1) with DataBuffer[RDouble, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleUByte(shared, true)

  def apply(i: Int) :Double = fromUByte(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUByte(v)) }
}

private[buffer] final class ViewRDoubleUByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleUByte(
  shared, new BufferRDoubleUByte(shared, ro), ro, off, str
) with DataView[RDouble, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleUByte(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUByte(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toUByte(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleSShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleSShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqRDoubleSShort(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SShort] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSShort(rarray, null)

  def apply(i: Int) :Double = fromSShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSShort(v) }
}

private[buffer] final class BufferRDoubleSShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSShort(shared, null, ro, 0, 1) with DataBuffer[RDouble, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleSShort(shared, true)

  def apply(i: Int) :Double = fromSShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSShort(v)) }
}

private[buffer] final class ViewRDoubleSShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleSShort(
  shared, new BufferRDoubleSShort(shared, ro), ro, off, str
) with DataView[RDouble, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleSShort(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSShort(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toSShort(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleUShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleUShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqRDoubleUShort(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UShort] {
  def this() = this(emptyChar, emptyChar)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUShort(rarray, null)

  def apply(i: Int) :Double = fromUShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUShort(v) }
}

private[buffer] final class BufferRDoubleUShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUShort(shared, null, ro, 0, 1) with DataBuffer[RDouble, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleUShort(shared, true)

  def apply(i: Int) :Double = fromUShort(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUShort(v)) }
}

private[buffer] final class ViewRDoubleUShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleUShort(
  shared, new BufferRDoubleUShort(shared, ro), ro, off, str
) with DataView[RDouble, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleUShort(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUShort(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toUShort(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleSInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleSInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqRDoubleSInt(rarray, null, warray == null, 0, 1) with DataArray[RDouble, SInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleSInt(rarray, null)

  def apply(i: Int) :Double = fromSInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSInt(v) }
}

private[buffer] final class BufferRDoubleSInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleSInt(shared, null, ro, 0, 1) with DataBuffer[RDouble, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleSInt(shared, true)

  def apply(i: Int) :Double = fromSInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toSInt(v)) }
}

private[buffer] final class ViewRDoubleSInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleSInt(
  shared, new BufferRDoubleSInt(shared, ro), ro, off, str
) with DataView[RDouble, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleSInt(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSInt(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toSInt(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleUInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleUInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqRDoubleUInt(rarray, null, warray == null, 0, 1) with DataArray[RDouble, UInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleUInt(rarray, null)

  def apply(i: Int) :Double = fromUInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUInt(v) }
}

private[buffer] final class BufferRDoubleUInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleUInt(shared, null, ro, 0, 1) with DataBuffer[RDouble, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleUInt(shared, true)

  def apply(i: Int) :Double = fromUInt(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toUInt(v)) }
}

private[buffer] final class ViewRDoubleUInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleUInt(
  shared, new BufferRDoubleUInt(shared, ro), ro, off, str
) with DataView[RDouble, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleUInt(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUInt(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toUInt(v)) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleHFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleHFloat(
  rarray: Array[Short], warray: Array[Short]
) extends SeqRDoubleHFloat(rarray, null, warray == null, 0, 1) with DataArray[RDouble, HFloat] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleHFloat(rarray, null)

  def apply(i: Int) :Double = fromHFloat(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toHFloat(v) }
}

private[buffer] final class BufferRDoubleHFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleHFloat(shared, null, ro, 0, 1) with DataBuffer[RDouble, HFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleHFloat(shared, true)

  def apply(i: Int) :Double = fromHFloat(buff.get(i))
  def update(i: Int, v: Double) { buff.put(i, toHFloat(v)) }
}

private[buffer] final class ViewRDoubleHFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleHFloat(
  shared, new BufferRDoubleHFloat(shared, ro), ro, off, str
) with DataView[RDouble, HFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleHFloat(shared, true, offset, stride)

  def apply(i: Int) :Double = fromHFloat(buff.get(offset + i*stride))
  def update(i: Int, v: Double) { buff.put(offset + i*stride, toHFloat(v)) }
}


// Type: RFloat
private[buffer] sealed abstract class SeqRDoubleRFloat(
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleRFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleRFloat(
  rarray: Array[Float], warray: Array[Float]
) extends SeqRDoubleRFloat(rarray, null, warray == null, 0, 1) with DataArray[RDouble, RFloat] {
  def this() = this(emptyFloat, emptyFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleRFloat(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v.toFloat }
}

private[buffer] final class BufferRDoubleRFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleRFloat(shared, null, ro, 0, 1) with DataBuffer[RDouble, RFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleRFloat(shared, true)

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v.toFloat) }
}

private[buffer] final class ViewRDoubleRFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleRFloat(
  shared, new BufferRDoubleRFloat(shared, ro), ro, off, str
) with DataView[RDouble, RFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleRFloat(shared, true, offset, stride)

  def apply(i: Int) :Double = buff.get(offset + i*stride)
  def update(i: Int, v: Double) { buff.put(offset + i*stride, v.toFloat) }
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
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewRDoubleRDouble(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayRDoubleRDouble(
  rarray: Array[Double], warray: Array[Double]
) extends SeqRDoubleRDouble(rarray, null, warray == null, 0, 1) with DataArray[RDouble, RDouble] {
  def this() = this(emptyDouble, emptyDouble)
  protected[buffer] def mkReadOnlyInstance() = new ArrayRDoubleRDouble(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v }
}

private[buffer] final class BufferRDoubleRDouble(
  shared: ByteBuffer, ro: Boolean
) extends SeqRDoubleRDouble(shared, null, ro, 0, 1) with DataBuffer[RDouble, RDouble] {
  protected[buffer] def mkReadOnlyInstance() = new BufferRDoubleRDouble(shared, true)

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) { buff.put(i, v) }
}

private[buffer] final class ViewRDoubleRDouble(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqRDoubleRDouble(
  shared, new BufferRDoubleRDouble(shared, ro), ro, off, str
) with DataView[RDouble, RDouble] {
  protected[buffer] def mkReadOnlyInstance() = new ViewRDoubleRDouble(shared, true, offset, stride)

  def apply(i: Int) :Double = buff.get(offset + i*stride)
  def update(i: Int, v: Double) { buff.put(offset + i*stride, v) }
}
