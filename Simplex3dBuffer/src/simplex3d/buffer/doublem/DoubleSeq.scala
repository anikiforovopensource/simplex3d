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
import scala.reflect.Manifest
import simplex3d.math.doublem.DoubleMath._
import simplex3d.buffer.Util._
import simplex3d.buffer.conversion.Double._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseDouble1[+R <: DefinedDouble](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSeq[Double1, Double, Double, R](shared, backing, ro, off, str) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Double
  final def components: Int = 1

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
}


// Type: SByte
private[buffer] sealed abstract class SeqDouble1SByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[SByte](shared, backing, ro, off, str) {
  final def rawType = RawType.SByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayDouble1SByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1SByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1SByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1SByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqDouble1SByte(rarray, null, warray == null, 0, 1) with DataArray[Double1, SByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SByte(rarray, null)

  def apply(i: Int) :Double = fromSByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSByte(v) }
}

private[buffer] final class BufferDouble1SByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1SByte(shared, null, ro, 0, 1) with DataBuffer[Double1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SByte(shared, true)

  def apply(i: Int) :Double = fromSByte(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toSByte(v)) }
}

private[buffer] final class ViewDouble1SByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1SByte(
  shared, new BufferDouble1SByte(shared, ro), ro, off, str
) with DataView[Double1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SByte(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSByte(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toSByte(v)) }
}


// Type: UByte
private[buffer] sealed abstract class SeqDouble1UByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[UByte](shared, backing, ro, off, str) {
  final def rawType = RawType.UByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayDouble1UByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1UByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1UByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1UByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqDouble1UByte(rarray, null, warray == null, 0, 1) with DataArray[Double1, UByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UByte(rarray, null)

  def apply(i: Int) :Double = fromUByte(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUByte(v) }
}

private[buffer] final class BufferDouble1UByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1UByte(shared, null, ro, 0, 1) with DataBuffer[Double1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UByte(shared, true)

  def apply(i: Int) :Double = fromUByte(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toUByte(v)) }
}

private[buffer] final class ViewDouble1UByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1UByte(
  shared, new BufferDouble1UByte(shared, ro), ro, off, str
) with DataView[Double1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UByte(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUByte(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toUByte(v)) }
}


// Type: SShort
private[buffer] sealed abstract class SeqDouble1SShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[SShort](shared, backing, ro, off, str) {
  final def rawType = RawType.SShort
  final def normalized = true

  final def mkDataArray(array: Array[Short]) =
    new ArrayDouble1SShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1SShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1SShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1SShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqDouble1SShort(rarray, null, warray == null, 0, 1) with DataArray[Double1, SShort] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SShort(rarray, null)

  def apply(i: Int) :Double = fromSShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSShort(v) }
}

private[buffer] final class BufferDouble1SShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1SShort(shared, null, ro, 0, 1) with DataBuffer[Double1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SShort(shared, true)

  def apply(i: Int) :Double = fromSShort(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toSShort(v)) }
}

private[buffer] final class ViewDouble1SShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1SShort(
  shared, new BufferDouble1SShort(shared, ro), ro, off, str
) with DataView[Double1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SShort(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSShort(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toSShort(v)) }
}


// Type: UShort
private[buffer] sealed abstract class SeqDouble1UShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[UShort](shared, backing, ro, off, str) {
  final def rawType = RawType.UShort
  final def normalized = true

  final def mkDataArray(array: Array[Char]) =
    new ArrayDouble1UShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1UShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1UShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1UShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqDouble1UShort(rarray, null, warray == null, 0, 1) with DataArray[Double1, UShort] {
  def this() = this(emptyChar, emptyChar)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UShort(rarray, null)

  def apply(i: Int) :Double = fromUShort(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUShort(v) }
}

private[buffer] final class BufferDouble1UShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1UShort(shared, null, ro, 0, 1) with DataBuffer[Double1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UShort(shared, true)

  def apply(i: Int) :Double = fromUShort(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toUShort(v)) }
}

private[buffer] final class ViewDouble1UShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1UShort(
  shared, new BufferDouble1UShort(shared, ro), ro, off, str
) with DataView[Double1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UShort(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUShort(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toUShort(v)) }
}


// Type: SInt
private[buffer] sealed abstract class SeqDouble1SInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[SInt](shared, backing, ro, off, str) {
  final def rawType = RawType.SInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayDouble1SInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1SInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1SInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1SInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqDouble1SInt(rarray, null, warray == null, 0, 1) with DataArray[Double1, SInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SInt(rarray, null)

  def apply(i: Int) :Double = fromSInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toSInt(v) }
}

private[buffer] final class BufferDouble1SInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1SInt(shared, null, ro, 0, 1) with DataBuffer[Double1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SInt(shared, true)

  def apply(i: Int) :Double = fromSInt(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toSInt(v)) }
}

private[buffer] final class ViewDouble1SInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1SInt(
  shared, new BufferDouble1SInt(shared, ro), ro, off, str
) with DataView[Double1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SInt(shared, true, offset, stride)

  def apply(i: Int) :Double = fromSInt(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toSInt(v)) }
}


// Type: UInt
private[buffer] sealed abstract class SeqDouble1UInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[UInt](shared, backing, ro, off, str) {
  final def rawType = RawType.UInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayDouble1UInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1UInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1UInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1UInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqDouble1UInt(rarray, null, warray == null, 0, 1) with DataArray[Double1, UInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UInt(rarray, null)

  def apply(i: Int) :Double = fromUInt(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toUInt(v) }
}

private[buffer] final class BufferDouble1UInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1UInt(shared, null, ro, 0, 1) with DataBuffer[Double1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UInt(shared, true)

  def apply(i: Int) :Double = fromUInt(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toUInt(v)) }
}

private[buffer] final class ViewDouble1UInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1UInt(
  shared, new BufferDouble1UInt(shared, ro), ro, off, str
) with DataView[Double1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UInt(shared, true, offset, stride)

  def apply(i: Int) :Double = fromUInt(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toUInt(v)) }
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqDouble1HalfFloat(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[HalfFloat](shared, backing, ro, off, str) {
  final def rawType: Int = RawType.HalfFloat
  final def normalized = false

  final def mkDataArray(array: Array[Short]) =
    new ArrayDouble1HalfFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1HalfFloat(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1HalfFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1HalfFloat(
  rarray: Array[Short], warray: Array[Short]
) extends SeqDouble1HalfFloat(rarray, null, warray == null, 0, 1) with DataArray[Double1, HalfFloat] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1HalfFloat(rarray, null)

  def apply(i: Int) :Double = fromHalfFloat(rarray(i))
  def update(i: Int, v: Double) { warray(i) = toHalfFloat(v) }
}

private[buffer] final class BufferDouble1HalfFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1HalfFloat(shared, null, ro, 0, 1) with DataBuffer[Double1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1HalfFloat(shared, true)

  def apply(i: Int) :Double = fromHalfFloat(buffer.get(i))
  def update(i: Int, v: Double) { buffer.put(i, toHalfFloat(v)) }
}

private[buffer] final class ViewDouble1HalfFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1HalfFloat(
  shared, new BufferDouble1HalfFloat(shared, ro), ro, off, str
) with DataView[Double1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1HalfFloat(shared, true, offset, stride)

  def apply(i: Int) :Double = fromHalfFloat(buffer.get(offset + i*stride))
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, toHalfFloat(v)) }
}


// Type: RawFloat
private[buffer] sealed abstract class SeqDouble1RawFloat(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[RawFloat](shared, backing, ro, off, str) {
  final def rawType: Int = RawType.RawFloat
  final def normalized = false

  final def mkDataArray(array: Array[Float]) =
    new ArrayDouble1RawFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1RawFloat(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1RawFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1RawFloat(
  rarray: Array[Float], warray: Array[Float]
) extends SeqDouble1RawFloat(rarray, null, warray == null, 0, 1) with DataArray[Double1, RawFloat] {
  def this() = this(emptyFloat, emptyFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1RawFloat(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v.toFloat }
}

private[buffer] final class BufferDouble1RawFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1RawFloat(shared, null, ro, 0, 1) with DataBuffer[Double1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1RawFloat(shared, true)

  def apply(i: Int) :Double = buffer.get(i)
  def update(i: Int, v: Double) { buffer.put(i, v.toFloat) }
}

private[buffer] final class ViewDouble1RawFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1RawFloat(
  shared, new BufferDouble1RawFloat(shared, ro), ro, off, str
) with DataView[Double1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1RawFloat(shared, true, offset, stride)

  def apply(i: Int) :Double = buffer.get(offset + i*stride)
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, v.toFloat) }
}


// Type: RawDouble
private[buffer] sealed abstract class SeqDouble1RawDouble(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseDouble1[RawDouble](shared, backing, ro, off, str) {
  final def rawType: Int = RawType.RawDouble
  final def normalized = false

  final def mkDataArray(array: Array[Double]) =
    new ArrayDouble1RawDouble(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferDouble1RawDouble(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewDouble1RawDouble(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayDouble1RawDouble(
  rarray: Array[Double], warray: Array[Double]
) extends SeqDouble1RawDouble(rarray, null, warray == null, 0, 1) with DataArray[Double1, RawDouble] {
  def this() = this(emptyDouble, emptyDouble)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1RawDouble(rarray, null)

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) { warray(i) = v }
}

private[buffer] final class BufferDouble1RawDouble(
  shared: ByteBuffer, ro: Boolean
) extends SeqDouble1RawDouble(shared, null, ro, 0, 1) with DataBuffer[Double1, RawDouble] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1RawDouble(shared, true)

  def apply(i: Int) :Double = buffer.get(i)
  def update(i: Int, v: Double) { buffer.put(i, v) }
}

private[buffer] final class ViewDouble1RawDouble(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqDouble1RawDouble(
  shared, new BufferDouble1RawDouble(shared, ro), ro, off, str
) with DataView[Double1, RawDouble] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1RawDouble(shared, true, offset, stride)

  def apply(i: Int) :Double = buffer.get(offset + i*stride)
  def update(i: Int, v: Double) { buffer.put(offset + i*stride, v) }
}
