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
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.buffer.Util._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseSInt[+R <: DefinedInt](
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
)
extends BaseSeq[SInt, Int, Int, R](shared, primitive, ro, off, str)
with CompositionFactory[SInt, DefinedInt]
{
  final def elemManifest = MetaManifest.SInt
  final def readManifest = Manifest.Int
  final def components: Int = 1
  final def normalized = false

  final def mkReadDataArray[P <: DefinedInt](primitive: ReadDataArray[SInt, P])
  :ReadDataArray[SInt, P] = primitive
  final def mkReadDataBuffer[P <: DefinedInt](primitive: ReadDataBuffer[SInt, P])
  :ReadDataBuffer[SInt, P] = primitive
  protected final def mkReadDataViewInstance[P <: DefinedInt](
    primitive: ReadDataBuffer[SInt, P], off: Int, str: Int
  ) :ReadDataView[SInt, P] = new ViewSInt(primitive, off, str)

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) :ReadDataView[SInt, R] = {
    new ViewSInt(backing.mkReadDataBuffer(byteBuffer), off, str)
  }
  
  override def mkSerializableInstance() = new SerializableIntData(components, rawType)
}

private[buffer] final class ViewSInt[+R <: DefinedInt](
  primitive: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseSInt[R](primitive, primitive, primitive.readOnly, off, str) with DataView[SInt, R]
{
  final def rawType = backing.rawType
  private[buffer] def mkReadOnlyInstance() = new ViewSInt(backing.asReadOnly(), offset, stride)

  def apply(i: Int) :Int = backing(offset + i*stride)
  def update(i: Int, v: Int) :Unit = backing(offset + i*stride) = v

  final def mkDataArray(array: R#Array @uncheckedVariance) :DataArray[SInt, R] =
    backing.mkDataArray(array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[SInt, R] =
    backing.mkReadDataBuffer(byteBuffer)
}


// Type: SByte
private[buffer] sealed abstract class SeqSIntSByte(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[SByte](shared, primitive, ro, off, str) {
  final def rawType = RawType.SByte

  final def mkDataArray(array: Array[Byte]) =
    new ArraySIntSByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSByte(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntSByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqSIntSByte(rarray, null, warray == null, 0, 1) with DataArray[SInt, SByte] {
  def this() = this(emptyByte, emptyByte)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntSByte(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[buffer] final class BufferSIntSByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntSByte(shared, null, ro, 0, 1) with DataBuffer[SInt, SByte] {
  private[buffer] def mkReadOnlyInstance() = new BufferSIntSByte(shared, true)

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toByte)
}


// Type: UByte
private[buffer] sealed abstract class SeqSIntUByte(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[UByte](shared, primitive, ro, off, str) {
  final def rawType = RawType.UByte

  final def mkDataArray(array: Array[Byte]) =
    new ArraySIntUByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUByte(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntUByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqSIntUByte(rarray, null, warray == null, 0, 1) with IndexArray[UByte] {
  def this() = this(emptyByte, emptyByte)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntUByte(rarray, null)

  def apply(i: Int) :Int = rarray(i) & 0xFF
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[buffer] final class BufferSIntUByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntUByte(shared, null, ro, 0, 1) with IndexBuffer[UByte] {
  private[buffer] def mkReadOnlyInstance() = new BufferSIntUByte(shared, true)

  def apply(i: Int) :Int = buff.get(i) & 0xFF
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toByte)
}


// Type: SShort
private[buffer] sealed abstract class SeqSIntSShort(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[SShort](shared, primitive, ro, off, str) {
  final def rawType = RawType.SShort

  final def mkDataArray(array: Array[Short]) =
    new ArraySIntSShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSShort(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntSShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqSIntSShort(rarray, null, warray == null, 0, 1) with DataArray[SInt, SShort] {
  def this() = this(emptyShort, emptyShort)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntSShort(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toShort
}

private[buffer] final class BufferSIntSShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntSShort(shared, null, ro, 0, 1) with DataBuffer[SInt, SShort] {
  private[buffer] def mkReadOnlyInstance() = new BufferSIntSShort(shared, true)

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toShort)
}


// Type: UShort
private[buffer] sealed abstract class SeqSIntUShort(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[UShort](shared, primitive, ro, off, str) {
  final def rawType = RawType.UShort

  final def mkDataArray(array: Array[Char]) =
    new ArraySIntUShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUShort(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntUShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqSIntUShort(rarray, null, warray == null, 0, 1) with IndexArray[UShort] {
  def this() = this(emptyChar, emptyChar)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntUShort(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toChar
}

private[buffer] final class BufferSIntUShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntUShort(shared, null, ro, 0, 1) with IndexBuffer[UShort] {
  private[buffer] def mkReadOnlyInstance() = new BufferSIntUShort(shared, true)

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.toChar)
}


// Type: SInt
private[buffer] sealed abstract class SeqSIntSInt(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[SInt](shared, primitive, ro, off, str) {
  final def rawType = RawType.SInt

  final def mkDataArray(array: Array[Int]) =
    new ArraySIntSInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntSInt(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntSInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqSIntSInt(rarray, null, warray == null, 0, 1) with DataArray[SInt, SInt] {
  def this() = this(emptyInt, emptyInt)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntSInt(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferSIntSInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntSInt(shared, null, ro, 0, 1) with DataBuffer[SInt, SInt]{
  private[buffer] def mkReadOnlyInstance() = new BufferSIntSInt(shared, true)

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}


// Type: UInt
private[buffer] sealed abstract class SeqSIntUInt(
  shared: AnyRef, primitive: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSInt[UInt](shared, primitive, ro, off, str) {
  final def rawType = RawType.UInt

  final def mkDataArray(array: Array[Int]) =
    new ArraySIntUInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferSIntUInt(byteBuffer, byteBuffer.isReadOnly)
  }
}

private[buffer] final class ArraySIntUInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqSIntUInt(rarray, null, warray == null, 0, 1) with IndexArray[UInt] {
  def this() = this(emptyInt, emptyInt)
  private[buffer] def mkReadOnlyInstance() = new ArraySIntUInt(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferSIntUInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqSIntUInt(shared, null, ro, 0, 1) with IndexBuffer[UInt]{
  private[buffer] def mkReadOnlyInstance() = new BufferSIntUInt(shared, true)

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}
