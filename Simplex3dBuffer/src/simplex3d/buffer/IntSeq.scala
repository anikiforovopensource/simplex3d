/*
 * Simplex3d, BaseBuffer module
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
import scala.reflect.Manifest
import simplex3d.buffer.{allocateDirectBuffer => alloc}
import simplex3d.buffer.Util._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseInt1[+R <: DefinedInt](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSeq[Int1, Int, Int, R](shared, backing, ro, off, str) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Int
  final def components: Int = 1
}


// Type: UByte
private[buffer] sealed abstract class SeqInt1UByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[UByte](shared, backing, ro, off, str) {
  final def rawType = RawType.UByte
  final def normalized = false

  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1UByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1UByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1UByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1UByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqInt1UByte(rarray, null, warray == null, 0, 1) with IndexArray[UByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UByte(rarray, null)

  def apply(i: Int) :Int = rarray(i) & 0xFF
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[buffer] final class BufferInt1UByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1UByte(shared, null, ro, 0, 1) with IndexBuffer[UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UByte(shared, true)

  def apply(i: Int) :Int = buffer.get(i) & 0xFF
  def update(i: Int, v: Int) :Unit = buffer.put(i, v.toByte)
}

private[buffer] final class ViewInt1UByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1UByte(
  shared, new BufferInt1UByte(shared, ro), ro, off, str
) with DataView[Int1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UByte(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v.toByte)
}


// Type: UShort
private[buffer] sealed abstract class SeqInt1UShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[UShort](shared, backing, ro, off, str) {
  final def rawType = RawType.UShort
  final def normalized = false

  final def mkDataArray(array: Array[Char]) =
    new ArrayInt1UShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1UShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1UShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1UShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqInt1UShort(rarray, null, warray == null, 0, 1) with IndexArray[UShort] {
  def this() = this(emptyChar, emptyChar)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UShort(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toChar
}

private[buffer] final class BufferInt1UShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1UShort(shared, null, ro, 0, 1) with IndexBuffer[UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UShort(shared, true)

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v.toChar)
}

private[buffer] final class ViewInt1UShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1UShort(
  shared, new BufferInt1UShort(shared, ro), ro, off, str
) with DataView[Int1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UShort(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(
    offset + i*stride,
    v.toChar
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqInt1UInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[UInt](shared, backing, ro, off, str) {
  final def rawType = RawType.UInt
  final def normalized = false

  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1UInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1UInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1UInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1UInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqInt1UInt(rarray, null, warray == null, 0, 1) with IndexArray[UInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UInt(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1UInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1UInt(shared, null, ro, 0, 1) with IndexBuffer[UInt]{
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UInt(shared, true)

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewInt1UInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1UInt(
  shared, new BufferInt1UInt(shared, ro), ro, off, str
) with DataView[Int1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UInt(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v)
}


// Type: SByte
private[buffer] sealed abstract class SeqInt1SByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[SByte](shared, backing, ro, off, str) {
  final def rawType = RawType.SByte
  final def normalized = false

  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1SByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1SByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1SByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1SByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqInt1SByte(rarray, null, warray == null, 0, 1) with DataArray[Int1, SByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SByte(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toByte
}

private[buffer] final class BufferInt1SByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1SByte(shared, null, ro, 0, 1) with DataBuffer[Int1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SByte(shared, true)

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v.toByte)
}

private[buffer] final class ViewInt1SByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1SByte(
  shared, new BufferInt1SByte(shared, ro), ro, off, str
) with DataView[Int1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SByte(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v.toByte)
}


// Type: SShort
private[buffer] sealed abstract class SeqInt1SShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[SShort](shared, backing, ro, off, str) {
  final def rawType = RawType.SShort
  final def normalized = false

  final def mkDataArray(array: Array[Short]) =
    new ArrayInt1SShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1SShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1SShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1SShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqInt1SShort(rarray, null, warray == null, 0, 1) with DataArray[Int1, SShort] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SShort(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.toShort
}

private[buffer] final class BufferInt1SShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1SShort(shared, null, ro, 0, 1) with DataBuffer[Int1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SShort(shared, true)

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v.toShort)
}

private[buffer] final class ViewInt1SShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1SShort(
  shared, new BufferInt1SShort(shared, ro), ro, off, str
) with DataView[Int1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SShort(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v.toShort)
}


// Type: SInt
private[buffer] sealed abstract class SeqInt1SInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseInt1[SInt](shared, backing, ro, off, str) {
  final def rawType = RawType.SInt
  final def normalized = false

  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1SInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferInt1SInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewInt1SInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayInt1SInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqInt1SInt(rarray, null, warray == null, 0, 1) with DataArray[Int1, SInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SInt(rarray, null)

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1SInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqInt1SInt(shared, null, ro, 0, 1) with DataBuffer[Int1, SInt]{
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SInt(shared, true)

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqInt1SInt(
  shared, new BufferInt1SInt(shared, ro), ro, off, str
) with DataView[Int1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SInt(shared, true, offset, stride)

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v)
}
