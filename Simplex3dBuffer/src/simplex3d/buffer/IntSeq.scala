/*
 * Simplex3d, BaseBuffer module
 * Copyright (C) 2010 Simplex3d Team
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
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseInt1[+D <: ReadableInt](
  buff: D#BufferType
) extends BaseSeq[Int1, Int, D](buff) {
  final def elementManifest = componentManifest
  final def componentManifest = scala.reflect.ClassManifest.Int
  final def components: Int = 1
}


// Type: UByte
private[buffer] sealed abstract class SeqInt1UByte(
  buff: ByteBuffer
) extends BaseInt1[UByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1UByte(new Array[Byte](size))
  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1UByte(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1UByte(allocateByteBuffer(size))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1UByte(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1UByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1UByte(
  override val array: Array[Byte]
) extends SeqInt1UByte(ByteBuffer.wrap(array)) with IndexArray[UByte] {
  def this() = this(new Array[Byte](0))
  def backingSeq = this

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i) & 0xFF
  def update(i: Int, v: Int) = array(i) = byte(v)
}

private[buffer] final class BufferInt1UByte(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1UByte(sharedBuffer.duplicate()) with IndexBuffer[UByte] {
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i) & 0xFF
  def update(i: Int, v: Int) = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1UByte(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1UByte(sharedBuffer.duplicate()) with DataView[Int1, UByte] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1UByte(sharedBuffer)

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Int) = buffer.put(offset + i*stride, byte(v))
}


// Type: UShort
private[buffer] sealed abstract class SeqInt1UShort(
  buff: CharBuffer
) extends BaseInt1[UShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1UShort(new Array[Char](size))
  final def mkDataArray(array: Array[Char]) =
    new ArrayInt1UShort(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1UShort(allocateByteBuffer(size*2))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1UShort(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1UShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1UShort(
  override val array: Array[Char]
) extends SeqInt1UShort(
  CharBuffer.wrap(array)
) with IndexArray[UShort] {
  def this() = this(new Array[Char](0))
  def backingSeq = this

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferInt1UShort(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1UShort(
  sharedBuffer.asCharBuffer()
) with IndexBuffer[UShort] {
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v.asInstanceOf[Char])
}

private[buffer] final class ViewInt1UShort(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1UShort(
  sharedBuffer.asCharBuffer()
) with DataView[Int1, UShort] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1UShort(sharedBuffer)

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) = {
    buffer.put(offset + i*stride, v.asInstanceOf[Char])
  }
}


// Type: UInt
private[buffer] sealed abstract class SeqInt1UInt(
  buff: IntBuffer
) extends BaseInt1[UInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1UInt(new Array[Int](size))
  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1UInt(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1UInt(allocateByteBuffer(size*4))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1UInt(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1UInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1UInt(
  override val array: Array[Int]
) extends SeqInt1UInt(IntBuffer.wrap(array)) with IndexArray[UInt] {
  def this() = this(new Array[Int](0))
  def backingSeq = this

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v
}

private[buffer] final class BufferInt1UInt(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1UInt(sharedBuffer.asIntBuffer()) with IndexBuffer[UInt]{
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v)
}

private[buffer] final class ViewInt1UInt(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1UInt(sharedBuffer.asIntBuffer()) with DataView[Int1, UInt] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1UInt(sharedBuffer)

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) = buffer.put(offset + i*stride, v)
}


// Type: SByte
private[buffer] sealed abstract class SeqInt1SByte(
  buff: ByteBuffer
) extends BaseInt1[SByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1SByte(new Array[Byte](size))
  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1SByte(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SByte(allocateByteBuffer(size))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SByte(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SByte(
  override val array: Array[Byte]
) extends SeqInt1SByte(ByteBuffer.wrap(array)) with DataArray[Int1, SByte] {
  def this() = this(new Array[Byte](0))
  def backingSeq = this

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = byte(v)
}

private[buffer] final class BufferInt1SByte(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1SByte(sharedBuffer.duplicate()) with DataBuffer[Int1, SByte] {
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1SByte(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1SByte(sharedBuffer.duplicate()) with DataView[Int1, SByte] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1SByte(sharedBuffer)

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) = buffer.put(offset + i*stride, byte(v))
}


// Type: SShort
private[buffer] sealed abstract class SeqInt1SShort(
  buff: ShortBuffer
) extends BaseInt1[SShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1SShort(new Array[Short](size))
  final def mkDataArray(array: Array[Short]) =
    new ArrayInt1SShort(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SShort(allocateByteBuffer(size*2))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SShort(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SShort(
  override val array: Array[Short]
) extends SeqInt1SShort(
  ShortBuffer.wrap(array)
) with DataArray[Int1, SShort] {
  def this() = this(new Array[Short](0))
  def backingSeq = this

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = short(v)
}

private[buffer] final class BufferInt1SShort(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1SShort(
  sharedBuffer.asShortBuffer()
) with DataBuffer[Int1, SShort] {
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, short(v))
}

private[buffer] final class ViewInt1SShort(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1SShort(
  sharedBuffer.asShortBuffer()
) with DataView[Int1, SShort] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1SShort(sharedBuffer)

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) = buffer.put(offset + i*stride, short(v))
}


// Type: SInt
private[buffer] sealed abstract class SeqInt1SInt(
  buff: IntBuffer
) extends BaseInt1[SInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) =
    new ArrayInt1SInt(new Array[Int](size))
  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1SInt(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SInt(allocateByteBuffer(size*4))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SInt(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SInt(
  override val array: Array[Int]
) extends SeqInt1SInt(IntBuffer.wrap(array)) with DataArray[Int1, SInt] {
  def this() = this(new Array[Int](0))
  def backingSeq = this

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v
}

private[buffer] final class BufferInt1SInt(
  private[buffer] override val sharedBuffer: ByteBuffer
) extends SeqInt1SInt(sharedBuffer.asIntBuffer()) with DataBuffer[Int1, SInt]{
  def this() = this(allocateByteBuffer(0))
  def backingSeq = this

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  private[buffer] override val sharedBuffer: ByteBuffer,
  val offset: Int,
  override val stride: Int
) extends SeqInt1SInt(sharedBuffer.asIntBuffer()) with DataView[Int1, SInt] {
  def this() = this(allocateByteBuffer(0), 0, 1)
  val backingSeq = new BufferInt1SInt(sharedBuffer)

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) = buffer.put(offset + i*stride, v)
}
