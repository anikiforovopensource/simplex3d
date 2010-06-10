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
import simplex3d.math._
import simplex3d.buffer.{allocateByteBuffer => alloc}
import simplex3d.buffer.Util._


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

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayInt1UByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1UByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferInt1UByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqInt1UByte(buff) with IndexArray[UByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i) & 0xFF
  def update(i: Int, v: Int) :Unit = warray(i) = byte(v)
}

private[buffer] final class BufferInt1UByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqInt1UByte(buff) with IndexBuffer[UByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1UByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i) & 0xFF
  def update(i: Int, v: Int) :Unit = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1UByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UByte(buff) with DataView[Int1, UByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferInt1UByte(sharedBuff, buff)
  def asReadOnly() = new ViewInt1UByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, byte(v))
}


// Type: UShort
private[buffer] sealed abstract class SeqInt1UShort(
  buff: CharBuffer
) extends BaseInt1[UShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayInt1UShort(array, array, CharBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Char]) =
    new ArrayInt1UShort(array, array, CharBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferInt1UShort(buff, buff.asCharBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqInt1UShort(
  buff
) with IndexArray[UShort] {
  def this() = this(eaChar, eaChar, ebChar)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = CharBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferInt1UShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer
) extends SeqInt1UShort(buff) with IndexBuffer[UShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1UShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v.asInstanceOf[Char])
}

private[buffer] final class ViewInt1UShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UShort(buff) with DataView[Int1, UShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer(), 0, 1)
  val backingSeq = new BufferInt1UShort(sharedBuff, buff)
  def asReadOnly() = new ViewInt1UShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(
    offset + i*stride,
    v.asInstanceOf[Char]
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqInt1UInt(
  buff: IntBuffer
) extends BaseInt1[UInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayInt1UInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1UInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferInt1UInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqInt1UInt(buff) with IndexArray[UInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1UInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqInt1UInt(buff) with IndexBuffer[UInt]{
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1UInt(sharedBuff, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewInt1UInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UInt(buff) with DataView[Int1, UInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferInt1UInt(sharedBuff, buff)
  def asReadOnly() = new ViewInt1UInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v)
}


// Type: SByte
private[buffer] sealed abstract class SeqInt1SByte(
  buff: ByteBuffer
) extends BaseInt1[SByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayInt1SByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1SByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferInt1SByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqInt1SByte(buff) with DataArray[Int1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = byte(v)
}

private[buffer] final class BufferInt1SByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqInt1SByte(buff) with DataBuffer[Int1, SByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1SByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1SByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SByte(buff) with DataView[Int1, SByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferInt1SByte(sharedBuff, buff)
  def asReadOnly() = new ViewInt1SByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, byte(v))
}


// Type: SShort
private[buffer] sealed abstract class SeqInt1SShort(
  buff: ShortBuffer
) extends BaseInt1[SShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayInt1SShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Short]) =
    new ArrayInt1SShort(array, array, ShortBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferInt1SShort(buff, buff.asShortBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SShort(byteBuffer, byteBuffer.asShortBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqInt1SShort(
  buff
) with DataArray[Int1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = short(v)
}

private[buffer] final class BufferInt1SShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer
) extends SeqInt1SShort(buff) with DataBuffer[Int1, SShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1SShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, short(v))
}

private[buffer] final class ViewInt1SShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SShort(buff) with DataView[Int1, SShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer(), 0, 1)
  val backingSeq = new BufferInt1SShort(sharedBuff, buff)
  def asReadOnly() = new ViewInt1SShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, short(v))
}


// Type: SInt
private[buffer] sealed abstract class SeqInt1SInt(
  buff: IntBuffer
) extends BaseInt1[SInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayInt1SInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1SInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferInt1SInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqInt1SInt(buff) with DataArray[Int1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayInt1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1SInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqInt1SInt(buff) with DataBuffer[Int1, SInt]{
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferInt1SInt(sharedBuff, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SInt(buff) with DataView[Int1, SInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferInt1SInt(sharedBuff, buff)
  def asReadOnly() = new ViewInt1SInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buffer.put(offset + i*stride, v)
}
