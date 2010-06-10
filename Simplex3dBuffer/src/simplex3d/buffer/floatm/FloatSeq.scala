/*
 * Simplex3d, FloatBuffer module
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

package simplex3d.buffer.floatm

import java.nio._
import simplex3d.math._
import simplex3d.math.floatm.FloatMath._
import simplex3d.buffer.{allocateByteBuffer => alloc, _}
import simplex3d.buffer.Util._
import simplex3d.buffer.HalfFloatUtil.{
  floatToHalfFloat => toHalfFloat, floatFromHalfFloat => fromHalfFloat
}


/**
 * @author Aleksey Nikiforov (lex)
 */
private[floatm] object Shared {
  // Use double multiplication for float sequences to prevent errors
  // Use division for double sequences to prevent errors
  final val fromNSByte = 0.00787401574803149606
  final val fromNUByte = 0.00392156862745098039
  final val fromNSShort = 3.05185094759971922971e-5
  final val fromNUShort = 1.52590218966964217594e-5
  final val fromNSInt = 4.65661287524579692411e-10
  final val fromNUInt = 2.32830643708079737543e-10

  final val toNSByte = 127f
  final val toNUByte = 255f
  final val toNSShort = 32767f
  final val toNUShort = 65535f
  final val toNSInt = 2147483647f
  final val toNUInt = 4294967295f

  final def iround(x: Float) :Int = {
    if (x >= 0) int(x + 0.5f)
    else int(x - 0.5f)
  }
}
import Shared._

private[buffer] sealed abstract class BaseFloat1[+D <: ReadableFloat](
  buff: D#BufferType
) extends BaseSeq[Float1, Float, D](buff) {
  final def elementManifest = componentManifest
  final def componentManifest = scala.reflect.ClassManifest.Float
  final def components: Int = 1
}


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  buff: ByteBuffer
) extends BaseFloat1[SByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1SByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1SByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1SByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1SByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1SByte(buff) with DataArray[Float1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = byte(iround(v))
}

private[buffer] final class BufferFloat1SByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1SByte(buff) with DataBuffer[Float1, SByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1SByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(i, byte(iround(v)))
}

private[buffer] final class ViewFloat1SByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SByte(buff) with DataView[Float1, SByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferFloat1SByte(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1SByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    byte(iround(v))
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  buff: ByteBuffer
) extends BaseFloat1[UByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1UByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1UByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1UByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1UByte(buff) with DataArray[Float1, UByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i) & 0xFF
  def update(i: Int, v: Float) :Unit = warray(i) = byte(iround(v))
}

private[buffer] final class BufferFloat1UByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1UByte(buff) with DataBuffer[Float1, UByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1UByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i) & 0xFF
  def update(i: Int, v: Float) :Unit = buffer.put(i, byte(iround(v)))
}

private[buffer] final class ViewFloat1UByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UByte(buff) with DataView[Float1, UByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferFloat1UByte(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1UByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    byte(iround(v))
  )
}


// Type: NSByte
private[buffer] sealed abstract class SeqFloat1NSByte(
  buff: ByteBuffer
) extends BaseFloat1[NSByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1NSByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1NSByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1NSByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NSByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NSByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1NSByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1NSByte(buff) with DataArray[Float1, NSByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NSByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -127) -1 else float(v*fromNSByte)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = byte(iround(clamp(v, -1, 1)*toNSByte))
}

private[buffer] final class BufferFloat1NSByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1NSByte(buff) with DataBuffer[Float1, NSByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NSByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -127) -1 else float(v*fromNSByte)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    byte(iround(clamp(v, -1, 1)*toNSByte))
  )
}

private[buffer] final class ViewFloat1NSByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSByte(buff) with DataView[Float1, NSByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferFloat1NSByte(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NSByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*stride)
    if (v < -127) -1 else float(v*fromNSByte)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    byte(iround(clamp(v, -1, 1)*toNSByte))
  )
}


// Type: NUByte
private[buffer] sealed abstract class SeqFloat1NUByte(
  buff: ByteBuffer
) extends BaseFloat1[NUByte](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1NUByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1NUByte(array, array, ByteBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1NUByte(buff, buff.duplicate())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NUByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NUByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1NUByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1NUByte(buff) with DataArray[Float1, NUByte] {
  def this() = this(eaByte, eaByte, ebByte)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NUByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((rarray(i) & 0xFF)*fromNUByte)
  def update(i: Int, v: Float) :Unit =
    warray(i) = byte(iround(clamp(v, 0, 1)*toNUByte))
}

private[buffer] final class BufferFloat1NUByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1NUByte(buff) with DataBuffer[Float1, NUByte] {
  def this() = this(alloc(0), alloc(0).duplicate())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NUByte(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((buffer.get(i) & 0xFF)*fromNUByte)
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    byte(iround(clamp(v, 0, 1)*toNUByte))
  )
}

private[buffer] final class ViewFloat1NUByte(
  sharedBuff: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUByte(buff) with DataView[Float1, NUByte] {
  def this() = this(alloc(0), alloc(0).duplicate(), 0, 1)
  val backingSeq = new BufferFloat1NUByte(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NUByte(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (buffer.get(offset + i*stride) & 0xFF)*fromNUByte
  )
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    byte(iround(clamp(v, 0, 1)*toNUByte))
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  buff: ShortBuffer
) extends BaseFloat1[SShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayFloat1SShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Short]) =
    new ArrayFloat1SShort(array, array, ShortBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1SShort(buff, buff.asShortBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SShort(byteBuffer, byteBuffer.asShortBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1SShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqFloat1SShort(buff) with DataArray[Float1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = short(iround(v))
}

private[buffer] final class BufferFloat1SShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1SShort(buff) with DataBuffer[Float1, SShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1SShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(i, short(iround(v)))
}

private[buffer] final class ViewFloat1SShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SShort(buff) with DataView[Float1, SShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer(), 0, 1)
  val backingSeq = new BufferFloat1SShort(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1SShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    short(iround(v))
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  buff: CharBuffer
) extends BaseFloat1[UShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayFloat1UShort(array, array, CharBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Char]) =
    new ArrayFloat1UShort(array, array, CharBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1UShort(buff, buff.asCharBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqFloat1UShort(buff) with DataArray[Float1, UShort] {
  def this() = this(eaChar, eaChar, ebChar)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = CharBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = iround(v).asInstanceOf[Char]
}

private[buffer] final class BufferFloat1UShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1UShort(buff) with DataBuffer[Float1, UShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1UShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    iround(v).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1UShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UShort(buff) with DataView[Float1, UShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer(), 0, 1)
  val backingSeq = new BufferFloat1UShort(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1UShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    iround(v).asInstanceOf[Char]
  )
}


// Type: NSShort
private[buffer] sealed abstract class SeqFloat1NSShort(
  buff: ShortBuffer
) extends BaseFloat1[NSShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayFloat1NSShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Short]) =
    new ArrayFloat1NSShort(array, array, ShortBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1NSShort(buff, buff.asShortBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NSShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NSShort(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1NSShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqFloat1NSShort(buff) with DataArray[Float1, NSShort] {
  def this() = this(eaShort, eaShort, ebShort)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NSShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -32767) -1 else float(v*fromNSShort)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = short(iround(clamp(v, -1, 1)*toNSShort))
}

private[buffer] final class BufferFloat1NSShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1NSShort(buff) with DataBuffer[Float1, NSShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NSShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -32767) -1 else float(v*fromNSShort)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    short(iround(clamp(v, -1, 1)*toNSShort))
  )
}

private[buffer] final class ViewFloat1NSShort(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSShort(buff) with DataView[Float1, NSShort] {
  def this() = this(alloc(0), alloc(0).asShortBuffer(), 0, 1)
  val backingSeq = new BufferFloat1NSShort(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NSShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*stride)
    if (v < -32767) -1 else float(v*fromNSShort)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    short(iround(clamp(v, -1, 1)*toNSShort))
  )
}


// Type: NUShort
private[buffer] sealed abstract class SeqFloat1NUShort(
  buff: CharBuffer
) extends BaseFloat1[NUShort](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayFloat1NUShort(array, array, CharBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Char]) =
    new ArrayFloat1NUShort(array, array, CharBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1NUShort(buff, buff.asCharBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NUShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NUShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1NUShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqFloat1NUShort(buff) with DataArray[Float1, NUShort] {
  def this() = this(eaChar, eaChar, ebChar)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = CharBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NUShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(rarray(i)*fromNUShort)
  def update(i: Int, v: Float) =
    warray(i) = iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
}

private[buffer] final class BufferFloat1NUShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1NUShort(buff) with DataBuffer[Float1, NUShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NUShort(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buffer.get(i)*fromNUShort)
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1NUShort(
  sharedBuff: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUShort(buff) with DataView[Float1, NUShort] {
  def this() = this(alloc(0), alloc(0).asCharBuffer(), 0, 1)
  val backingSeq = new BufferFloat1NUShort(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NUShort(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buffer.get(offset + i*stride)*fromNUShort)
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  buff: IntBuffer
) extends BaseFloat1[SInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1SInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1SInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1SInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1SInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1SInt(buff) with DataArray[Float1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = iround(v)
}

private[buffer] final class BufferFloat1SInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1SInt(buff) with DataBuffer[Float1, SInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1SInt(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(i, iround(v))
}

private[buffer] final class ViewFloat1SInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SInt(buff) with DataView[Float1, SInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferFloat1SInt(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1SInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(offset + i*stride, iround(v))
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  buff: IntBuffer
) extends BaseFloat1[UInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1UInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1UInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1UInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1UInt(buff) with DataArray[Float1, UInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(rarray(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit =
    warray(i) = int(long(iround(v)) & 0xFFFFFFFFL)
}

private[buffer] final class BufferFloat1UInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1UInt(buff) with DataBuffer[Float1, UInt]{
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1UInt(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buffer.get(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    int(long(iround(v)) & 0xFFFFFFFFL)
  )
}

private[buffer] final class ViewFloat1UInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UInt(buff) with DataView[Float1, UInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferFloat1UInt(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1UInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buffer.get(offset + i*stride)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    int(long(iround(v)) & 0xFFFFFFFFL)
  )
}


// Type: NSInt
private[buffer] sealed abstract class SeqFloat1NSInt(
  buff: IntBuffer
) extends BaseFloat1[NSInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1NSInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1NSInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1NSInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NSInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NSInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1NSInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1NSInt(buff) with DataArray[Float1, NSInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NSInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -2147483647) -1 else float(v*fromNSInt)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = iround(clamp(v, -1, 1)*toNSInt)
}

private[buffer] final class BufferFloat1NSInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1NSInt(buff) with DataBuffer[Float1, NSInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NSInt(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -2147483647) -1 else float(v*fromNSInt)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    iround(clamp(v, -1, 1)*toNSInt)
  )
}

private[buffer] final class ViewFloat1NSInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSInt(buff) with DataView[Float1, NSInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferFloat1NSInt(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NSInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*stride)
    if (v < -2147483647) -1 else float(v*fromNSInt)
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    iround(clamp(v, -1, 1)*toNSInt)
  )
}


// Type: NUInt
private[buffer] sealed abstract class SeqFloat1NUInt(
  buff: IntBuffer
) extends BaseFloat1[NUInt](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1NUInt(array, array, IntBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1NUInt(array, array, IntBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1NUInt(buff, buff.asIntBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1NUInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1NUInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1NUInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1NUInt(buff) with DataArray[Float1, NUInt] {
  def this() = this(eaInt, eaInt, ebInt)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1NUInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((long(rarray(i)) & 0xFFFFFFFFL)*fromNUInt)
  def update(i: Int, v: Float) :Unit = warray(i) = iround(clamp(v, 0, 1)*toNUInt)
}

private[buffer] final class BufferFloat1NUInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1NUInt(buff) with DataBuffer[Float1, NUInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1NUInt(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buffer.get(i)) & 0xFFFFFFFFL)*fromNUInt
  )
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    iround(clamp(v, 0, 1)*toNUInt)
  )
}

private[buffer] final class ViewFloat1NUInt(
  sharedBuff: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUInt(buff) with DataView[Float1, NUInt] {
  def this() = this(alloc(0), alloc(0).asIntBuffer(), 0, 1)
  val backingSeq = new BufferFloat1NUInt(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1NUInt(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buffer.get(offset + i*stride)) & 0xFFFFFFFFL)*fromNUInt
  )
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toNUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqFloat1HalfFloat(
  buff: ShortBuffer
) extends BaseFloat1[HalfFloat](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayFloat1HalfFloat(array, array, ShortBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Short]) =
    new ArrayFloat1HalfFloat(array, array, ShortBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1HalfFloat(buff, buff.asShortBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1HalfFloat(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1HalfFloat(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1HalfFloat(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqFloat1HalfFloat(buff) with DataArray[Float1, HalfFloat] {
  def this() = this(eaShort, eaShort, ebShort)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1HalfFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(rarray(i))
  def update(i: Int, v: Float) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferFloat1HalfFloat(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1HalfFloat(buff) with DataBuffer[Float1, HalfFloat] {
  def this() = this(alloc(0), alloc(0).asShortBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1HalfFloat(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buffer.get(i))
  def update(i: Int, v: Float) :Unit = buffer.put(i, toHalfFloat(v))
}

private[buffer] final class ViewFloat1HalfFloat(
  sharedBuff: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1HalfFloat(buff) with DataView[Float1, HalfFloat] {
  def this() = this(alloc(0), alloc(0).asShortBuffer(), 0, 1)
  val backingSeq = new BufferFloat1HalfFloat(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1HalfFloat(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buffer.get(offset + i*stride))
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqFloat1RawFloat(
  var buff: FloatBuffer
) extends BaseFloat1[RawFloat](buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()
  
  final def mkDataArray(size: Int) = {
    val array = new Array[Float](size)
    new ArrayFloat1RawFloat(array, array, FloatBuffer.wrap(array))
  }
  final def mkDataArray(array: Array[Float]) =
    new ArrayFloat1RawFloat(array, array, FloatBuffer.wrap(array))
  final def mkDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1RawFloat(buff, buff.asFloatBuffer())
  }
  final def mkDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1RawFloat(byteBuffer, byteBuffer.asFloatBuffer())
  }
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1RawFloat(
      byteBuffer, byteBuffer.asFloatBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1RawFloat(
  rarray: Array[Float], warray: Array[Float], buff: FloatBuffer
) extends SeqFloat1RawFloat(buff) with DataArray[Float1, RawFloat] {
  def this() = this(eaFloat, eaFloat, ebFloat)
  setReadArray(rarray)
  private[buffer] override val bindingBuffer = FloatBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnly() = new ArrayFloat1RawFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  sharedBuff: ByteBuffer,
  buff: FloatBuffer
) extends SeqFloat1RawFloat(buff) with DataBuffer[Float1, RawFloat] {
  def this() = this(alloc(0), alloc(0).asFloatBuffer())
  setSharedByteBuffer(sharedBuff)
  def backingSeq = this
  def asReadOnly() = new BufferFloat1RawFloat(
    sharedBuff, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  sharedBuff: ByteBuffer,
  buff: FloatBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1RawFloat(buff) with DataView[Float1, RawFloat] {
  def this() = this(alloc(0), alloc(0).asFloatBuffer(), 0, 1)
  val backingSeq = new BufferFloat1RawFloat(sharedBuff, buff)
  def asReadOnly() = new ViewFloat1RawFloat(
    sharedBuff, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(offset + i*stride, v)
}
