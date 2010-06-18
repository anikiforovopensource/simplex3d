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

  final def uround(x: Float) :Int = {
    if (x >= 0) int(long(x + 0.5f))
    else int(long(x - 0.5f))
  }
}
import Shared._

private[buffer] sealed abstract class BaseFloat1[+R <: ReadableFloat](
  shared: AnyRef, buff: R#BufferType
) extends BaseSeq[Float1, Float, R](shared, buff) {
  final def elementManifest = componentManifest
  final def componentManifest = scala.reflect.ClassManifest.Float
  final def components: Int = 1
}


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseFloat1[SByte](shared, buff) {
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
) extends SeqFloat1SByte(rarray, buff) with DataArray[Float1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = byte(iround(v))
}

private[buffer] final class BufferFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1SByte(shared, buff) with DataBuffer[Float1, SByte] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1SByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, byte(iround(v)))
}

private[buffer] final class ViewFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SByte(shared, buff) with DataView[Float1, SByte] {
  val backingSeq = new BufferFloat1SByte(shared, buff)
  def asReadOnlySeq() = new ViewFloat1SByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(v))
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseFloat1[UByte](shared, buff) {
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
) extends SeqFloat1UByte(rarray, buff) with DataArray[Float1, UByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i) & 0xFF
  def update(i: Int, v: Float) :Unit = warray(i) = byte(iround(v))
}

private[buffer] final class BufferFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1UByte(shared, buff) with DataBuffer[Float1, UByte] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1UByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(i) & 0xFF
  def update(i: Int, v: Float) :Unit = buff.put(i, byte(iround(v)))
}

private[buffer] final class ViewFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UByte(shared, buff) with DataView[Float1, UByte] {
  val backingSeq = new BufferFloat1UByte(shared, buff)
  def asReadOnlySeq() = new ViewFloat1UByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(v))
  )
}


// Type: NSByte
private[buffer] sealed abstract class SeqFloat1NSByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseFloat1[NSByte](shared, buff) {
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
) extends SeqFloat1NSByte(rarray, buff) with DataArray[Float1, NSByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NSByte(rarray, null, buffer.asReadOnlyBuffer())

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
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1NSByte(shared, buff) with DataBuffer[Float1, NSByte] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NSByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -127) -1 else float(v*fromNSByte)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    byte(iround(clamp(v, -1, 1)*toNSByte))
  )
}

private[buffer] final class ViewFloat1NSByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSByte(shared, buff) with DataView[Float1, NSByte] {
  val backingSeq = new BufferFloat1NSByte(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NSByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -127) -1 else float(v*fromNSByte)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, -1, 1)*toNSByte))
  )
}


// Type: NUByte
private[buffer] sealed abstract class SeqFloat1NUByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseFloat1[NUByte](shared, buff) {
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
) extends SeqFloat1NUByte(rarray, buff) with DataArray[Float1, NUByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override val bindingBuffer = ByteBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NUByte(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((rarray(i) & 0xFF)*fromNUByte)
  def update(i: Int, v: Float) :Unit =
    warray(i) = byte(iround(clamp(v, 0, 1)*toNUByte))
}

private[buffer] final class BufferFloat1NUByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1NUByte(shared, buff) with DataBuffer[Float1, NUByte] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NUByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((buff.get(i) & 0xFF)*fromNUByte)
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    byte(iround(clamp(v, 0, 1)*toNUByte))
  )
}

private[buffer] final class ViewFloat1NUByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUByte(shared, buff) with DataView[Float1, NUByte] {
  val backingSeq = new BufferFloat1NUByte(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NUByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (buff.get(offset + i*stride) & 0xFF)*fromNUByte
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, 0, 1)*toNUByte))
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  shared: AnyRef, buff: ShortBuffer
) extends BaseFloat1[SShort](shared, buff) {
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
) extends SeqFloat1SShort(rarray, buff) with DataArray[Float1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = short(iround(v))
}

private[buffer] final class BufferFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1SShort(shared, buff) with DataBuffer[Float1, SShort] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1SShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, short(iround(v)))
}

private[buffer] final class ViewFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SShort(shared, buff) with DataView[Float1, SShort] {
  val backingSeq = new BufferFloat1SShort(shared, buff)
  def asReadOnlySeq() = new ViewFloat1SShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    short(iround(v))
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  shared: AnyRef, buff: CharBuffer
) extends BaseFloat1[UShort](shared, buff) {
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
) extends SeqFloat1UShort(rarray, buff) with DataArray[Float1, UShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override val bindingBuffer = CharBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = iround(v).asInstanceOf[Char]
}

private[buffer] final class BufferFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1UShort(shared, buff) with DataBuffer[Float1, UShort] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1UShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(v).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UShort(shared, buff) with DataView[Float1, UShort] {
  val backingSeq = new BufferFloat1UShort(shared, buff)
  def asReadOnlySeq() = new ViewFloat1UShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(v).asInstanceOf[Char]
  )
}


// Type: NSShort
private[buffer] sealed abstract class SeqFloat1NSShort(
  shared: AnyRef, buff: ShortBuffer
) extends BaseFloat1[NSShort](shared, buff) {
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
) extends SeqFloat1NSShort(rarray, buff) with DataArray[Float1, NSShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NSShort(rarray, null, buffer.asReadOnlyBuffer())

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
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1NSShort(shared, buff) with DataBuffer[Float1, NSShort] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NSShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -32767) -1 else float(v*fromNSShort)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    short(iround(clamp(v, -1, 1)*toNSShort))
  )
}

private[buffer] final class ViewFloat1NSShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSShort(shared, buff) with DataView[Float1, NSShort] {
  val backingSeq = new BufferFloat1NSShort(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NSShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -32767) -1 else float(v*fromNSShort)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    short(iround(clamp(v, -1, 1)*toNSShort))
  )
}


// Type: NUShort
private[buffer] sealed abstract class SeqFloat1NUShort(
  shared: AnyRef, buff: CharBuffer
) extends BaseFloat1[NUShort](shared, buff) {
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
) extends SeqFloat1NUShort(rarray, buff) with DataArray[Float1, NUShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override val bindingBuffer = CharBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NUShort(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(rarray(i)*fromNUShort)
  def update(i: Int, v: Float) =
    warray(i) = iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
}

private[buffer] final class BufferFloat1NUShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1NUShort(shared, buff) with DataBuffer[Float1, NUShort] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NUShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buff.get(i)*fromNUShort)
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1NUShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUShort(shared, buff) with DataView[Float1, NUShort] {
  val backingSeq = new BufferFloat1NUShort(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NUShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buff.get(offset + i*stride)*fromNUShort)
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toNUShort).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseFloat1[SInt](shared, buff) {
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
) extends SeqFloat1SInt(rarray, buff) with DataArray[Float1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = iround(v)
}

private[buffer] final class BufferFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1SInt(shared, buff) with DataBuffer[Float1, SInt] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1SInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, iround(v))
}

private[buffer] final class ViewFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1SInt(shared, buff) with DataView[Float1, SInt] {
  val backingSeq = new BufferFloat1SInt(shared, buff)
  def asReadOnlySeq() = new ViewFloat1SInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(offset + i*stride, iround(v))
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseFloat1[UInt](shared, buff) {
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
) extends SeqFloat1UInt(rarray, buff) with DataArray[Float1, UInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(rarray(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit =
    warray(i) = uround(v)
}

private[buffer] final class BufferFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1UInt(shared, buff) with DataBuffer[Float1, UInt]{
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1UInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buff.get(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    uround(v)
  )
}

private[buffer] final class ViewFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1UInt(shared, buff) with DataView[Float1, UInt] {
  val backingSeq = new BufferFloat1UInt(shared, buff)
  def asReadOnlySeq() = new ViewFloat1UInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buff.get(offset + i*stride)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    uround(v)
  )
}


// Type: NSInt
private[buffer] sealed abstract class SeqFloat1NSInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseFloat1[NSInt](shared, buff) {
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
) extends SeqFloat1NSInt(rarray, buff) with DataArray[Float1, NSInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NSInt(rarray, null, buffer.asReadOnlyBuffer())

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
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1NSInt(shared, buff) with DataBuffer[Float1, NSInt] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NSInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -2147483647) -1 else float(v*fromNSInt)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(clamp(v, -1, 1)*toNSInt)
  )
}

private[buffer] final class ViewFloat1NSInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NSInt(shared, buff) with DataView[Float1, NSInt] {
  val backingSeq = new BufferFloat1NSInt(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NSInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -2147483647) -1 else float(v*fromNSInt)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, -1, 1)*toNSInt)
  )
}


// Type: NUInt
private[buffer] sealed abstract class SeqFloat1NUInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseFloat1[NUInt](shared, buff) {
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
) extends SeqFloat1NUInt(rarray, buff) with DataArray[Float1, NUInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override val bindingBuffer = IntBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1NUInt(rarray, null, buffer.asReadOnlyBuffer())

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((long(rarray(i)) & 0xFFFFFFFFL)*fromNUInt)
  def update(i: Int, v: Float) :Unit = warray(i) = uround(clamp(v, 0, 1)*toNUInt)
}

private[buffer] final class BufferFloat1NUInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1NUInt(shared, buff) with DataBuffer[Float1, NUInt] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1NUInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buff.get(i)) & 0xFFFFFFFFL)*fromNUInt
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    uround(clamp(v, 0, 1)*toNUInt)
  )
}

private[buffer] final class ViewFloat1NUInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1NUInt(shared, buff) with DataView[Float1, NUInt] {
  val backingSeq = new BufferFloat1NUInt(shared, buff)
  def asReadOnlySeq() = new ViewFloat1NUInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def bindingType = RawType.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buff.get(offset + i*stride)) & 0xFFFFFFFFL)*fromNUInt
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    uround(clamp(v, 0, 1)*toNUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqFloat1HalfFloat(
  shared: AnyRef, buff: ShortBuffer
) extends BaseFloat1[HalfFloat](shared, buff) {
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
) extends SeqFloat1HalfFloat(rarray, buff) with DataArray[Float1, HalfFloat] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override val bindingBuffer = ShortBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1HalfFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(rarray(i))
  def update(i: Int, v: Float) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1HalfFloat(shared, buff) with DataBuffer[Float1, HalfFloat] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buff.get(i))
  def update(i: Int, v: Float) :Unit = buff.put(i, toHalfFloat(v))
}

private[buffer] final class ViewFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1HalfFloat(shared, buff) with DataView[Float1, HalfFloat] {
  val backingSeq = new BufferFloat1HalfFloat(shared, buff)
  def asReadOnlySeq() = new ViewFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buff.get(offset + i*stride))
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqFloat1RawFloat(
  shared: AnyRef, buff: FloatBuffer
) extends BaseFloat1[RawFloat](shared, buff) {
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
) extends SeqFloat1RawFloat(rarray, buff) with DataArray[Float1, RawFloat] {
  def this() = this(eaFloat, eaFloat, ebFloat)

  private[buffer] override val bindingBuffer = FloatBuffer.wrap(rarray)
  def backingSeq = this
  def asReadOnlySeq() = new ArrayFloat1RawFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer
) extends SeqFloat1RawFloat(shared, buff) with DataBuffer[Float1, RawFloat] {
  def backingSeq = this
  def asReadOnlySeq() = new BufferFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqFloat1RawFloat(shared, buff) with DataView[Float1, RawFloat] {
  val backingSeq = new BufferFloat1RawFloat(shared, buff)
  def asReadOnlySeq() = new ViewFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def bindingType: Int = RawType.RawFloat

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(offset + i*stride, v)
}
