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
import scala.reflect.Manifest
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

  final val fromSByte = 0.00787401574803149606
  final val fromUByte = 0.00392156862745098039
  final val fromSShort = 3.05185094759971922971e-5
  final val fromUShort = 1.52590218966964217594e-5
  final val fromSInt = 4.65661287524579692411e-10
  final val fromUInt = 2.32830643708079737543e-10

  final val toSByte = 127f
  final val toUByte = 255f
  final val toSShort = 32767f
  final val toUShort = 65535f
  final val toSInt = 2147483647f
  final val toUInt = 4294967295f

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
  shared: AnyRef, buff: R#BufferType, offset: Int, stride: Int
) extends BaseSeq[Float1, Float, R](shared, buff, offset, stride) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Float
  final def components: Int = 1

  private[buffer] def mkBindingBuffer() = asReadOnlyBuffer()
}


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  shared: AnyRef, buff: ByteBuffer, offset: Int, stride: Int
) extends BaseFloat1[SByte](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1SByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayFloat1SByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1SByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1SByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1SByte(rarray, buff, 0, 1) with DataArray[Float1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = byte(iround(clamp(v, -1, 1)*toSByte))
}

private[buffer] final class BufferFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1SByte(shared, buff, 0, 1) with DataBuffer[Float1, SByte] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    byte(iround(clamp(v, -1, 1)*toSByte))
  )
}

private[buffer] final class ViewFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SByte(shared, buff, offset, stride) with DataView[Float1, SByte] {
  val backingSeq = new BufferFloat1SByte(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, -1, 1)*toSByte))
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  shared: AnyRef, buff: ByteBuffer, offset: Int, stride: Int
) extends BaseFloat1[UByte](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayFloat1UByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayFloat1UByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferFloat1UByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqFloat1UByte(rarray, buff, 0, 1) with DataArray[Float1, UByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((rarray(i) & 0xFF)*fromUByte)
  def update(i: Int, v: Float) :Unit =
    warray(i) = byte(iround(clamp(v, 0, 1)*toUByte))
}

private[buffer] final class BufferFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1UByte(shared, buff, 0, 1) with DataBuffer[Float1, UByte] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((buff.get(i) & 0xFF)*fromUByte)
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    byte(iround(clamp(v, 0, 1)*toUByte))
  )
}

private[buffer] final class ViewFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UByte(shared, buff, offset, stride) with DataView[Float1, UByte] {
  val backingSeq = new BufferFloat1UByte(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (buff.get(offset + i*stride) & 0xFF)*fromUByte
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, 0, 1)*toUByte))
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  shared: AnyRef, buff: ShortBuffer, offset: Int, stride: Int
) extends BaseFloat1[SShort](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayFloat1SShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Short]) =
    new ArrayFloat1SShort(array, array, ShortBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1SShort(buff, buff.asShortBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SShort(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1SShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqFloat1SShort(rarray, buff, 0, 1) with DataArray[Float1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = short(iround(clamp(v, -1, 1)*toSShort))
}

private[buffer] final class BufferFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1SShort(shared, buff, 0, 1) with DataBuffer[Float1, SShort] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    short(iround(clamp(v, -1, 1)*toSShort))
  )
}

private[buffer] final class ViewFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SShort(shared, buff, offset, stride) with DataView[Float1, SShort] {
  val backingSeq = new BufferFloat1SShort(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    short(iround(clamp(v, -1, 1)*toSShort))
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  shared: AnyRef, buff: CharBuffer, offset: Int, stride: Int
) extends BaseFloat1[UShort](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayFloat1UShort(array, array, CharBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Char]) =
    new ArrayFloat1UShort(array, array, CharBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1UShort(buff, buff.asCharBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqFloat1UShort(rarray, buff, 0, 1) with DataArray[Float1, UShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override def mkBindingBuffer() = CharBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(rarray(i)*fromUShort)
  def update(i: Int, v: Float) =
    warray(i) = iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
}

private[buffer] final class BufferFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1UShort(shared, buff, 0, 1) with DataBuffer[Float1, UShort] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buff.get(i)*fromUShort)
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UShort(shared, buff, offset, stride) with DataView[Float1, UShort] {
  val backingSeq = new BufferFloat1UShort(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(buff.get(offset + i*stride)*fromUShort)
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  shared: AnyRef, buff: IntBuffer, offset: Int, stride: Int
) extends BaseFloat1[SInt](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1SInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayFloat1SInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1SInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1SInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1SInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1SInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1SInt(rarray, buff, 0, 1) with DataArray[Float1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = iround(clamp(v, -1, 1)*toSInt)
}

private[buffer] final class BufferFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1SInt(shared, buff, 0, 1) with DataBuffer[Float1, SInt] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(clamp(v, -1, 1)*toSInt)
  )
}

private[buffer] final class ViewFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SInt(shared, buff, offset, stride) with DataView[Float1, SInt] {
  val backingSeq = new BufferFloat1SInt(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, -1, 1)*toSInt)
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  shared: AnyRef, buff: IntBuffer, offset: Int, stride: Int
) extends BaseFloat1[UInt](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayFloat1UInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayFloat1UInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1UInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1UInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1UInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayFloat1UInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqFloat1UInt(rarray, buff, 0, 1) with DataArray[Float1, UInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float((long(rarray(i)) & 0xFFFFFFFFL)*fromUInt)
  def update(i: Int, v: Float) :Unit = warray(i) = uround(clamp(v, 0, 1)*toUInt)
}

private[buffer] final class BufferFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1UInt(shared, buff, 0, 1) with DataBuffer[Float1, UInt] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buff.get(i)) & 0xFFFFFFFFL)*fromUInt
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    uround(clamp(v, 0, 1)*toUInt)
  )
}

private[buffer] final class ViewFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UInt(shared, buff, offset, stride) with DataView[Float1, UInt] {
  val backingSeq = new BufferFloat1UInt(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = float(
    (long(buff.get(offset + i*stride)) & 0xFFFFFFFFL)*fromUInt
  )
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    uround(clamp(v, 0, 1)*toUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqFloat1HalfFloat(
  shared: AnyRef, buff: ShortBuffer, offset: Int, stride: Int
) extends BaseFloat1[HalfFloat](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayFloat1HalfFloat(array, array, ShortBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Short]) =
    new ArrayFloat1HalfFloat(array, array, ShortBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferFloat1HalfFloat(buff, buff.asShortBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1HalfFloat(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1HalfFloat(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1HalfFloat(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqFloat1HalfFloat(rarray, buff, 0, 1) with DataArray[Float1, HalfFloat] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1HalfFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(rarray(i))
  def update(i: Int, v: Float) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1HalfFloat(shared, buff, 0, 1) with DataBuffer[Float1, HalfFloat] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buff.get(i))
  def update(i: Int, v: Float) :Unit = buff.put(i, toHalfFloat(v))
}

private[buffer] final class ViewFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1HalfFloat(shared, buff, offset, stride) with DataView[Float1, HalfFloat] {
  val backingSeq = new BufferFloat1HalfFloat(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Float = fromHalfFloat(buff.get(offset + i*stride))
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqFloat1RawFloat(
  shared: AnyRef, buff: FloatBuffer, offset: Int, stride: Int
) extends BaseFloat1[RawFloat](shared, buff, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()
  
  final def mkReadDataArray(size: Int) = {
    val array = new Array[Float](size)
    new ArrayFloat1RawFloat(array, array, FloatBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Float]) =
    new ArrayFloat1RawFloat(array, array, FloatBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferFloat1RawFloat(buff, buff.asFloatBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferFloat1RawFloat(byteBuffer, byteBuffer.asFloatBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewFloat1RawFloat(
      byteBuffer, byteBuffer.asFloatBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayFloat1RawFloat(
  rarray: Array[Float], warray: Array[Float], buff: FloatBuffer
) extends SeqFloat1RawFloat(rarray, buff, 0, 1) with DataArray[Float1, RawFloat] {
  def this() = this(eaFloat, eaFloat, ebFloat)

  private[buffer] override def mkBindingBuffer() = FloatBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1RawFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer
) extends SeqFloat1RawFloat(shared, buff, 0, 1) with DataBuffer[Float1, RawFloat] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1RawFloat(shared, buff, offset, stride) with DataView[Float1, RawFloat] {
  val backingSeq = new BufferFloat1RawFloat(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(offset + i*stride, v)
}
