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
import simplex3d.math.floatm.FloatMath._
import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
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
  final val toSInt = 2147483647d
  final val toUInt = 4294967295d

  final def iround(x: Float) :Int = {
    if (x >= 0) (x + 0.5f).toInt
    else (x - 0.5f).toInt
  }

  final def lround(x: Double) :Int = {
    if (x >= 0) ((x + 0.5).toLong).toInt
    else ((x - 0.5).toLong).toInt
  }
}
import Shared._

private[buffer] sealed abstract class BaseFloat1[+R <: ReadableFloat](
  shared: AnyRef, buff: R#BufferType, backing: AnyRef, offset: Int, stride: Int
) extends BaseSeq[Float1, Float, R](shared, buff, backing, offset, stride) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Float
  final def components: Int = 1

  private[buffer] def mkBindingBuffer() = asReadOnlyBuffer()
}


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  shared: AnyRef, buff: ByteBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[SByte](shared, buff, backing, offset, stride) {
  final def rawType = RawData.SByte
  final def normalized = true

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
) extends SeqFloat1SByte(rarray, buff, null, 0, 1) with DataArray[Float1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, -1, 1)*toSByte)).toByte
}

private[buffer] final class BufferFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1SByte(shared, buff, null, 0, 1) with DataBuffer[Float1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    (iround(clamp(v, -1, 1)*toSByte)).toByte
  )
}

private[buffer] final class ViewFloat1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SByte(
  shared, buff, new BufferFloat1SByte(shared, buff), offset, stride
) with DataView[Float1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    (iround(clamp(v, -1, 1)*toSByte)).toByte
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  shared: AnyRef, buff: ByteBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[UByte](shared, buff, backing, offset, stride) {
  final def rawType = RawData.UByte
  final def normalized = true

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
) extends SeqFloat1UByte(rarray, buff, null, 0, 1) with DataArray[Float1, UByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = ((rarray(i) & 0xFF)*fromUByte).toFloat
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, 0, 1)*toUByte)).toByte
}

private[buffer] final class BufferFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqFloat1UByte(shared, buff, null, 0, 1) with DataBuffer[Float1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = ((buff.get(i) & 0xFF)*fromUByte).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    (iround(clamp(v, 0, 1)*toUByte)).toByte
  )
}

private[buffer] final class ViewFloat1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UByte(
  shared, buff, new BufferFloat1UByte(shared, buff), offset, stride
) with DataView[Float1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = (
    (buff.get(offset + i*stride) & 0xFF)*fromUByte
  ).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    (iround(clamp(v, 0, 1)*toUByte)).toByte
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  shared: AnyRef, buff: ShortBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[SShort](shared, buff, backing, offset, stride) {
  final def rawType = RawData.SShort
  final def normalized = true

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
) extends SeqFloat1SShort(rarray, buff, null, 0, 1) with DataArray[Float1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, -1, 1)*toSShort)).toShort
}

private[buffer] final class BufferFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1SShort(shared, buff, null, 0, 1) with DataBuffer[Float1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = {
    val v = buff.get(i)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    (iround(clamp(v, -1, 1)*toSShort)).toShort
  )
}

private[buffer] final class ViewFloat1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SShort(
  shared, buff, new BufferFloat1SShort(shared, buff), offset, stride
) with DataView[Float1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = {
    val v = buff.get(offset + i*stride)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    (iround(clamp(v, -1, 1)*toSShort)).toShort
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  shared: AnyRef, buff: CharBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[UShort](shared, buff, backing, offset, stride) {
  final def rawType = RawData.UShort
  final def normalized = true

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
) extends SeqFloat1UShort(rarray, buff, null, 0, 1) with DataArray[Float1, UShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override def mkBindingBuffer() = CharBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = (rarray(i)*fromUShort).toFloat
  def update(i: Int, v: Float) =
    warray(i) = iround(clamp(v, 0, 1)*toUShort).toChar
}

private[buffer] final class BufferFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqFloat1UShort(shared, buff, null, 0, 1) with DataBuffer[Float1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = (buff.get(i)*fromUShort).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    iround(clamp(v, 0, 1)*toUShort).toChar
  )
}

private[buffer] final class ViewFloat1UShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UShort(
  shared, buff, new BufferFloat1UShort(shared, buff), offset, stride
) with DataView[Float1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = (buff.get(offset + i*stride)*fromUShort).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toUShort).toChar
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  shared: AnyRef, buff: IntBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[SInt](shared, buff, backing, offset, stride) {
  final def rawType = RawData.SInt
  final def normalized = true

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
) extends SeqFloat1SInt(rarray, buff, null, 0, 1) with DataArray[Float1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = (rarray(i)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit =
    warray(i) = lround(clamp(v, -1, 1)*toSInt)
}

private[buffer] final class BufferFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1SInt(shared, buff, null, 0, 1) with DataBuffer[Float1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = (buff.get(i)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    lround(clamp(v, -1, 1)*toSInt)
  )
}

private[buffer] final class ViewFloat1SInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1SInt(
  shared, buff, new BufferFloat1SInt(shared, buff), offset, stride
) with DataView[Float1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = (buff.get(offset + i*stride)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    lround(clamp(v, -1, 1)*toSInt)
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  shared: AnyRef, buff: IntBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[UInt](shared, buff, backing, offset, stride) {
  final def rawType = RawData.UInt
  final def normalized = true

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
) extends SeqFloat1UInt(rarray, buff, null, 0, 1) with DataArray[Float1, UInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = ((rarray(i).toLong & 0xFFFFFFFFL)*fromUInt).toFloat
  def update(i: Int, v: Float) :Unit = warray(i) = lround(clamp(v, 0, 1)*toUInt)
}

private[buffer] final class BufferFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqFloat1UInt(shared, buff, null, 0, 1) with DataBuffer[Float1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = (
    (buff.get(i).toLong & 0xFFFFFFFFL)*fromUInt
  ).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    i,
    lround(clamp(v, 0, 1)*toUInt)
  )
}

private[buffer] final class ViewFloat1UInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1UInt(
  shared, buff, new BufferFloat1UInt(shared, buff), offset, stride
) with DataView[Float1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = (
    (buff.get(offset + i*stride).toLong & 0xFFFFFFFFL)*fromUInt
  ).toFloat
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    lround(clamp(v, 0, 1)*toUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqFloat1HalfFloat(
  shared: AnyRef, buff: ShortBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[HalfFloat](shared, buff, backing, offset, stride) {
  final def rawType: Int = RawData.HalfFloat
  final def normalized = false

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
) extends SeqFloat1HalfFloat(rarray, buff, null, 0, 1) with DataArray[Float1, HalfFloat] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1HalfFloat(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = fromHalfFloat(rarray(i))
  def update(i: Int, v: Float) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqFloat1HalfFloat(shared, buff, null, 0, 1) with DataBuffer[Float1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = fromHalfFloat(buff.get(i))
  def update(i: Int, v: Float) :Unit = buff.put(i, toHalfFloat(v))
}

private[buffer] final class ViewFloat1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1HalfFloat(
  shared, buff, new BufferFloat1HalfFloat(shared, buff), offset, stride
) with DataView[Float1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1HalfFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = fromHalfFloat(buff.get(offset + i*stride))
  def update(i: Int, v: Float) :Unit = buff.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqFloat1RawFloat(
  shared: AnyRef, buff: FloatBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseFloat1[RawFloat](shared, buff, backing, offset, stride) {
  final def rawType: Int = RawData.RawFloat
  final def normalized = false

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
) extends SeqFloat1RawFloat(rarray, buff, null, 0, 1) with DataArray[Float1, RawFloat] {
  def this() = this(eaFloat, eaFloat, ebFloat)

  private[buffer] override def mkBindingBuffer() = FloatBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1RawFloat(rarray, null, buffer.asReadOnlyBuffer())

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer
) extends SeqFloat1RawFloat(shared, buff, null, 0, 1) with DataBuffer[Float1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def apply(i: Int) :Float = buff.get(i)
  def update(i: Int, v: Float) :Unit = buff.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer,
  offset: Int,
  stride: Int
) extends SeqFloat1RawFloat(
  shared, buff, new BufferFloat1RawFloat(shared, buff), offset, stride
) with DataView[Float1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1RawFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def apply(i: Int) :Float = buff.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buff.put(offset + i*stride, v)
}
