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

package simplex3d.buffer.doublem

import java.nio._
import scala.reflect.Manifest
import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._
import simplex3d.buffer.{allocateDirectBuffer => alloc, _}
import simplex3d.buffer.Util._
import simplex3d.buffer.HalfFloatUtil.{
  doubleToHalfFloat => toHalfFloat, doubleFromHalfFloat => fromHalfFloat
}


/**
 * @author Aleksey Nikiforov (lex)
 */
private[doublem] object Shared {

  final val fromSByte = 0.00787401574803149606
  final val fromUByte = 0.00392156862745098039
  final val fromSShort = 3.05185094759971922971e-5
  final val fromUShort = 1.52590218966964217594e-5
  final val fromSInt = 4.65661287524579692411e-10
  final val fromUInt = 2.32830643708079737543e-10

  final val toSByte = 127d
  final val toUByte = 255d
  final val toSShort = 32767d
  final val toUShort = 65535d
  final val toSInt = 2147483647d
  final val toUInt = 4294967295d

  final def iround(x: Double) :Int = {
    if (x >= 0) int(x + 0.5)
    else int(x - 0.5)
  }

  final def lround(x: Double) :Int = {
    if (x >= 0) int(long(x + 0.5))
    else int(long(x - 0.5))
  }
}
import Shared._

private[buffer] sealed abstract class BaseDouble1[+R <: ReadableDouble](
  shared: AnyRef, buff: R#BufferType, backing: AnyRef, offset: Int, stride: Int
) extends BaseSeq[Double1, Double, R](shared, buff, backing, offset, stride) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Double
  final def components: Int = 1

  private[buffer] def mkBindingBuffer() = asReadOnlyBuffer()
}


// Type: SByte
private[buffer] sealed abstract class SeqDouble1SByte(
  shared: AnyRef, buff: ByteBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[SByte](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayDouble1SByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayDouble1SByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferDouble1SByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1SByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1SByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayDouble1SByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqDouble1SByte(rarray, buff, null, 0, 1) with DataArray[Double1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = rarray(i)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Double) :Unit =
    warray(i) = byte(iround(clamp(v, -1, 1)*toSByte))
}

private[buffer] final class BufferDouble1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqDouble1SByte(shared, buff, null, 0, 1) with DataBuffer[Double1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(i)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    byte(iround(clamp(v, -1, 1)*toSByte))
  )
}

private[buffer] final class ViewDouble1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1SByte(
  shared, buff, new BufferDouble1SByte(shared, buff), offset, stride
) with DataView[Double1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(offset + i*stride)
    if (v < -127) -1 else float(v*fromSByte)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, -1, 1)*toSByte))
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqDouble1UByte(
  shared: AnyRef, buff: ByteBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[UByte](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayDouble1UByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayDouble1UByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferDouble1UByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1UByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1UByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayDouble1UByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqDouble1UByte(rarray, buff, null, 0, 1) with DataArray[Double1, UByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = float((rarray(i) & 0xFF)*fromUByte)
  def update(i: Int, v: Double) :Unit =
    warray(i) = byte(iround(clamp(v, 0, 1)*toUByte))
}

private[buffer] final class BufferDouble1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqDouble1UByte(shared, buff, null, 0, 1) with DataBuffer[Double1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = float((buff.get(i) & 0xFF)*fromUByte)
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    byte(iround(clamp(v, 0, 1)*toUByte))
  )
}

private[buffer] final class ViewDouble1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1UByte(
  shared, buff, new BufferDouble1UByte(shared, buff), offset, stride
) with DataView[Double1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(
    (buff.get(offset + i*stride) & 0xFF)*fromUByte
  )
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    byte(iround(clamp(v, 0, 1)*toUByte))
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqDouble1SShort(
  shared: AnyRef, buff: ShortBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[SShort](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayDouble1SShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Short]) =
    new ArrayDouble1SShort(array, array, ShortBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferDouble1SShort(buff, buff.asShortBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1SShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1SShort(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayDouble1SShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqDouble1SShort(rarray, buff, null, 0, 1) with DataArray[Double1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = rarray(i)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Double) :Unit =
    warray(i) = short(iround(clamp(v, -1, 1)*toSShort))
}

private[buffer] final class BufferDouble1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqDouble1SShort(shared, buff, null, 0, 1) with DataBuffer[Double1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(i)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    short(iround(clamp(v, -1, 1)*toSShort))
  )
}

private[buffer] final class ViewDouble1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1SShort(
  shared, buff, new BufferDouble1SShort(shared, buff), offset, stride
) with DataView[Double1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(offset + i*stride)
    if (v < -32767) -1 else float(v*fromSShort)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    short(iround(clamp(v, -1, 1)*toSShort))
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqDouble1UShort(
  shared: AnyRef, buff: CharBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[UShort](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayDouble1UShort(array, array, CharBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Char]) =
    new ArrayDouble1UShort(array, array, CharBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferDouble1UShort(buff, buff.asCharBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1UShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1UShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayDouble1UShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqDouble1UShort(rarray, buff, null, 0, 1) with DataArray[Double1, UShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override def mkBindingBuffer() = CharBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(rarray(i)*fromUShort)
  def update(i: Int, v: Double) =
    warray(i) = iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
}

private[buffer] final class BufferDouble1UShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqDouble1UShort(shared, buff, null, 0, 1) with DataBuffer[Double1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(buff.get(i)*fromUShort)
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
  )
}

private[buffer] final class ViewDouble1UShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1UShort(
  shared, buff, new BufferDouble1UShort(shared, buff), offset, stride
) with DataView[Double1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(buff.get(offset + i*stride)*fromUShort)
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toUShort).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqDouble1SInt(
  shared: AnyRef, buff: IntBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[SInt](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayDouble1SInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayDouble1SInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferDouble1SInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1SInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1SInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayDouble1SInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqDouble1SInt(rarray, buff, null, 0, 1) with DataArray[Double1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = rarray(i)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Double) :Unit =
    warray(i) = iround(clamp(v, -1, 1)*toSInt)
}

private[buffer] final class BufferDouble1SInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqDouble1SInt(shared, buff, null, 0, 1) with DataBuffer[Double1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1SInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(i)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    iround(clamp(v, -1, 1)*toSInt)
  )
}

private[buffer] final class ViewDouble1SInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1SInt(
  shared, buff, new BufferDouble1SInt(shared, buff), offset, stride
) with DataView[Double1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1SInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = {
    val v = buff.get(offset + i*stride)
    if (v < -2147483647) -1 else float(v*fromSInt)
  }
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    iround(clamp(v, -1, 1)*toSInt)
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqDouble1UInt(
  shared: AnyRef, buff: IntBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[UInt](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayDouble1UInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayDouble1UInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferDouble1UInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1UInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1UInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayDouble1UInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqDouble1UInt(rarray, buff, null, 0, 1) with DataArray[Double1, UInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = float((long(rarray(i)) & 0xFFFFFFFFL)*fromUInt)
  def update(i: Int, v: Double) :Unit = warray(i) = lround(clamp(v, 0, 1)*toUInt)
}

private[buffer] final class BufferDouble1UInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqDouble1UInt(shared, buff, null, 0, 1) with DataBuffer[Double1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1UInt(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(
    (long(buff.get(i)) & 0xFFFFFFFFL)*fromUInt
  )
  def update(i: Int, v: Double) :Unit = buff.put(
    i,
    lround(clamp(v, 0, 1)*toUInt)
  )
}

private[buffer] final class ViewDouble1UInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1UInt(
  shared, buff, new BufferDouble1UInt(shared, buff), offset, stride
) with DataView[Double1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1UInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Double = float(
    (long(buff.get(offset + i*stride)) & 0xFFFFFFFFL)*fromUInt
  )
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    lround(clamp(v, 0, 1)*toUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqDouble1HalfFloat(
  shared: AnyRef, buff: ShortBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[HalfFloat](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayDouble1HalfFloat(array, array, ShortBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Short]) =
    new ArrayDouble1HalfFloat(array, array, ShortBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferDouble1HalfFloat(buff, buff.asShortBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1HalfFloat(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1HalfFloat(
      byteBuffer, byteBuffer.asShortBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayDouble1HalfFloat(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqDouble1HalfFloat(rarray, buff, null, 0, 1) with DataArray[Double1, HalfFloat] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1HalfFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Double = fromHalfFloat(rarray(i))
  def update(i: Int, v: Double) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferDouble1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqDouble1HalfFloat(shared, buff, null, 0, 1) with DataBuffer[Double1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1HalfFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Double = fromHalfFloat(buff.get(i))
  def update(i: Int, v: Double) :Unit = buff.put(i, toHalfFloat(v))
}

private[buffer] final class ViewDouble1HalfFloat(
  shared: ByteBuffer,
  buff: ShortBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1HalfFloat(
  shared, buff, new BufferDouble1HalfFloat(shared, buff), offset, stride
) with DataView[Double1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1HalfFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.HalfFloat

  def apply(i: Int) :Double = fromHalfFloat(buff.get(offset + i*stride))
  def update(i: Int, v: Double) :Unit = buff.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqDouble1RawFloat(
  shared: AnyRef, buff: FloatBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[RawFloat](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Float](size)
    new ArrayDouble1RawFloat(array, array, FloatBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Float]) =
    new ArrayDouble1RawFloat(array, array, FloatBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferDouble1RawFloat(buff, buff.asFloatBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1RawFloat(byteBuffer, byteBuffer.asFloatBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1RawFloat(
      byteBuffer, byteBuffer.asFloatBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayDouble1RawFloat(
  rarray: Array[Float], warray: Array[Float], buff: FloatBuffer
) extends SeqDouble1RawFloat(rarray, buff, null, 0, 1) with DataArray[Double1, RawFloat] {
  def this() = this(eaFloat, eaFloat, ebFloat)

  private[buffer] override def mkBindingBuffer() = FloatBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1RawFloat(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) :Unit = warray(i) = float(v)
}

private[buffer] final class BufferDouble1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer
) extends SeqDouble1RawFloat(shared, buff, null, 0, 1) with DataBuffer[Double1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1RawFloat(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) :Unit = buff.put(i, float(v))
}

private[buffer] final class ViewDouble1RawFloat(
  shared: ByteBuffer,
  buff: FloatBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1RawFloat(
  shared, buff, new BufferDouble1RawFloat(shared, buff), offset, stride
) with DataView[Double1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1RawFloat(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawFloat

  def apply(i: Int) :Double = buff.get(offset + i*stride)
  def update(i: Int, v: Double) :Unit = buff.put(offset + i*stride, float(v))
}


// Type: RawDouble
private[buffer] sealed abstract class SeqDouble1RawDouble(
  shared: AnyRef, buff: DoubleBuffer, backing: AnyRef, offset: Int, stride: Int
) extends BaseDouble1[RawDouble](shared, buff, backing, offset, stride) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()
  
  final def mkReadDataArray(size: Int) = {
    val array = new Array[Double](size)
    new ArrayDouble1RawDouble(array, array, DoubleBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Double]) =
    new ArrayDouble1RawDouble(array, array, DoubleBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferDouble1RawDouble(buff, buff.asDoubleBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferDouble1RawDouble(byteBuffer, byteBuffer.asDoubleBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewDouble1RawDouble(
      byteBuffer, byteBuffer.asDoubleBuffer(), offset, stride
    )
  }
}

private[buffer] final class ArrayDouble1RawDouble(
  rarray: Array[Double], warray: Array[Double], buff: DoubleBuffer
) extends SeqDouble1RawDouble(rarray, buff, null, 0, 1) with DataArray[Double1, RawDouble] {
  def this() = this(eaDouble, eaDouble, ebDouble)

  private[buffer] override def mkBindingBuffer() = DoubleBuffer.wrap(rarray)
  protected[buffer] def mkReadOnlyInstance() = new ArrayDouble1RawDouble(rarray, null, buffer.asReadOnlyBuffer())

  def normalized: Boolean = false
  def rawType: Int = RawData.RawDouble

  def apply(i: Int) :Double = rarray(i)
  def update(i: Int, v: Double) :Unit = warray(i) = v
}

private[buffer] final class BufferDouble1RawDouble(
  shared: ByteBuffer,
  buff: DoubleBuffer
) extends SeqDouble1RawDouble(shared, buff, null, 0, 1) with DataBuffer[Double1, RawDouble] {
  protected[buffer] def mkReadOnlyInstance() = new BufferDouble1RawDouble(
    shared, buffer.asReadOnlyBuffer()
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawDouble

  def apply(i: Int) :Double = buff.get(i)
  def update(i: Int, v: Double) :Unit = buff.put(i, v)
}

private[buffer] final class ViewDouble1RawDouble(
  shared: ByteBuffer,
  buff: DoubleBuffer,
  offset: Int,
  stride: Int
) extends SeqDouble1RawDouble(
  shared, buff, new BufferDouble1RawDouble(shared, buff), offset, stride
) with DataView[Double1, RawDouble] {
  protected[buffer] def mkReadOnlyInstance() = new ViewDouble1RawDouble(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def normalized: Boolean = false
  def rawType: Int = RawData.RawDouble

  def apply(i: Int) :Double = buff.get(offset + i*stride)
  def update(i: Int, v: Double) :Unit = buff.put(offset + i*stride, v)
}
