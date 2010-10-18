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

package simplex3d.buffer
package floatm

import java.nio._
import scala.reflect.Manifest
import simplex3d.math.floatm.FloatMath._
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

private[buffer] sealed abstract class BaseFloat1[+R <: DefinedFloat](
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseSeq[Float1, Float, Float, R](shared, backing, ro, off, str) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Float
  final def components: Int = 1
}


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[SByte](shared, backing, ro, off, str) {
  final def rawType = RawType.SByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1SByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1SByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1SByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1SByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqFloat1SByte(rarray, null, warray == null, 0, 1) with DataArray[Float1, SByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SByte(rarray, null)

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, -1, 1)*toSByte)).toByte
}

private[buffer] final class BufferFloat1SByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1SByte(shared, null, ro, 0, 1) with DataBuffer[Float1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SByte(shared, true)

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    (iround(clamp(v, -1, 1)*toSByte)).toByte
  )
}

private[buffer] final class ViewFloat1SByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1SByte(
  shared, new BufferFloat1SByte(shared, ro), ro, off, str
) with DataView[Float1, SByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SByte(shared, true, offset, stride)

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*stride)
    if (v < -127) -1 else (v*fromSByte).toFloat
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    (iround(clamp(v, -1, 1)*toSByte)).toByte
  )
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[UByte](shared, backing, ro, off, str) {
  final def rawType = RawType.UByte
  final def normalized = true

  final def mkDataArray(array: Array[Byte]) =
    new ArrayFloat1UByte(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1UByte(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1UByte(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1UByte(
  rarray: Array[Byte], warray: Array[Byte]
) extends SeqFloat1UByte(rarray, null, warray == null, 0, 1) with DataArray[Float1, UByte] {
  def this() = this(emptyByte, emptyByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UByte(rarray, null)

  def apply(i: Int) :Float = ((rarray(i) & 0xFF)*fromUByte).toFloat
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, 0, 1)*toUByte)).toByte
}

private[buffer] final class BufferFloat1UByte(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1UByte(shared, null, ro, 0, 1) with DataBuffer[Float1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UByte(shared, true)

  def apply(i: Int) :Float = ((buffer.get(i) & 0xFF)*fromUByte).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    (iround(clamp(v, 0, 1)*toUByte)).toByte
  )
}

private[buffer] final class ViewFloat1UByte(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1UByte(
  shared, new BufferFloat1UByte(shared, ro), ro, off, str
) with DataView[Float1, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UByte(shared, true, offset, stride)

  def apply(i: Int) :Float = (
    (buffer.get(offset + i*stride) & 0xFF)*fromUByte
  ).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    (iround(clamp(v, 0, 1)*toUByte)).toByte
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[SShort](shared, backing, ro, off, str) {
  final def rawType = RawType.SShort
  final def normalized = true

  final def mkDataArray(array: Array[Short]) =
    new ArrayFloat1SShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1SShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1SShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1SShort(
  rarray: Array[Short], warray: Array[Short]
) extends SeqFloat1SShort(rarray, null, warray == null, 0, 1) with DataArray[Float1, SShort] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SShort(rarray, null)

  def apply(i: Int) :Float = {
    val v = rarray(i)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit =
    warray(i) = (iround(clamp(v, -1, 1)*toSShort)).toShort
}

private[buffer] final class BufferFloat1SShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1SShort(shared, null, ro, 0, 1) with DataBuffer[Float1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SShort(shared, true)

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    (iround(clamp(v, -1, 1)*toSShort)).toShort
  )
}

private[buffer] final class ViewFloat1SShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1SShort(
  shared, new BufferFloat1SShort(shared, ro), ro, off, str
) with DataView[Float1, SShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SShort(shared, true, offset, stride)

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*stride)
    if (v < -32767) -1 else (v*fromSShort).toFloat
  }
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    (iround(clamp(v, -1, 1)*toSShort)).toShort
  )
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[UShort](shared, backing, ro, off, str) {
  final def rawType = RawType.UShort
  final def normalized = true

  final def mkDataArray(array: Array[Char]) =
    new ArrayFloat1UShort(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1UShort(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1UShort(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1UShort(
  rarray: Array[Char], warray: Array[Char]
) extends SeqFloat1UShort(rarray, null, warray == null, 0, 1) with DataArray[Float1, UShort] {
  def this() = this(emptyChar, emptyChar)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UShort(rarray, null)

  def apply(i: Int) :Float = (rarray(i)*fromUShort).toFloat
  def update(i: Int, v: Float) =
    warray(i) = iround(clamp(v, 0, 1)*toUShort).toChar
}

private[buffer] final class BufferFloat1UShort(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1UShort(shared, null, ro, 0, 1) with DataBuffer[Float1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UShort(shared, true)

  def apply(i: Int) :Float = (buffer.get(i)*fromUShort).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    iround(clamp(v, 0, 1)*toUShort).toChar
  )
}

private[buffer] final class ViewFloat1UShort(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1UShort(
  shared, new BufferFloat1UShort(shared, ro), ro, off, str
) with DataView[Float1, UShort] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UShort(shared, true, offset, stride)

  def apply(i: Int) :Float = (buffer.get(offset + i*stride)*fromUShort).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    iround(clamp(v, 0, 1)*toUShort).toChar
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[SInt](shared, backing, ro, off, str) {
  final def rawType = RawType.SInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1SInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1SInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1SInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1SInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqFloat1SInt(rarray, null, warray == null, 0, 1) with DataArray[Float1, SInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1SInt(rarray, null)

  def apply(i: Int) :Float = (rarray(i)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit =
    warray(i) = lround(clamp(v, -1, 1)*toSInt)
}

private[buffer] final class BufferFloat1SInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1SInt(shared, null, ro, 0, 1) with DataBuffer[Float1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1SInt(shared, true)

  def apply(i: Int) :Float = (buffer.get(i)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    lround(clamp(v, -1, 1)*toSInt)
  )
}

private[buffer] final class ViewFloat1SInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1SInt(
  shared, new BufferFloat1SInt(shared, ro), ro, off, str
) with DataView[Float1, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1SInt(shared, true, offset, stride)

  def apply(i: Int) :Float = (buffer.get(offset + i*stride)*fromSInt).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    lround(clamp(v, -1, 1)*toSInt)
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[UInt](shared, backing, ro, off, str) {
  final def rawType = RawType.UInt
  final def normalized = true

  final def mkDataArray(array: Array[Int]) =
    new ArrayFloat1UInt(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1UInt(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1UInt(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1UInt(
  rarray: Array[Int], warray: Array[Int]
) extends SeqFloat1UInt(rarray, null, warray == null, 0, 1) with DataArray[Float1, UInt] {
  def this() = this(emptyInt, emptyInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1UInt(rarray, null)

  def apply(i: Int) :Float = ((rarray(i).toLong & 0xFFFFFFFFL)*fromUInt).toFloat
  def update(i: Int, v: Float) :Unit = warray(i) = lround(clamp(v, 0, 1)*toUInt)
}

private[buffer] final class BufferFloat1UInt(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1UInt(shared, null, ro, 0, 1) with DataBuffer[Float1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1UInt(shared, true)

  def apply(i: Int) :Float = (
    (buffer.get(i).toLong & 0xFFFFFFFFL)*fromUInt
  ).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    i,
    lround(clamp(v, 0, 1)*toUInt)
  )
}

private[buffer] final class ViewFloat1UInt(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1UInt(
  shared, new BufferFloat1UInt(shared, ro), ro, off, str
) with DataView[Float1, UInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1UInt(shared, true, offset, stride)

  def apply(i: Int) :Float = (
    (buffer.get(offset + i*stride).toLong & 0xFFFFFFFFL)*fromUInt
  ).toFloat
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    lround(clamp(v, 0, 1)*toUInt)
  )
}


// Type: HalfFloat
private[buffer] sealed abstract class SeqFloat1HalfFloat(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[HalfFloat](shared, backing, ro, off, str) {
  final def rawType: Int = RawType.HalfFloat
  final def normalized = false

  final def mkDataArray(array: Array[Short]) =
    new ArrayFloat1HalfFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1HalfFloat(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1HalfFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1HalfFloat(
  rarray: Array[Short], warray: Array[Short]
) extends SeqFloat1HalfFloat(rarray, null, warray == null, 0, 1) with DataArray[Float1, HalfFloat] {
  def this() = this(emptyShort, emptyShort)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1HalfFloat(rarray, null)

  def apply(i: Int) :Float = fromHalfFloat(rarray(i))
  def update(i: Int, v: Float) :Unit = warray(i) = toHalfFloat(v)
}

private[buffer] final class BufferFloat1HalfFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1HalfFloat(shared, null, ro, 0, 1) with DataBuffer[Float1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1HalfFloat(shared, true)

  def apply(i: Int) :Float = fromHalfFloat(buffer.get(i))
  def update(i: Int, v: Float) :Unit = buffer.put(i, toHalfFloat(v))
}

private[buffer] final class ViewFloat1HalfFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1HalfFloat(
  shared, new BufferFloat1HalfFloat(shared, ro), ro, off, str
) with DataView[Float1, HalfFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1HalfFloat(shared, true, offset, stride)

  def apply(i: Int) :Float = fromHalfFloat(buffer.get(offset + i*stride))
  def update(i: Int, v: Float) :Unit = buffer.put(
    offset + i*stride,
    toHalfFloat(v)
  )
}


// Type: RawFloat
private[buffer] sealed abstract class SeqFloat1RawFloat(
  shared: AnyRef, backing: AnyRef, ro: Boolean,
  off: Int, str: Int
) extends BaseFloat1[RawFloat](shared, backing, ro, off, str) {
  final def rawType: Int = RawType.RawFloat
  final def normalized = false

  final def mkDataArray(array: Array[Float]) =
    new ArrayFloat1RawFloat(array, array)
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    new BufferFloat1RawFloat(byteBuffer, byteBuffer.isReadOnly)
  }
  protected final def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int) = {
    new ViewFloat1RawFloat(byteBuffer, byteBuffer.isReadOnly, off, str)
  }
}

private[buffer] final class ArrayFloat1RawFloat(
  rarray: Array[Float], warray: Array[Float]
) extends SeqFloat1RawFloat(rarray, null, warray == null, 0, 1) with DataArray[Float1, RawFloat] {
  def this() = this(emptyFloat, emptyFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayFloat1RawFloat(rarray, null)

  def apply(i: Int) :Float = rarray(i)
  def update(i: Int, v: Float) :Unit = warray(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  shared: ByteBuffer, ro: Boolean
) extends SeqFloat1RawFloat(shared, null, ro, 0, 1) with DataBuffer[Float1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferFloat1RawFloat(shared, true)

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) :Unit = buffer.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  shared: ByteBuffer, ro: Boolean, off: Int, str: Int
) extends SeqFloat1RawFloat(
  shared, new BufferFloat1RawFloat(shared, ro), ro, off, str
) with DataView[Float1, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewFloat1RawFloat(shared, true, offset, stride)

  def apply(i: Int) :Float = buffer.get(offset + i*stride)
  def update(i: Int, v: Float) :Unit = buffer.put(offset + i*stride, v)
}
