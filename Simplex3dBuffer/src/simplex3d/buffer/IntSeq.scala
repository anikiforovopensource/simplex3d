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
import simplex3d.math._
import simplex3d.buffer.{allocateByteBuffer => alloc}
import simplex3d.buffer.Util._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseInt1[+R <: ReadableInt](
  shared: AnyRef, buff: R#BufferType
) extends BaseSeq[Int1, Int, R](shared, buff) {
  final def elementManifest = componentManifest
  final def componentManifest = Manifest.Int
  final def components: Int = 1

  private[buffer] def mkBindingBuffer() = asReadOnlyBuffer()
}


// Type: UByte
private[buffer] sealed abstract class SeqInt1UByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseInt1[UByte](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayInt1UByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayInt1UByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferInt1UByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqInt1UByte(rarray, buff) with IndexArray[UByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i) & 0xFF
  def update(i: Int, v: Int) :Unit = warray(i) = byte(v)
}

private[buffer] final class BufferInt1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqInt1UByte(shared, buff) with IndexBuffer[UByte] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i) & 0xFF
  def update(i: Int, v: Int) :Unit = buff.put(i, byte(v))
}

private[buffer] final class ViewInt1UByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UByte(shared, buff) with DataView[Int1, UByte] {
  val backingSeq = new BufferInt1UByte(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride) & 0xFF
  def update(i: Int, v: Int) :Unit = buff.put(offset + i*stride, byte(v))
}


// Type: UShort
private[buffer] sealed abstract class SeqInt1UShort(
  shared: AnyRef, buff: CharBuffer
) extends BaseInt1[UShort](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Char](size)
    new ArrayInt1UShort(array, array, CharBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Char]) =
    new ArrayInt1UShort(array, array, CharBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferInt1UShort(buff, buff.asCharBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UShort(byteBuffer, byteBuffer.asCharBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UShort(byteBuffer, byteBuffer.asCharBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UShort(
  rarray: Array[Char], warray: Array[Char], buff: CharBuffer
) extends SeqInt1UShort(
  rarray, buff
) with IndexArray[UShort] {
  def this() = this(eaChar, eaChar, ebChar)

  private[buffer] override def mkBindingBuffer() = CharBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferInt1UShort(
  shared: ByteBuffer,
  buff: CharBuffer
) extends SeqInt1UShort(shared, buff) with IndexBuffer[UShort] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v.asInstanceOf[Char])
}

private[buffer] final class ViewInt1UShort(
  shared: ByteBuffer,
  buff: CharBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UShort(shared, buff) with DataView[Int1, UShort] {
  val backingSeq = new BufferInt1UShort(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buff.put(
    offset + i*stride,
    v.asInstanceOf[Char]
  )
}


// Type: UInt
private[buffer] sealed abstract class SeqInt1UInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseInt1[UInt](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayInt1UInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayInt1UInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferInt1UInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1UInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1UInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1UInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqInt1UInt(rarray, buff) with IndexArray[UInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1UInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1UInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqInt1UInt(shared, buff) with IndexBuffer[UInt]{
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1UInt(shared, buffer.asReadOnlyBuffer())

  def rawType = RawData.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}

private[buffer] final class ViewInt1UInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1UInt(shared, buff) with DataView[Int1, UInt] {
  val backingSeq = new BufferInt1UInt(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1UInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buff.put(offset + i*stride, v)
}


// Type: SByte
private[buffer] sealed abstract class SeqInt1SByte(
  shared: AnyRef, buff: ByteBuffer
) extends BaseInt1[SByte](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Byte](size)
    new ArrayInt1SByte(array, array, ByteBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Byte]) =
    new ArrayInt1SByte(array, array, ByteBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size)
    new BufferInt1SByte(buff, buff.duplicate())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SByte(byteBuffer, byteBuffer.duplicate())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SByte(byteBuffer, byteBuffer.duplicate(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SByte(
  rarray: Array[Byte], warray: Array[Byte], buff: ByteBuffer
) extends SeqInt1SByte(rarray, buff) with DataArray[Int1, SByte] {
  def this() = this(eaByte, eaByte, ebByte)

  private[buffer] override def mkBindingBuffer() = ByteBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SByte(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = byte(v)
}

private[buffer] final class BufferInt1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer
) extends SeqInt1SByte(shared, buff) with DataBuffer[Int1, SByte] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SByte(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, byte(v))
}

private[buffer] final class ViewInt1SByte(
  shared: ByteBuffer,
  buff: ByteBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SByte(shared, buff) with DataView[Int1, SByte] {
  val backingSeq = new BufferInt1SByte(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SByte(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buff.put(offset + i*stride, byte(v))
}


// Type: SShort
private[buffer] sealed abstract class SeqInt1SShort(
  shared: AnyRef, buff: ShortBuffer
) extends BaseInt1[SShort](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Short](size)
    new ArrayInt1SShort(array, array, ShortBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Short]) =
    new ArrayInt1SShort(array, array, ShortBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*2)
    new BufferInt1SShort(buff, buff.asShortBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SShort(byteBuffer, byteBuffer.asShortBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SShort(byteBuffer, byteBuffer.asShortBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SShort(
  rarray: Array[Short], warray: Array[Short], buff: ShortBuffer
) extends SeqInt1SShort(
  rarray, buff
) with DataArray[Int1, SShort] {
  def this() = this(eaShort, eaShort, ebShort)

  private[buffer] override def mkBindingBuffer() = ShortBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SShort(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = short(v)
}

private[buffer] final class BufferInt1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer
) extends SeqInt1SShort(shared, buff) with DataBuffer[Int1, SShort] {
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SShort(
    shared, buffer.asReadOnlyBuffer()
  )

  def rawType = RawData.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, short(v))
}

private[buffer] final class ViewInt1SShort(
  shared: ByteBuffer,
  buff: ShortBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SShort(shared, buff) with DataView[Int1, SShort] {
  val backingSeq = new BufferInt1SShort(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SShort(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buff.put(offset + i*stride, short(v))
}


// Type: SInt
private[buffer] sealed abstract class SeqInt1SInt(
  shared: AnyRef, buff: IntBuffer
) extends BaseInt1[SInt](shared, buff) {
  final def asReadOnlyBuffer() = buffer.asReadOnlyBuffer()
  final def asBuffer() = buffer.duplicate()

  final def mkReadDataArray(size: Int) = {
    val array = new Array[Int](size)
    new ArrayInt1SInt(array, array, IntBuffer.wrap(array))
  }
  final def mkReadDataArray(array: Array[Int]) =
    new ArrayInt1SInt(array, array, IntBuffer.wrap(array))
  final def mkReadDataBuffer(size: Int) = {
    val buff = alloc(size*4)
    new BufferInt1SInt(buff, buff.asIntBuffer())
  }
  final def mkReadDataBuffer(byteBuffer: ByteBuffer) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new BufferInt1SInt(byteBuffer, byteBuffer.asIntBuffer())
  }
  final def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) = {
    byteBuffer.clear(); byteBuffer.order(ByteOrder.nativeOrder())
    new ViewInt1SInt(byteBuffer, byteBuffer.asIntBuffer(), offset, stride)
  }
}

private[buffer] final class ArrayInt1SInt(
  rarray: Array[Int], warray: Array[Int], buff: IntBuffer
) extends SeqInt1SInt(rarray, buff) with DataArray[Int1, SInt] {
  def this() = this(eaInt, eaInt, ebInt)

  private[buffer] override def mkBindingBuffer() = IntBuffer.wrap(rarray)
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new ArrayInt1SInt(rarray, null, buffer.asReadOnlyBuffer())

  def rawType = RawData.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = rarray(i)
  def update(i: Int, v: Int) :Unit = warray(i) = v
}

private[buffer] final class BufferInt1SInt(
  shared: ByteBuffer,
  buff: IntBuffer
) extends SeqInt1SInt(shared, buff) with DataBuffer[Int1, SInt]{
  def backingSeq = this
  protected[buffer] def mkReadOnlyInstance() = new BufferInt1SInt(shared, buffer.asReadOnlyBuffer())

  def rawType = RawData.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(i)
  def update(i: Int, v: Int) :Unit = buff.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  shared: ByteBuffer,
  buff: IntBuffer,
  override val offset: Int,
  override val stride: Int
) extends SeqInt1SInt(shared, buff) with DataView[Int1, SInt] {
  val backingSeq = new BufferInt1SInt(shared, buff)
  protected[buffer] def mkReadOnlyInstance() = new ViewInt1SInt(
    shared, buffer.asReadOnlyBuffer(), offset, stride
  )

  def rawType = RawData.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buff.get(offset + i*stride)
  def update(i: Int, v: Int) :Unit = buff.put(offset + i*stride, v)
}
