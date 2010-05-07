/*
 * Simplex3d, FloatBuffer module
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

package simplex3d.buffer.floatm

import java.nio._
import simplex3d.buffer._
import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] sealed abstract class BaseFloat1[+D <: ReadFloat](
  buff: D#BufferType
) extends BaseSeq[Float1, Float, D](buff) {
  final def components: Int = 1

  protected final def translatePut(
    destOffset: Int,
    src: ContiguousSeq[Float1, _],
    srcOffset: Int,
    srcStep: Int,
    srcLim: Int
  ) {
    val dest = backingSeq

    var desti = destOffset
    var srci = srcOffset

    while (srci < srcLim)  {
      dest(desti) = src(srci)
      desti += step
      srci += srcStep
    }
  }
}

private[buffer] sealed abstract class ArrayFloat1[+D <: ReadFloat](
  buff: D#BufferType
) extends BaseFloat1[D](buff) with DataArray[Float1, D] {
  final def backingSeq = this
}

private[buffer] sealed abstract class BufferFloat1[+D <: ReadFloat](
  buff: D#BufferType
)extends BaseFloat1[D](buff) with DataBuffer[Float1, D] {
  final def backingSeq = this
}

private[buffer] sealed abstract class ViewFloat1[+D <: ReadFloat](
  buff: D#BufferType
) extends BaseFloat1[D](buff) with DataView[Float1, D]


// Type: SByte
private[buffer] final class ArrayFloat1SByte(
  override val array: Array[Byte]
) extends ArrayFloat1[SByte](ByteBuffer.wrap(array)) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = byte(v)
}

private[buffer] final class BufferFloat1SByte(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[SByte](byteBuffer) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, byte(v))
}

private[buffer] final class ViewFloat1SByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[SByte](byteBuffer) {
  val backingSeq: BufferFloat1[SByte] = new BufferFloat1SByte(byteBuffer)

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, byte(v))
}


// Type: UByte
private[buffer] final class ArrayFloat1UByte(
  override val array: Array[Byte]
) extends ArrayFloat1[UByte](ByteBuffer.wrap(array)) {
  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i) & 0xFF
  def update(i: Int, v: Float) = array(i) = byte(v)
}

private[buffer] final class BufferFloat1UByte(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[UByte](byteBuffer) {
  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i) & 0xFF
  def update(i: Int, v: Float) = buffer.put(i, byte(v))
}

private[buffer] final class ViewFloat1UByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[UByte](byteBuffer) {
  val backingSeq: BufferFloat1[UByte] = new BufferFloat1UByte(byteBuffer)

  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step) & 0xFF
  def update(i: Int, v: Float) = buffer.put(offset + i*step, byte(v))
}


// Type: NSByte
private[buffer] final class ArrayFloat1NSByte(
  override val array: Array[Byte]
) extends ArrayFloat1[NSByte](ByteBuffer.wrap(array)) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = array(i)
    if (v < -127) -1 else v*0.007874016f
  }
  def update(i: Int, v: Float) = array(i) = byte(clamp(v, -1, 1)*127)
}

private[buffer] final class BufferFloat1NSByte(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NSByte](byteBuffer) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -127) -1 else v*0.007874016f
  }
  def update(i: Int, v: Float) = buffer.put(
    i,
    byte(clamp(v, -1, 1)*127)
  )
}

private[buffer] final class ViewFloat1NSByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NSByte](byteBuffer) {
  val backingSeq: BufferFloat1[NSByte] = new BufferFloat1NSByte(byteBuffer)

  def componentBinding = Binding.SByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*step)
    if (v < -127) -1 else v*0.007874016f
  }
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    byte(clamp(v, -1, 1)*127)
  )
}


// Type: NUByte
private[buffer] final class ArrayFloat1NUByte(
  override val array: Array[Byte]
) extends ArrayFloat1[NUByte](ByteBuffer.wrap(array)) {
  def componentBinding = Binding.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = (array(i) & 0xFF)*0.003921569f
  def update(i: Int, v: Float) = array(i) = byte(clamp(v, 0, 1)*255)
}

private[buffer] final class BufferFloat1NUByte(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NUByte](byteBuffer) {
  def componentBinding = Binding.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = (buffer.get(i) & 0xFF)*0.003921569f
  def update(i: Int, v: Float) = buffer.put(
    i,
    byte(clamp(v, 0, 1)*255)
  )
}

private[buffer] final class ViewFloat1NUByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NUByte](byteBuffer) {
  val backingSeq: BufferFloat1[NUByte] = new BufferFloat1NUByte(byteBuffer)

  def componentBinding = Binding.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = (buffer.get(offset + i*step) & 0xFF)*0.003921569f
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    byte(clamp(v, 0, 1)*255)
  )
}


// Type: SShort
private[buffer] final class ArrayFloat1SShort(
  override val array: Array[Short]
) extends ArrayFloat1[SShort](ShortBuffer.wrap(array)) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = short(v)
}

private[buffer] final class BufferFloat1SShort(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[SShort](byteBuffer.asShortBuffer()) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, short(v))
}

private[buffer] final class ViewFloat1SShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[SShort](byteBuffer.asShortBuffer()) {
  val backingSeq: BufferFloat1[SShort] = new BufferFloat1SShort(byteBuffer)

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, short(v))
}


// Type: UShort
private[buffer] final class ArrayFloat1UShort(
  override val array: Array[Char]
) extends ArrayFloat1[UShort](CharBuffer.wrap(array)) {
  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferFloat1UShort(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[UShort](byteBuffer.asCharBuffer()) {
  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, v.asInstanceOf[Char])
}

private[buffer] final class ViewFloat1UShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[UShort](byteBuffer.asCharBuffer()) {
  val backingSeq: BufferFloat1[UShort] = new BufferFloat1UShort(byteBuffer)

  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, v.asInstanceOf[Char])
}


// Type: NSShort
private[buffer] final class ArrayFloat1NSShort(
  override val array: Array[Short]
) extends ArrayFloat1[NSShort](ShortBuffer.wrap(array)) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = array(i)
    if (v < -32767) -1 else v*3.051851E-5f
  }
  def update(i: Int, v: Float) = array(i) = short(clamp(v, -1, 1)*32767)
}

private[buffer] final class BufferFloat1NSShort(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NSShort](byteBuffer.asShortBuffer()) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -32767) -1 else v*3.051851E-5f
  }
  def update(i: Int, v: Float) = buffer.put(
    i,
    short(clamp(v, -1, 1)*32767)
  )
}

private[buffer] final class ViewFloat1NSShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NSShort](byteBuffer.asShortBuffer()) {
  val backingSeq: BufferFloat1[NSShort] = new BufferFloat1NSShort(byteBuffer)

  def componentBinding = Binding.SShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*step)
    if (v < -32767) -1 else v*3.051851E-5f
  }
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    short(clamp(v, -1, 1)*32767)
  )
}


// Type: NUShort
private[buffer] final class ArrayFloat1NUShort(
  override val array: Array[Char]
) extends ArrayFloat1[NUShort](CharBuffer.wrap(array)) {
  def componentBinding = Binding.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = array(i)*1.5259022E-5f
  def update(i: Int, v: Float) {
    array(i) = (clamp(v, 0, 1)*65535).asInstanceOf[Char]
  }
}

private[buffer] final class BufferFloat1NUShort(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NUShort](byteBuffer.asCharBuffer()) {
  def componentBinding = Binding.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = buffer.get(i)*1.5259022E-5f
  def update(i: Int, v: Float) = buffer.put(
    i,
    (clamp(v, 0, 1)*65535).asInstanceOf[Char]
  )
}

private[buffer] final class ViewFloat1NUShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NUShort](byteBuffer.asCharBuffer()) {
  val backingSeq: BufferFloat1[NUShort] = new BufferFloat1NUShort(byteBuffer)

  def componentBinding = Binding.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = buffer.get(offset + i*step)*1.5259022E-5f
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    (clamp(v, 0, 1)*65535).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] final class ArrayFloat1SInt(
  override val array: Array[Int]
) extends ArrayFloat1[SInt](IntBuffer.wrap(array)) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = int(v)
}

private[buffer] final class BufferFloat1SInt(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[SInt](byteBuffer.asIntBuffer()) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, int(v))
}

private[buffer] final class ViewFloat1SInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[SInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferFloat1[SInt] = new BufferFloat1SInt(byteBuffer)

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, int(v))
}


// Type: UInt
private[buffer] final class ArrayFloat1UInt(
  override val array: Array[Int]
) extends ArrayFloat1[UInt](IntBuffer.wrap(array)) {
  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(array(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) = array(i) = int(long(v) & 0xFFFFFFFFL)
}

private[buffer] final class BufferFloat1UInt(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[UInt](byteBuffer.asIntBuffer()) {
  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buffer.get(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) = buffer.put(
    i,
    int(long(v) & 0xFFFFFFFFL)
  )
}

private[buffer] final class ViewFloat1UInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[UInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferFloat1[UInt] = new BufferFloat1UInt(byteBuffer)

  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buffer.get(offset + i*step)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    int(long(v) & 0xFFFFFFFFL)
  )
}


// Type: NSInt
private[buffer] final class ArrayFloat1NSInt(
  override val array: Array[Int]
) extends ArrayFloat1[NSInt](IntBuffer.wrap(array)) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = array(i)
    if (v < -2147483647) -1 else v*4.656613E-10f
  }
  def update(i: Int, v: Float) = array(i) = int(clamp(v, -1, 1)*2147483647)
}

private[buffer] final class BufferFloat1NSInt(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NSInt](byteBuffer.asIntBuffer()) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(i)
    if (v < -2147483647) -1 else v*4.656613E-10f
  }
  def update(i: Int, v: Float) = buffer.put(
    i,
    int(clamp(v, -1, 1)*2147483647)
  )
}

private[buffer] final class ViewFloat1NSInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NSInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferFloat1[NSInt] = new BufferFloat1NSInt(byteBuffer)

  def componentBinding = Binding.SInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    val v = buffer.get(offset + i*step)
    if (v < -2147483647) -1 else v*4.656613E-10f
  }
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    int(clamp(v, -1, 1)*2147483647)
  )
}


// Type: NUInt
private[buffer] final class ArrayFloat1NUInt(
  override val array: Array[Int]
) extends ArrayFloat1[NUInt](IntBuffer.wrap(array)) {
  def componentBinding = Binding.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = (long(array(i)) & 0xFFFFFFFFL)*2.3283064E-10f
  def update(i: Int, v: Float) = array(i) = int(clamp(v, 0, 1)*4294967295f)
}

private[buffer] final class BufferFloat1NUInt(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[NUInt](byteBuffer.asIntBuffer()) {
  def componentBinding = Binding.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    (long(buffer.get(i)) & 0xFFFFFFFFL)*2.3283064E-10f
  }
  def update(i: Int, v: Float) = buffer.put(
    i,
    int(clamp(v, 0, 1)*4294967295f)
  )
}

private[buffer] final class ViewFloat1NUInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[NUInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferFloat1[NUInt] = new BufferFloat1NUInt(byteBuffer)

  def componentBinding = Binding.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = {
    (long(buffer.get(offset + i*step)) & 0xFFFFFFFFL)*2.3283064E-10f
  }
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    int(clamp(v, 0, 1)*4294967295f)
  )
}


// Type: RawFloat
private[buffer] final class ArrayFloat1RawFloat(
  override val array: Array[Float]
) extends ArrayFloat1[RawFloat](FloatBuffer.wrap(array)) {
  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  override val byteBuffer: ByteBuffer
) extends BufferFloat1[RawFloat](byteBuffer.asFloatBuffer()) {
  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewFloat1[RawFloat](byteBuffer.asFloatBuffer()) {
  val backingSeq: BufferFloat1[RawFloat] = new BufferFloat1RawFloat(byteBuffer)

  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, v)
}
