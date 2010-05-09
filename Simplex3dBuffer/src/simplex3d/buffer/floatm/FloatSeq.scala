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


// Type: SByte
private[buffer] sealed abstract class SeqFloat1SByte(
  buff: ByteBuffer
) extends BaseFloat1[SByte](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1SByte(new Array[Byte](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1SByte(BufferUtil.allocateByteBuffer(size))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1SByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1SByte(
  override val array: Array[Byte]
) extends SeqFloat1SByte(ByteBuffer.wrap(array)) with DataArray[Float1, SByte] {
  def backingSeq = this

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = byte(v)
}

private[buffer] final class BufferFloat1SByte(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1SByte(byteBuffer) with DataBuffer[Float1, SByte] {
  def backingSeq = this

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, byte(v))
}

private[buffer] final class ViewFloat1SByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1SByte(byteBuffer) with DataView[Float1, SByte] {
  val backingSeq = new BufferFloat1SByte(byteBuffer)

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, byte(v))
}


// Type: UByte
private[buffer] sealed abstract class SeqFloat1UByte(
  buff: ByteBuffer
) extends BaseFloat1[UByte](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1UByte(new Array[Byte](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1UByte(BufferUtil.allocateByteBuffer(size))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1UByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1UByte(
  override val array: Array[Byte]
) extends SeqFloat1UByte(ByteBuffer.wrap(array)) with DataArray[Float1, UByte] {
  def backingSeq = this

  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i) & 0xFF
  def update(i: Int, v: Float) = array(i) = byte(v)
}

private[buffer] final class BufferFloat1UByte(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1UByte(byteBuffer) with DataBuffer[Float1, UByte] {
  def backingSeq = this

  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i) & 0xFF
  def update(i: Int, v: Float) = buffer.put(i, byte(v))
}

private[buffer] final class ViewFloat1UByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1UByte(byteBuffer) with DataView[Float1, UByte] {
  val backingSeq = new BufferFloat1UByte(byteBuffer)

  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step) & 0xFF
  def update(i: Int, v: Float) = buffer.put(offset + i*step, byte(v))
}


// Type: NSByte
private[buffer] sealed abstract class SeqFloat1NSByte(
  buff: ByteBuffer
) extends BaseFloat1[NSByte](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NSByte(new Array[Byte](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NSByte(BufferUtil.allocateByteBuffer(size))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NSByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NSByte(
  override val array: Array[Byte]
) extends SeqFloat1NSByte(
  ByteBuffer.wrap(array)
) with DataArray[Float1, NSByte] {
  def backingSeq = this

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
) extends SeqFloat1NSByte(
  byteBuffer
) with DataBuffer[Float1, NSByte] {
  def backingSeq = this

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
) extends SeqFloat1NSByte(
  byteBuffer
) with DataView[Float1, NSByte] {
  val backingSeq = new BufferFloat1NSByte(byteBuffer)

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
private[buffer] sealed abstract class SeqFloat1NUByte(
  buff: ByteBuffer
) extends BaseFloat1[NUByte](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NUByte(new Array[Byte](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NUByte(BufferUtil.allocateByteBuffer(size))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NUByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NUByte(
  override val array: Array[Byte]
) extends SeqFloat1NUByte(
  ByteBuffer.wrap(array)
) with DataArray[Float1, NUByte] {
  def backingSeq = this

  def componentBinding = Binding.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = (array(i) & 0xFF)*0.003921569f
  def update(i: Int, v: Float) = array(i) = byte(clamp(v, 0, 1)*255)
}

private[buffer] final class BufferFloat1NUByte(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1NUByte(
  byteBuffer
) with DataBuffer[Float1, NUByte] {
  def backingSeq = this

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
) extends SeqFloat1NUByte(
  byteBuffer
) with DataView[Float1, NUByte] {
  val backingSeq = new BufferFloat1NUByte(byteBuffer)

  def componentBinding = Binding.UByte
  def normalized: Boolean = true

  def apply(i: Int) :Float = (buffer.get(offset + i*step) & 0xFF)*0.003921569f
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    byte(clamp(v, 0, 1)*255)
  )
}


// Type: SShort
private[buffer] sealed abstract class SeqFloat1SShort(
  buff: ShortBuffer
) extends BaseFloat1[SShort](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1SShort(new Array[Short](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1SShort(BufferUtil.allocateByteBuffer(size*2))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1SShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1SShort(
  override val array: Array[Short]
) extends SeqFloat1SShort(
  ShortBuffer.wrap(array)
) with DataArray[Float1, SShort] {
  def backingSeq = this

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = short(v)
}

private[buffer] final class BufferFloat1SShort(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1SShort(
  byteBuffer.asShortBuffer()
) with DataBuffer[Float1, SShort] {
  def backingSeq = this

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, short(v))
}

private[buffer] final class ViewFloat1SShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1SShort(
  byteBuffer.asShortBuffer()
) with DataView[Float1, SShort] {
  val backingSeq = new BufferFloat1SShort(byteBuffer)

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, short(v))
}


// Type: UShort
private[buffer] sealed abstract class SeqFloat1UShort(
  buff: CharBuffer
) extends BaseFloat1[UShort](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1UShort(new Array[Char](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1UShort(BufferUtil.allocateByteBuffer(size*2))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1UShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1UShort(
  override val array: Array[Char]
) extends SeqFloat1UShort(
  CharBuffer.wrap(array)
) with DataArray[Float1, UShort] {
  def backingSeq = this

  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferFloat1UShort(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1UShort(
  byteBuffer.asCharBuffer()
) with DataBuffer[Float1, UShort] {
  def backingSeq = this

  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, v.asInstanceOf[Char])
}

private[buffer] final class ViewFloat1UShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1UShort(
  byteBuffer.asCharBuffer()
) with DataView[Float1, UShort] {
  val backingSeq = new BufferFloat1UShort(byteBuffer)

  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = {
    buffer.put(offset + i*step, v.asInstanceOf[Char])
  }
}


// Type: NSShort
private[buffer] sealed abstract class SeqFloat1NSShort(
  buff: ShortBuffer
) extends BaseFloat1[NSShort](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NSShort(new Array[Short](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NSShort(BufferUtil.allocateByteBuffer(size*2))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NSShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NSShort(
  override val array: Array[Short]
) extends SeqFloat1NSShort(
  ShortBuffer.wrap(array)
) with DataArray[Float1, NSShort] {
  def backingSeq = this

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
) extends SeqFloat1NSShort(
  byteBuffer.asShortBuffer()
) with DataBuffer[Float1, NSShort] {
  def backingSeq = this

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
) extends SeqFloat1NSShort(
  byteBuffer.asShortBuffer()
) with DataView[Float1, NSShort] {
  val backingSeq = new BufferFloat1NSShort(byteBuffer)

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
private[buffer] sealed abstract class SeqFloat1NUShort(
  buff: CharBuffer
) extends BaseFloat1[NUShort](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NUShort(new Array[Char](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NUShort(BufferUtil.allocateByteBuffer(size*2))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NUShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NUShort(
  override val array: Array[Char]
) extends SeqFloat1NUShort(
  CharBuffer.wrap(array)
) with DataArray[Float1, NUShort] {
  def backingSeq = this

  def componentBinding = Binding.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = array(i)*1.5259022E-5f
  def update(i: Int, v: Float) {
    array(i) = (clamp(v, 0, 1)*65535).asInstanceOf[Char]
  }
}

private[buffer] final class BufferFloat1NUShort(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1NUShort(
  byteBuffer.asCharBuffer()
) with DataBuffer[Float1, NUShort] {
  def backingSeq = this

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
) extends SeqFloat1NUShort(
  byteBuffer.asCharBuffer()
) with DataView[Float1, NUShort] {
  val backingSeq = new BufferFloat1NUShort(byteBuffer)

  def componentBinding = Binding.UShort
  def normalized: Boolean = true

  def apply(i: Int) :Float = buffer.get(offset + i*step)*1.5259022E-5f
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    (clamp(v, 0, 1)*65535).asInstanceOf[Char]
  )
}


// Type: SInt
private[buffer] sealed abstract class SeqFloat1SInt(
  buff: IntBuffer
) extends BaseFloat1[SInt](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1SInt(new Array[Int](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1SInt(BufferUtil.allocateByteBuffer(size*4))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1SInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1SInt(
  override val array: Array[Int]
) extends SeqFloat1SInt(IntBuffer.wrap(array)) with DataArray[Float1, SInt] {
  def backingSeq = this

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = int(v)
}

private[buffer] final class BufferFloat1SInt(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1SInt(byteBuffer.asIntBuffer()) with DataBuffer[Float1, SInt]{
  def backingSeq = this

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, int(v))
}

private[buffer] final class ViewFloat1SInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1SInt(byteBuffer.asIntBuffer()) with DataView[Float1, SInt] {
  val backingSeq = new BufferFloat1SInt(byteBuffer)

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, int(v))
}


// Type: UInt
private[buffer] sealed abstract class SeqFloat1UInt(
  buff: IntBuffer
) extends BaseFloat1[UInt](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1UInt(new Array[Int](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1UInt(BufferUtil.allocateByteBuffer(size*4))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1UInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1UInt(
  override val array: Array[Int]
) extends SeqFloat1UInt(IntBuffer.wrap(array)) with DataArray[Float1, UInt] {
  def backingSeq = this

  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(array(i)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) = array(i) = int(long(v) & 0xFFFFFFFFL)
}

private[buffer] final class BufferFloat1UInt(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1UInt(byteBuffer.asIntBuffer()) with DataBuffer[Float1, UInt]{
  def backingSeq = this

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
) extends SeqFloat1UInt(byteBuffer.asIntBuffer()) with DataView[Float1, UInt] {
  val backingSeq = new BufferFloat1UInt(byteBuffer)

  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Float = long(buffer.get(offset + i*step)) & 0xFFFFFFFFL
  def update(i: Int, v: Float) = buffer.put(
    offset + i*step,
    int(long(v) & 0xFFFFFFFFL)
  )
}


// Type: NSInt
private[buffer] sealed abstract class SeqFloat1NSInt(
  buff: IntBuffer
) extends BaseFloat1[NSInt](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NSInt(new Array[Int](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NSInt(BufferUtil.allocateByteBuffer(size*4))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NSInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NSInt(
  override val array: Array[Int]
) extends SeqFloat1NSInt(
  IntBuffer.wrap(array)
) with DataArray[Float1, NSInt] {
  def backingSeq = this

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
) extends SeqFloat1NSInt(
  byteBuffer.asIntBuffer()
) with DataBuffer[Float1, NSInt] {
  def backingSeq = this

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
) extends SeqFloat1NSInt(
  byteBuffer.asIntBuffer()
) with DataView[Float1, NSInt] {
  val backingSeq = new BufferFloat1NSInt(byteBuffer)

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
private[buffer] sealed abstract class SeqFloat1NUInt(
  buff: IntBuffer
) extends BaseFloat1[NUInt](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1NUInt(new Array[Int](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1NUInt(BufferUtil.allocateByteBuffer(size*4))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1NUInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1NUInt(
  override val array: Array[Int]
) extends SeqFloat1NUInt(
  IntBuffer.wrap(array)
) with DataArray[Float1, NUInt] {
  def backingSeq = this

  def componentBinding = Binding.UInt
  def normalized: Boolean = true

  def apply(i: Int) :Float = (long(array(i)) & 0xFFFFFFFFL)*2.3283064E-10f
  def update(i: Int, v: Float) = array(i) = int(clamp(v, 0, 1)*4294967295f)
}

private[buffer] final class BufferFloat1NUInt(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1NUInt(
  byteBuffer.asIntBuffer()
) with DataBuffer[Float1, NUInt] {
  def backingSeq = this

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
) extends SeqFloat1NUInt(
  byteBuffer.asIntBuffer()
) with DataView[Float1, NUInt] {
  val backingSeq = new BufferFloat1NUInt(byteBuffer)

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
private[buffer] sealed abstract class SeqFloat1RawFloat(
  buff: FloatBuffer
) extends BaseFloat1[RawFloat](buff) {
  override def makeArray(size: Int) =
    new ArrayFloat1RawFloat(new Array[Float](size))
  override def makeBuffer(size: Int) =
    new BufferFloat1RawFloat(BufferUtil.allocateByteBuffer(size*4))
  override def makeView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewFloat1RawFloat(byteBuffer, offset, stride)
}

private[buffer] final class ArrayFloat1RawFloat(
  override val array: Array[Float]
) extends SeqFloat1RawFloat(
  FloatBuffer.wrap(array)
) with DataArray[Float1, RawFloat] {
  def backingSeq = this

  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = array(i)
  def update(i: Int, v: Float) = array(i) = v
}

private[buffer] final class BufferFloat1RawFloat(
  override val byteBuffer: ByteBuffer
) extends SeqFloat1RawFloat(
  byteBuffer.asFloatBuffer()
) with DataBuffer[Float1, RawFloat] {
  def backingSeq = this

  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = buffer.get(i)
  def update(i: Int, v: Float) = buffer.put(i, v)
}

private[buffer] final class ViewFloat1RawFloat(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqFloat1RawFloat(
  byteBuffer.asFloatBuffer()
) with DataView[Float1, RawFloat] {
  val backingSeq = new BufferFloat1RawFloat(byteBuffer)

  def normalized: Boolean = false
  def componentBinding: Int = Binding.RawFloat

  def apply(i: Int) :Float = buffer.get(offset + i*step)
  def update(i: Int, v: Float) = buffer.put(offset + i*step, v)
}
