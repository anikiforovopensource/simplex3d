/*
 * Simplex3d, IntBuffer module
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

package simplex3d.buffer.intm

import java.nio._
import simplex3d.math._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */

// Signed
// Type: SByte
private[buffer] sealed abstract class SeqInt1SByte(
  buff: ByteBuffer
) extends BaseInt1[SByte](buff) {
  final def mkDataArray(size: Int) =
    new ArrayInt1SByte(new Array[Byte](size))
  final def mkDataArray(array: Array[Byte]) =
    new ArrayInt1SByte(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SByte(BufferUtil.allocateByteBuffer(size))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SByte(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SByte(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SByte(
  override val array: Array[Byte]
) extends SeqInt1SByte(ByteBuffer.wrap(array)) with DataArray[Int1, SByte] {
  def backingSeq = this

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = byte(v)
}

private[buffer] final class BufferInt1SByte(
  override val byteBuffer: ByteBuffer
) extends SeqInt1SByte(byteBuffer) with DataBuffer[Int1, SByte] {
  def backingSeq = this

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1SByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqInt1SByte(byteBuffer) with DataView[Int1, SByte] {
  val backingSeq = new BufferInt1SByte(byteBuffer)

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, byte(v))
}


// Type: SShort
private[buffer] sealed abstract class SeqInt1SShort(
  buff: ShortBuffer
) extends BaseInt1[SShort](buff) {
  final def mkDataArray(size: Int) =
    new ArrayInt1SShort(new Array[Short](size))
  final def mkDataArray(array: Array[Short]) =
    new ArrayInt1SShort(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SShort(BufferUtil.allocateByteBuffer(size*2))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SShort(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SShort(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SShort(
  override val array: Array[Short]
) extends SeqInt1SShort(
  ShortBuffer.wrap(array)
) with DataArray[Int1, SShort] {
  def backingSeq = this

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = short(v)
}

private[buffer] final class BufferInt1SShort(
  override val byteBuffer: ByteBuffer
) extends SeqInt1SShort(
  byteBuffer.asShortBuffer()
) with DataBuffer[Int1, SShort] {
  def backingSeq = this

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, short(v))
}

private[buffer] final class ViewInt1SShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqInt1SShort(
  byteBuffer.asShortBuffer()
) with DataView[Int1, SShort] {
  val backingSeq = new BufferInt1SShort(byteBuffer)

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, short(v))
}


// Type: SInt
private[buffer] sealed abstract class SeqInt1SInt(
  buff: IntBuffer
) extends BaseInt1[SInt](buff) {
  final def mkDataArray(size: Int) =
    new ArrayInt1SInt(new Array[Int](size))
  final def mkDataArray(array: Array[Int]) =
    new ArrayInt1SInt(array)
  final def mkDataBuffer(size: Int) =
    new BufferInt1SInt(BufferUtil.allocateByteBuffer(size*4))
  final def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferInt1SInt(byteBuffer)
  final def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewInt1SInt(byteBuffer, offset, stride)
}

private[buffer] final class ArrayInt1SInt(
  override val array: Array[Int]
) extends SeqInt1SInt(IntBuffer.wrap(array)) with DataArray[Int1, SInt] {
  def backingSeq = this

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v
}

private[buffer] final class BufferInt1SInt(
  override val byteBuffer: ByteBuffer
) extends SeqInt1SInt(byteBuffer.asIntBuffer()) with DataBuffer[Int1, SInt]{
  def backingSeq = this

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends SeqInt1SInt(byteBuffer.asIntBuffer()) with DataView[Int1, SInt] {
  val backingSeq = new BufferInt1SInt(byteBuffer)

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, v)
}
