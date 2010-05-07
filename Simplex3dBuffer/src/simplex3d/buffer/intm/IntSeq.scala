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
private[buffer] final class ViewInt1UByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[UByte](byteBuffer) {
  val backingSeq: BufferInt1[UByte] = new BufferInt1UByte(byteBuffer)

  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step) & 0xFF
  def update(i: Int, v: Int) = buffer.put(offset + i*step, byte(v))
}

private[buffer] final class ViewInt1UShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[UShort](byteBuffer.asCharBuffer()) {
  val backingSeq: BufferInt1[UShort] = new BufferInt1UShort(byteBuffer)

  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, v.asInstanceOf[Char])
}

private[buffer] final class ViewInt1UInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[UInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferInt1[UInt] = new BufferInt1UInt(byteBuffer)

  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, v)
}


// Signed
// Type: SByte
private[buffer] final class ArrayInt1SByte(
  override val array: Array[Byte]
) extends ArrayInt1[SByte](ByteBuffer.wrap(array)) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = byte(v)
}

private[buffer] final class BufferInt1SByte(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[SByte](byteBuffer) {
  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, byte(v))
}

private[buffer] final class ViewInt1SByte(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[SByte](byteBuffer) {
  val backingSeq: BufferInt1[SByte] = new BufferInt1SByte(byteBuffer)

  def componentBinding = Binding.SByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, byte(v))
}


// Type: SShort
private[buffer] final class ArrayInt1SShort(
  override val array: Array[Short]
) extends ArrayInt1[SShort](ShortBuffer.wrap(array)) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = short(v)
}

private[buffer] final class BufferInt1SShort(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[SShort](byteBuffer.asShortBuffer()) {
  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, short(v))
}

private[buffer] final class ViewInt1SShort(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[SShort](byteBuffer.asShortBuffer()) {
  val backingSeq: BufferInt1[SShort] = new BufferInt1SShort(byteBuffer)

  def componentBinding = Binding.SShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, short(v))
}


// Type: SInt
private[buffer] final class ArrayInt1SInt(
  override val array: Array[Int]
) extends ArrayInt1[SInt](IntBuffer.wrap(array)) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v
}

private[buffer] final class BufferInt1SInt(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[SInt](byteBuffer.asIntBuffer()) {
  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v)
}

private[buffer] final class ViewInt1SInt(
  override val byteBuffer: ByteBuffer,
  val offset: Int,
  val stride: Int
) extends ViewInt1[SInt](byteBuffer.asIntBuffer()) {
  val backingSeq: BufferInt1[SInt] = new BufferInt1SInt(byteBuffer)

  def componentBinding = Binding.SInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(offset + i*step)
  def update(i: Int, v: Int) = buffer.put(offset + i*step, v)
}
