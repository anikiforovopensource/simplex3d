/*
 * Simplex3d, BaseBuffer module
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

package simplex3d.buffer

import java.nio._
import simplex3d.math._


private[buffer] sealed abstract class BaseInt1[+D <: ReadInt](
  buff: D#BufferType
) extends BaseSeq[Int1, Int, D](buff) {
  final def components: Int = 1

  protected final def translatePut(
    destOffset: Int,
    src: ContiguousSeq[Int1, _],
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

private[buffer] abstract class ArrayInt1[+D <: ReadInt](
  buff: D#BufferType
) extends BaseInt1[D](buff) with DataArray[Int1, D] {
  final def backingSeq = this
}

private[buffer] abstract class BufferInt1[+D <: ReadInt](
  buff: D#BufferType
)extends BaseInt1[D](buff) with DataBuffer[Int1, D] {
  final def backingSeq = this
}

private[buffer] abstract class ViewInt1[+D <: ReadInt](
  buff: D#BufferType
) extends BaseInt1[D](buff) with DataView[Int1, D]


// Type: UByte
private[buffer] final class ArrayInt1UByte(
  override val array: Array[Byte]
) extends ArrayInt1[UByte](ByteBuffer.wrap(array)) with IndexArray[UByte] {
  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i) & 0xFF
  def update(i: Int, v: Int) = array(i) = byte(v)
}

private[buffer] final class BufferInt1UByte(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[UByte](byteBuffer) with IndexBuffer[UByte] {
  def componentBinding = Binding.UByte
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i) & 0xFF
  def update(i: Int, v: Int) = buffer.put(i, byte(v))
}


// Type: UShort
private[buffer] final class ArrayInt1UShort(
  override val array: Array[Char]
) extends ArrayInt1[UShort](CharBuffer.wrap(array)) with IndexArray[UShort] {
  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v.asInstanceOf[Char]
}

private[buffer] final class BufferInt1UShort(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[UShort](byteBuffer.asCharBuffer()) with IndexBuffer[UShort] {
  def componentBinding = Binding.UShort
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v.asInstanceOf[Char])
}


// Type: UInt
private[buffer] final class ArrayInt1UInt(
  override val array: Array[Int]
) extends ArrayInt1[UInt](IntBuffer.wrap(array)) with IndexArray[UInt] {
  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = array(i)
  def update(i: Int, v: Int) = array(i) = v
}

private[buffer] final class BufferInt1UInt(
  override val byteBuffer: ByteBuffer
) extends BufferInt1[UInt](byteBuffer.asIntBuffer()) with IndexBuffer[UInt] {
  def componentBinding = Binding.UInt
  def normalized: Boolean = false

  def apply(i: Int) :Int = buffer.get(i)
  def update(i: Int, v: Int) = buffer.put(i, v)
}
