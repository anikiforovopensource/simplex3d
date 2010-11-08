/*
 * Simplex3d, IntBuffer module
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

package simplex3d.buffer.intm
package impl

import java.nio._
import simplex3d.math.intm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2i SInt
private[buffer] final class ArrayVec2iSInt(
  backing: ArrayInt1SInt
) extends BaseVec2i[SInt](backing, 0, 2) with DataArray[Vec2i, SInt] {
  def this() = this(new ArrayInt1SInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2iSInt(
  backing: BufferInt1SInt
) extends BaseVec2i[SInt](backing, 0, 2) with DataBuffer[Vec2i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2iSInt(
  backing: BufferInt1SInt, off: Int, str: Int
) extends BaseVec2i[SInt](backing, off, str) with DataView[Vec2i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2iSInt(backing.mkReadOnlyInstance(), offset, stride)

  override def apply(i: Int) :ConstVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
