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
// Vec4i SInt
private[buffer] final class ArrayVec4iSInt(
  backing: ArrayInt1SInt
) extends BaseVec4i[SInt](backing, 0, 4) with DataArray[Vec4i, SInt] {
  def this() = this(new ArrayInt1SInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4iSInt(
  backing: BufferInt1SInt
) extends BaseVec4i[SInt](backing, 0, 4) with DataBuffer[Vec4i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4iSInt(
  backing: BufferInt1SInt, off: Int, str: Int
) extends BaseVec4i[SInt](backing, off, str) with DataView[Vec4i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4iSInt(backing.mkReadOnlyInstance(), offset, stride)

  override def apply(i: Int) :ConstVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
