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
package impl

import java.nio._
import simplex3d.math.doublem._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2d RawFloat
private[buffer] final class ArrayVec2dRawFloat(
  backing: ArrayDouble1RawFloat
) extends BaseVec2d[RawFloat](backing, 0, 2) with DataArray[Vec2d, RawFloat] {
  def this() = this(new ArrayDouble1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2d) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec2d[RawFloat](backing, 0, 2) with DataBuffer[Vec2d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2d) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2dRawFloat(
  backing: BufferDouble1RawFloat, off: Int, str: Int
) extends BaseVec2d[RawFloat](backing, off, str) with DataView[Vec2d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
