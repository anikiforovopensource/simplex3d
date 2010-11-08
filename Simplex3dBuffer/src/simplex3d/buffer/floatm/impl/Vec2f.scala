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

package simplex3d.buffer.floatm
package impl

import java.nio._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2f RawFloat
private[buffer] final class ArrayVec2fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec2f[RawFloat](backing, 0, 2) with DataArray[Vec2f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2fRawFloat(backing.mkReadOnlyInstance())

  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec2f[RawFloat](backing, 0, 2) with DataBuffer[Vec2f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2fRawFloat(backing.mkReadOnlyInstance())

  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2fRawFloat(
  backing: BufferFloat1RawFloat, off: Int, str: Int
) extends BaseVec2f[RawFloat](backing, off, str) with DataView[Vec2f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2fRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
