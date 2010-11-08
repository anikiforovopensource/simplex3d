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
// Vec4f RawFloat
private[buffer] final class ArrayVec4fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec4f[RawFloat](backing, 0, 4) with DataArray[Vec4f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec4f[RawFloat](backing, 0, 4) with DataBuffer[Vec4f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4fRawFloat(
  backing: BufferFloat1RawFloat, off: Int, str: Int
) extends BaseVec4f[RawFloat](backing, off, str) with DataView[Vec4f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4fRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4f UByte
private[buffer] final class ArrayVec4fUByte(
  backing: ArrayFloat1UByte
) extends BaseVec4f[UByte](backing, 0, 4) with DataArray[Vec4f, UByte] {
  def this() = this(new ArrayFloat1UByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4fUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4fUByte(
  backing: BufferFloat1UByte
) extends BaseVec4f[UByte](backing, 0, 4) with DataBuffer[Vec4f, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4fUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4fUByte(
  backing: BufferFloat1UByte, off: Int, str: Int
) extends BaseVec4f[UByte](backing, off, str) with DataView[Vec4f, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4fUByte(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
