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
// Vec3f RawFloat
private[buffer] final class ArrayVec3fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec3f[RawFloat](backing, 0, 3) with DataArray[Vec3f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec3f[RawFloat](backing, 0, 3) with DataBuffer[Vec3f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3fRawFloat(
  backing: BufferFloat1RawFloat, off: Int, str: Int
) extends BaseVec3f[RawFloat](backing, off, str) with DataView[Vec3f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3fRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3f UByte
private[buffer] final class ArrayVec3fUByte(
  backing: ArrayFloat1UByte
) extends BaseVec3f[UByte](backing, 0, 3) with DataArray[Vec3f, UByte] {
  def this() = this(new ArrayFloat1UByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3fUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3fUByte(
  backing: BufferFloat1UByte
) extends BaseVec3f[UByte](backing, 0, 3) with DataBuffer[Vec3f, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3fUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3fUByte(
  backing: BufferFloat1UByte, off: Int, str: Int
) extends BaseVec3f[UByte](backing, off, str) with DataView[Vec3f, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3fUByte(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3fUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
