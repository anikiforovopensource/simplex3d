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
// Vec3d RawFloat
private[buffer] final class ArrayVec3dRawFloat(
  backing: ArrayDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3) with DataArray[Vec3d, RawFloat] {
  def this() = this(new ArrayDouble1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3) with DataBuffer[Vec3d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3dRawFloat(
  backing: BufferDouble1RawFloat, off: Int, str: Int
) extends BaseVec3d[RawFloat](backing, off, str) with DataView[Vec3d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3d UByte
private[buffer] final class ArrayVec3dUByte(
  backing: ArrayDouble1UByte
) extends BaseVec3d[UByte](backing, 0, 3) with DataArray[Vec3d, UByte] {
  def this() = this(new ArrayDouble1UByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3dUByte(
  backing: BufferDouble1UByte
) extends BaseVec3d[UByte](backing, 0, 3) with DataBuffer[Vec3d, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3dUByte(
  backing: BufferDouble1UByte, off: Int, str: Int
) extends BaseVec3d[UByte](backing, off, str) with DataView[Vec3d, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3dUByte(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec3dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
