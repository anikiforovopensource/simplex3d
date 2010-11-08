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
// Vec4d RawFloat
private[buffer] final class ArrayVec4dRawFloat(
  backing: ArrayDouble1RawFloat
) extends BaseVec4d[RawFloat](backing, 0, 4) with DataArray[Vec4d, RawFloat] {
  def this() = this(new ArrayDouble1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec4d[RawFloat](backing, 0, 4) with DataBuffer[Vec4d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4dRawFloat(
  backing: BufferDouble1RawFloat, off: Int, str: Int
) extends BaseVec4d[RawFloat](backing, off, str) with DataView[Vec4d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec4dRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4d UByte
private[buffer] final class ArrayVec4dUByte(
  backing: ArrayDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4) with DataArray[Vec4d, UByte] {
  def this() = this(new ArrayDouble1UByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4dUByte(
  backing: BufferDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4) with DataBuffer[Vec4d, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4dUByte(
  backing: BufferDouble1UByte, off: Int, str: Int
) extends BaseVec4d[UByte](backing, off, str) with DataView[Vec4d, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4dUByte(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
