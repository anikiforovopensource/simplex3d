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
package optimized

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
  override def backingSeq: ArrayDouble1RawFloat = backing
  def this() = this(new ArrayDouble1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec2d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec2d[RawFloat](backing, 0, 2) with DataBuffer[Vec2d, RawFloat] {
  override def backingSeq: BufferDouble1RawFloat = backing
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec2d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2dRawFloat(
  backing: BufferDouble1RawFloat,
  offset: Int,
  stride: Int
) extends BaseVec2d[RawFloat](backing, offset, stride) with DataView[Vec2d, RawFloat] {
  override def backingSeq: BufferDouble1RawFloat = backing
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ReadVec2d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3d RawFloat
private[buffer] final class ArrayVec3dRawFloat(
  backing: ArrayDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3) with DataArray[Vec3d, RawFloat] {
  def this() = this(new ArrayDouble1RawFloat)
  override def backingSeq: ArrayDouble1RawFloat = backing
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec3d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3) with DataBuffer[Vec3d, RawFloat] {
  override def backingSeq: BufferDouble1RawFloat = backing
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3dRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec3d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3dRawFloat(
  backing: BufferDouble1RawFloat,
  offset: Int,
  stride: Int
) extends BaseVec3d[RawFloat](backing, offset, stride) with DataView[Vec3d, RawFloat] {
  override def backingSeq: BufferDouble1RawFloat = backing
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride
  )
  
  override def apply(i: Int) :ReadVec3d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3dRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3dRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4d UByte
private[buffer] final class ArrayVec4dUByte(
  backing: ArrayDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4) with DataArray[Vec4d, UByte] {
  override def backingSeq: ArrayDouble1UByte = backing
  def this() = this(new ArrayDouble1UByte)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec4d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4dUByte(
  backing: BufferDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4) with DataBuffer[Vec4d, UByte] {
  override def backingSeq: BufferDouble1UByte = backing
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4dUByte(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec4d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4dUByte(
  backing: BufferDouble1UByte,
  offset: Int,
  stride: Int
) extends BaseVec4d[UByte](backing, offset, stride) with DataView[Vec4d, UByte] {
  override def backingSeq: BufferDouble1UByte = backing
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4dUByte(
    backing.mkReadOnlyInstance(), offset, stride
  )

  override def apply(i: Int) :ReadVec4d = {
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

  override def mkReadDataArray(size: Int) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4dUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4dUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}
