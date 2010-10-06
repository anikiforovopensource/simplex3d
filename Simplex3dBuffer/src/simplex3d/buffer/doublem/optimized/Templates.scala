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
) extends BaseVec2d[RawFloat](backing, 0, 2, null) with DataArray[Vec2d, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec2dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec2d[RawFloat](backing, 0, 2, null) with DataBuffer[Vec2d, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec2dRawFloat(
  backing: BufferDouble1RawFloat, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec2d[RawFloat](backing, off, str, sz) with DataView[Vec2d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride, size
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}


// Vec3d RawFloat
private[buffer] final class ArrayVec3dRawFloat(
  backing: ArrayDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3, null) with DataArray[Vec3d, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec3dRawFloat(
  backing: BufferDouble1RawFloat
) extends BaseVec3d[RawFloat](backing, 0, 3, null) with DataBuffer[Vec3d, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec3dRawFloat(
  backing: BufferDouble1RawFloat, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec3d[RawFloat](backing, off, str, sz) with DataView[Vec3d, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3dRawFloat(
    backing.mkReadOnlyInstance(), offset, stride, size
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3dRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}


// Vec4d UByte
private[buffer] final class ArrayVec4dUByte(
  backing: ArrayDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4, null) with DataArray[Vec4d, UByte] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec4dUByte(
  backing: BufferDouble1UByte
) extends BaseVec4d[UByte](backing, 0, 4, null) with DataBuffer[Vec4d, UByte] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec4dUByte(
  backing: BufferDouble1UByte, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec4d[UByte](backing, off, str, sz) with DataView[Vec4d, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4dUByte(
    backing.mkReadOnlyInstance(), offset, stride, size
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4dUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}
