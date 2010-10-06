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
package optimized

import java.nio._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2f RawFloat
private[buffer] final class ArrayVec2fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec2f[RawFloat](backing, 0, 2, null) with DataArray[Vec2f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec2fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec2f[RawFloat](backing, 0, 2, null) with DataBuffer[Vec2f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2fRawFloat(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec2fRawFloat(
  backing: BufferFloat1RawFloat, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec2f[RawFloat](backing, off, str, sz) with DataView[Vec2f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2fRawFloat(
    backing.mkReadOnlyInstance(), offset, stride, size
  )

  override def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backing.mkDataArray(array))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer))
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec2fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}


// Vec3f RawFloat
private[buffer] final class ArrayVec3fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec3f[RawFloat](backing, 0, 3, null) with DataArray[Vec3f, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec3fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec3f[RawFloat](backing, 0, 3, null) with DataBuffer[Vec3f, RawFloat] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec3fRawFloat(
  backing: BufferFloat1RawFloat, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec3f[RawFloat](backing, off, str, sz) with DataView[Vec3f, RawFloat] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3fRawFloat(
    backing.mkReadOnlyInstance(), offset, stride, size
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec3fRawFloat(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}


// Vec4f UByte
private[buffer] final class ArrayVec4fUByte(
  backing: ArrayFloat1UByte
) extends BaseVec4f[UByte](backing, 0, 4, null) with DataArray[Vec4f, UByte] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class BufferVec4fUByte(
  backing: BufferFloat1UByte
) extends BaseVec4f[UByte](backing, 0, 4, null) with DataBuffer[Vec4f, UByte] {
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}

private[buffer] final class ViewVec4fUByte(
  backing: BufferFloat1UByte, off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec4f[UByte](backing, off, str, sz) with DataView[Vec4f, UByte] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4fUByte(
    backing.mkReadOnlyInstance(), offset, stride, size
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
  override protected def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int, size: java.lang.Integer) =
    new ViewVec4fUByte(backing.mkReadDataBuffer(byteBuffer), offset, stride, size)
}
