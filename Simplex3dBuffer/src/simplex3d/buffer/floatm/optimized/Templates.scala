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
) extends BaseVec2f[RawFloat](backing) with DataArray[Vec2f, RawFloat] {
  override def backingSeq: ArrayFloat1RawFloat = backing
  def this() = this(new ArrayFloat1RawFloat)
  def asReadOnlySeq() = new ArrayVec2fRawFloat(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec2f[RawFloat](backing) with DataBuffer[Vec2f, RawFloat] {
  override def backingSeq: BufferFloat1RawFloat = backing
  def asReadOnlySeq() = new BufferVec2fRawFloat(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2fRawFloat(
  backing: BufferFloat1RawFloat,
  override val offset: Int,
  override val stride: Int
) extends BaseVec2f[RawFloat](backing) with DataView[Vec2f, RawFloat] {
  override def backingSeq: BufferFloat1RawFloat = backing
  def asReadOnlySeq() = new ViewVec2fRawFloat(
    backing.asReadOnlySeq(), offset, stride
  )

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3f RawFloat
private[buffer] final class ArrayVec3fRawFloat(
  backing: ArrayFloat1RawFloat
) extends BaseVec3f[RawFloat](backing) with DataArray[Vec3f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  override def backingSeq: ArrayFloat1RawFloat = backing
  def asReadOnlySeq() = new ArrayVec3fRawFloat(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3fRawFloat(
  backing: BufferFloat1RawFloat
) extends BaseVec3f[RawFloat](backing) with DataBuffer[Vec3f, RawFloat] {
  override def backingSeq: BufferFloat1RawFloat = backing
  def asReadOnlySeq() = new BufferVec3fRawFloat(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3fRawFloat(
  backing: BufferFloat1RawFloat,
  override val offset: Int,
  override val stride: Int
) extends BaseVec3f[RawFloat](backing) with DataView[Vec3f, RawFloat] {
  override def backingSeq: BufferFloat1RawFloat = backing
  def asReadOnlySeq() = new ViewVec3fRawFloat(
    backing.asReadOnlySeq(), offset, stride
  )
  
  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4f UByte
private[buffer] final class ArrayVec4fUByte(
  backing: ArrayFloat1UByte
) extends BaseVec4f[UByte](backing) with DataArray[Vec4f, UByte] {
  override def backingSeq: ArrayFloat1UByte = backing
  def this() = this(new ArrayFloat1UByte)
  def asReadOnlySeq() = new ArrayVec4fUByte(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4fUByte(
  backing: BufferFloat1UByte
) extends BaseVec4f[UByte](backing) with DataBuffer[Vec4f, UByte] {
  override def backingSeq: BufferFloat1UByte = backing
  def asReadOnlySeq() = new BufferVec4fUByte(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4fUByte(
  backing: BufferFloat1UByte,
  override val offset: Int,
  override val stride: Int
) extends BaseVec4f[UByte](backing) with DataView[Vec4f, UByte] {
  override def backingSeq: BufferFloat1UByte = backing
  def asReadOnlySeq() = new ViewVec4fUByte(
    backing.asReadOnlySeq(), offset, stride
  )

  override def apply(i: Int) :AnyVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Byte]) =
    new ArrayVec4fUByte(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fUByte(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}
