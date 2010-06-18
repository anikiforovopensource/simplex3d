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
  override val backingSeq: ArrayFloat1RawFloat
) extends BaseVec2f[RawFloat](backingSeq) with DataArray[Vec2f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  def asReadOnlySeq() = new ArrayVec2fRawFloat(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2fRawFloat(
  override val backingSeq: BufferFloat1RawFloat
) extends BaseVec2f[RawFloat](backingSeq) with DataBuffer[Vec2f, RawFloat] {
  def asReadOnlySeq() = new BufferVec2fRawFloat(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2fRawFloat(
  override val backingSeq: BufferFloat1RawFloat,
  override val offset: Int,
  override val stride: Int
) extends BaseVec2f[RawFloat](backingSeq) with DataView[Vec2f, RawFloat] {
  def asReadOnlySeq() = new ViewVec2fRawFloat(
    backingSeq.asReadOnlySeq(), offset, stride
  )

  override def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec2fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}


// Vec3f RawFloat
private[buffer] final class ArrayVec3fRawFloat(
  override val backingSeq: ArrayFloat1RawFloat
) extends BaseVec3f[RawFloat](backingSeq) with DataArray[Vec3f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)
  def asReadOnlySeq() = new ArrayVec3fRawFloat(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3fRawFloat(
  override val backingSeq: BufferFloat1RawFloat
) extends BaseVec3f[RawFloat](backingSeq) with DataBuffer[Vec3f, RawFloat] {
  def asReadOnlySeq() = new BufferVec3fRawFloat(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3fRawFloat(
  override val backingSeq: BufferFloat1RawFloat,
  override val offset: Int,
  override val stride: Int
) extends BaseVec3f[RawFloat](backingSeq) with DataView[Vec3f, RawFloat] {
  def asReadOnlySeq() = new ViewVec3fRawFloat(
    backingSeq.asReadOnlySeq(), offset, stride
  )
  
  override def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Float]) =
    new ArrayVec3fRawFloat(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3fRawFloat(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}


// Vec4f NUByte
private[buffer] final class ArrayVec4fNUByte(
  override val backingSeq: ArrayFloat1NUByte
) extends BaseVec4f[NUByte](backingSeq) with DataArray[Vec4f, NUByte] {
  def this() = this(new ArrayFloat1NUByte)
  def asReadOnlySeq() = new ArrayVec4fNUByte(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4fNUByte(
  override val backingSeq: BufferFloat1NUByte
) extends BaseVec4f[NUByte](backingSeq) with DataBuffer[Vec4f, NUByte] {
  def asReadOnlySeq() = new BufferVec4fNUByte(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4fNUByte(
  override val backingSeq: BufferFloat1NUByte,
  override val offset: Int,
  override val stride: Int
) extends BaseVec4f[NUByte](backingSeq) with DataView[Vec4f, NUByte] {
  def asReadOnlySeq() = new ViewVec4fNUByte(
    backingSeq.asReadOnlySeq(), offset, stride
  )

  override def apply(i: Int) :AnyVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Byte]) =
    new ArrayVec4fNUByte(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4fNUByte(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}
