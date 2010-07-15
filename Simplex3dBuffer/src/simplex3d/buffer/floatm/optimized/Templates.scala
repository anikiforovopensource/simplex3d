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
  backing: ReadDataArray[Float1, RawFloat]
) extends BaseVec2f[RawFloat](backing) with DataArray[Vec2f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)

  val backingSeq = backing.asInstanceOf[DataArray[Float1, RawFloat]]
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
  backing: ReadDataBuffer[Float1, RawFloat]
) extends BaseVec2f[RawFloat](backing) with DataBuffer[Vec2f, RawFloat] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1, RawFloat]]
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
  backing: ReadDataBuffer[Float1,RawFloat],
  override val offset: Int,
  override val stride: Int
) extends BaseVec2f[RawFloat](backing) with DataView[Vec2f, RawFloat] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1,RawFloat]]
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
  backing: ReadDataArray[Float1, RawFloat]
) extends BaseVec3f[RawFloat](backing) with DataArray[Vec3f, RawFloat] {
  def this() = this(new ArrayFloat1RawFloat)

  val backingSeq = backing.asInstanceOf[DataArray[Float1, RawFloat]]
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
  backing: ReadDataBuffer[Float1, RawFloat]
) extends BaseVec3f[RawFloat](backing) with DataBuffer[Vec3f, RawFloat] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1, RawFloat]]
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
  backing: ReadDataBuffer[Float1,RawFloat],
  override val offset: Int,
  override val stride: Int
) extends BaseVec3f[RawFloat](backing) with DataView[Vec3f, RawFloat] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1,RawFloat]]
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
  backing: ReadDataArray[Float1, UByte]
) extends BaseVec4f[UByte](backing) with DataArray[Vec4f, UByte] {
  def this() = this(new ArrayFloat1UByte)

  val backingSeq = backing.asInstanceOf[DataArray[Float1, UByte]]
  def asReadOnlySeq() = new ArrayVec4fUByte(backingSeq.asReadOnlySeq())

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
  backing: ReadDataBuffer[Float1, UByte]
) extends BaseVec4f[UByte](backing) with DataBuffer[Vec4f, UByte] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1, UByte]]
  def asReadOnlySeq() = new BufferVec4fUByte(backingSeq.asReadOnlySeq())

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
  backing: ReadDataBuffer[Float1,UByte],
  override val offset: Int,
  override val stride: Int
) extends BaseVec4f[UByte](backing) with DataView[Vec4f, UByte] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1,UByte]]
  def asReadOnlySeq() = new ViewVec4fUByte(
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
