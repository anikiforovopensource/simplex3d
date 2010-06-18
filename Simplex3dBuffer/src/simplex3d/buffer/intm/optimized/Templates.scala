/*
 * Simplex3d, IntBuffer module
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

package simplex3d.buffer.intm
package optimized

import java.nio._
import simplex3d.math.intm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2i SInt
private[buffer] final class ArrayVec2iSInt(
  override val backingSeq: ArrayInt1SInt
) extends BaseVec2i[SInt](backingSeq) with DataArray[Vec2i, SInt] {
  def this() = this(new ArrayInt1SInt)
  def asReadOnlySeq() = new ArrayVec2iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2iSInt(
  override val backingSeq: BufferInt1SInt
) extends BaseVec2i[SInt](backingSeq) with DataBuffer[Vec2i, SInt] {
  def asReadOnlySeq() = new BufferVec2iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2iSInt(
  override val backingSeq: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec2i[SInt](backingSeq) with DataView[Vec2i, SInt] {
  def asReadOnlySeq() = new ViewVec2iSInt(backingSeq.asReadOnlySeq(), offset, stride)

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  override def mkDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(size*2))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(size*2))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}


// Vec3i SInt
private[buffer] final class ArrayVec3iSInt(
  override val backingSeq: ArrayInt1SInt
) extends BaseVec3i[SInt](backingSeq) with DataArray[Vec3i, SInt] {
  def this() = this(new ArrayInt1SInt)
  def asReadOnlySeq() = new ArrayVec3iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3iSInt(
  override val backingSeq: BufferInt1SInt
) extends BaseVec3i[SInt](backingSeq) with DataBuffer[Vec3i, SInt] {
  def asReadOnlySeq() = new BufferVec3iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3iSInt(
  override val backingSeq: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec3i[SInt](backingSeq) with DataView[Vec3i, SInt] {
  def asReadOnlySeq() = new ViewVec3iSInt(backingSeq.asReadOnlySeq(), offset, stride)
  
  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  override def mkDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(size*3))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(size*3))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}


// Vec4i SInt
private[buffer] final class ArrayVec4iSInt(
  override val backingSeq: ArrayInt1SInt
) extends BaseVec4i[SInt](backingSeq) with DataArray[Vec4i, SInt] {
  def this() = this(new ArrayInt1SInt)
  def asReadOnlySeq() = new ArrayVec4iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4i = {
    val j = i*4
    ConstVec4i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4iSInt(
  override val backingSeq: BufferInt1SInt
) extends BaseVec4i[SInt](backingSeq) with DataBuffer[Vec4i, SInt] {
  def asReadOnlySeq() = new BufferVec4iSInt(backingSeq.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4i = {
    val j = i*4
    ConstVec4i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4iSInt(
  override val backingSeq: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec4i[SInt](backingSeq) with DataView[Vec4i, SInt] {
  def asReadOnlySeq() = new ViewVec4iSInt(backingSeq.asReadOnlySeq(), offset, stride)

  override def apply(i: Int) :AnyVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  override def mkDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(size*4))
  override def mkDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkDataArray(array))
  override def mkDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(size*4))
  override def mkDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkDataBuffer(byteBuffer))
  override def mkDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}
