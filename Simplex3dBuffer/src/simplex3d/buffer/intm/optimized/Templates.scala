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
  backing: ArrayInt1SInt
) extends BaseVec2i[SInt](backing) with DataArray[Vec2i, SInt] {
  override def backingSeq: ArrayInt1SInt = backing
  def this() = this(new ArrayInt1SInt)
  def asReadOnlySeq() = new ArrayVec2iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2iSInt(
  backing: BufferInt1SInt
) extends BaseVec2i[SInt](backing) with DataBuffer[Vec2i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new BufferVec2iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2iSInt(
  backing: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec2i[SInt](backing) with DataView[Vec2i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new ViewVec2iSInt(backing.asReadOnlySeq(), offset, stride)

  override def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3i SInt
private[buffer] final class ArrayVec3iSInt(
  backing: ArrayInt1SInt
) extends BaseVec3i[SInt](backing) with DataArray[Vec3i, SInt] {
  def this() = this(new ArrayInt1SInt)
  override def backingSeq: ArrayInt1SInt = backing
  def asReadOnlySeq() = new ArrayVec3iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3iSInt(
  backing: BufferInt1SInt
) extends BaseVec3i[SInt](backing) with DataBuffer[Vec3i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new BufferVec3iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3iSInt(
  backing: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec3i[SInt](backing) with DataView[Vec3i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new ViewVec3iSInt(backing.asReadOnlySeq(), offset, stride)
  
  override def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4i SInt
private[buffer] final class ArrayVec4iSInt(
  backing: ArrayInt1SInt
) extends BaseVec4i[SInt](backing) with DataArray[Vec4i, SInt] {
  override def backingSeq: ArrayInt1SInt = backing
  def this() = this(new ArrayInt1SInt)
  def asReadOnlySeq() = new ArrayVec4iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4iSInt(
  backing: BufferInt1SInt
) extends BaseVec4i[SInt](backing) with DataBuffer[Vec4i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new BufferVec4iSInt(backing.asReadOnlySeq())

  override def apply(i: Int) :AnyVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4iSInt(
  backing: BufferInt1SInt,
  override val offset: Int,
  override val stride: Int
) extends BaseVec4i[SInt](backing) with DataView[Vec4i, SInt] {
  override def backingSeq: BufferInt1SInt = backing
  def asReadOnlySeq() = new ViewVec4iSInt(backing.asReadOnlySeq(), offset, stride)

  override def apply(i: Int) :AnyVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: AnyVec4i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backingSeq.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}
