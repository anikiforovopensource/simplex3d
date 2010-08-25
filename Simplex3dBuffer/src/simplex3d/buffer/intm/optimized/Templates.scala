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
) extends BaseVec2i[SInt](backing, 0, 2) with DataArray[Vec2i, SInt] {
  def this() = this(new ArrayInt1SInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backing.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec2iSInt(
  backing: BufferInt1SInt
) extends BaseVec2i[SInt](backing, 0, 2) with DataBuffer[Vec2i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backing.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec2iSInt(
  backing: BufferInt1SInt,
  offset: Int,
  stride: Int
) extends BaseVec2i[SInt](backing, offset, stride) with DataView[Vec2i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2iSInt(backing.mkReadOnlyInstance(), offset, stride)

  override def apply(i: Int) :ReadVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backing(j),
      backing(j + 1)
    )
  }
  override def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec2iSInt(backing.mkReadDataArray(size*2))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec2iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(size*2))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec2iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec2iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec3i SInt
private[buffer] final class ArrayVec3iSInt(
  backing: ArrayInt1SInt
) extends BaseVec3i[SInt](backing, 0, 3) with DataArray[Vec3i, SInt] {
  def this() = this(new ArrayInt1SInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backing.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec3iSInt(
  backing: BufferInt1SInt
) extends BaseVec3i[SInt](backing, 0, 3) with DataBuffer[Vec3i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backing.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec3iSInt(
  backing: BufferInt1SInt,
  offset: Int,
  stride: Int
) extends BaseVec3i[SInt](backing, offset, stride) with DataView[Vec3i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3iSInt(backing.mkReadOnlyInstance(), offset, stride)
  
  override def apply(i: Int) :ReadVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  override def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec3iSInt(backing.mkReadDataArray(size*3))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec3iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(size*3))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec3iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec3iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}


// Vec4i SInt
private[buffer] final class ArrayVec4iSInt(
  backing: ArrayInt1SInt
) extends BaseVec4i[SInt](backing, 0, 4) with DataArray[Vec4i, SInt] {
  def this() = this(new ArrayInt1SInt)
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backing.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class BufferVec4iSInt(
  backing: BufferInt1SInt
) extends BaseVec4i[SInt](backing, 0, 4) with DataBuffer[Vec4i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4iSInt(backing.mkReadOnlyInstance())

  override def apply(i: Int) :ReadVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backing.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ViewVec4iSInt(
  backing: BufferInt1SInt,
  offset: Int,
  stride: Int
) extends BaseVec4i[SInt](backing, offset, stride) with DataView[Vec4i, SInt] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4iSInt(backing.mkReadOnlyInstance(), offset, stride)

  override def apply(i: Int) :ReadVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  override def update(i: Int, v: ReadVec4i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }

  override def mkReadDataArray(size: Int) =
    new ArrayVec4iSInt(backing.mkReadDataArray(size*4))
  override def mkReadDataArray(array: Array[Int]) =
    new ArrayVec4iSInt(backing.mkReadDataArray(array))
  override def mkReadDataBuffer(size: Int) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(size*4))
  override def mkReadDataBuffer(byteBuffer: ByteBuffer) =
    new BufferVec4iSInt(backing.mkReadDataBuffer(byteBuffer))
  override def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) =
    new ViewVec4iSInt(backing.mkReadDataBuffer(byteBuffer), offset, stride)
}
