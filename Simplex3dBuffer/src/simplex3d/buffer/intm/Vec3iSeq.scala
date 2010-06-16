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

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.math.intm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec3i[+R <: ReadableInt](
  seq: ContiguousSeq[Int1, R]
) extends GenericSeq[Vec3i, R](seq) {
  final def elementManifest = Manifest.Vec3iClassManifest
  final def components: Int = 3

  def apply(i: Int) :AnyVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      seq(j),
      seq(j + 1),
      seq(j + 2)
    )
  }
  def update(i: Int, v: AnyVec3i) {
    val j = offset + i*stride
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
  }

  def mkDataArray(size: Int) :DataArray[Vec3i, R] =
    new ArrayVec3i[R](backingSeq.mkDataArray(size*3))

  def mkDataArray(array: R#ArrayType @uncheckedVariance) :DataArray[Vec3i, R] =
    new ArrayVec3i[R](backingSeq.mkDataArray(array))

  def mkDataBuffer(size: Int) :DataBuffer[Vec3i, R] =
    new BufferVec3i[R](backingSeq.mkDataBuffer(size*3))

  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[Vec3i, R] =
    new BufferVec3i[R](backingSeq.mkDataBuffer(byteBuffer))

  def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[Vec3i, R] =
    new ViewVec3i[R](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec3i[+R <: ReadableInt](
  override val backingSeq: DataArray[Int1, R]
) extends BaseVec3i[R](backingSeq) with DataArray[Vec3i, R] {
  def asReadOnly() = new ArrayVec3i(
    backingSeq.asReadOnly().asInstanceOf[DataArray[Int1, R]]
  )
}

private[buffer] final class BufferVec3i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R]
) extends BaseVec3i[R](backingSeq) with DataBuffer[Vec3i, R] {
  def asReadOnly() = new BufferVec3i(
    backingSeq.asReadOnly().asInstanceOf[DataBuffer[Int1, R]]
  )
}

private[buffer] final class ViewVec3i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec3i[R](backingSeq) with DataView[Vec3i, R] {
  def asReadOnly() = new ViewVec3i(
    backingSeq.asReadOnly().asInstanceOf[DataBuffer[Int1, R]], offset, stride
  )
}
