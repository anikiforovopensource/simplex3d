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
import simplex3d.math.intm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec2i[+R <: ReadableInt](
  seq: ContiguousSeq[Int1, R]
) extends CompositeSeq[Vec2i, R](seq) {
  final def elementManifest = Manifest.Vec2iClassManifest
  final def components: Int = 2

  def apply(i: Int) :AnyVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      seq(j),
      seq(j + 1)
    )
  }
  def update(i: Int, v: AnyVec2i) {
    val j = offset + i*stride
    seq(j) = v.x
    seq(j + 1) = v.y
  }

  def mkDataArray(size: Int) :DataArray[Vec2i, R] =
    new ArrayVec2i[R](backingSeq.mkDataArray(size*2))

  def mkDataArray(array: R#ArrayType @uncheckedVariance) :DataArray[Vec2i, R] =
    new ArrayVec2i[R](backingSeq.mkDataArray(array))

  def mkDataBuffer(size: Int) :DataBuffer[Vec2i, R] =
    new BufferVec2i[R](backingSeq.mkDataBuffer(size*2))

  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[Vec2i, R] =
    new BufferVec2i[R](backingSeq.mkDataBuffer(byteBuffer))

  def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[Vec2i, R] =
    new ViewVec2i[R](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec2i[+R <: ReadableInt](
  override val backingSeq: DataArray[Int1, R]
) extends BaseVec2i[R](backingSeq) with DataArray[Vec2i, R] {
  def asReadOnlySeq() = new ArrayVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Int1, R]]
  )
}

private[buffer] final class BufferVec2i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R]
) extends BaseVec2i[R](backingSeq) with DataBuffer[Vec2i, R] {
  def asReadOnlySeq() = new BufferVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]]
  )
}

private[buffer] final class ViewVec2i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec2i[R](backingSeq) with DataView[Vec2i, R] {
  def asReadOnlySeq() = new ViewVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]], offset, stride
  )
}
