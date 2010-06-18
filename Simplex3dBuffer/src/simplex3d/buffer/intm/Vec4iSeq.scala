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
private[buffer] abstract class BaseVec4i[+R <: ReadableInt](
  seq: ContiguousSeq[Int1, R]
) extends GenericSeq[Vec4i, R](seq) {
  final def elementManifest = Manifest.Vec4iClassManifest
  final def components: Int = 4

  def apply(i: Int) :AnyVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      seq(j),
      seq(j + 1),
      seq(j + 2),
      seq(j + 3)
    )
  }
  def update(i: Int, v: AnyVec4i) {
    val j = offset + i*stride
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
    seq(j + 3) = v.w
  }

  def mkDataArray(size: Int) :DataArray[Vec4i, R] =
    new ArrayVec4i[R](backingSeq.mkDataArray(size*4))

  def mkDataArray(array: R#ArrayType @uncheckedVariance) :DataArray[Vec4i, R] =
    new ArrayVec4i[R](backingSeq.mkDataArray(array))

  def mkDataBuffer(size: Int) :DataBuffer[Vec4i, R] =
    new BufferVec4i[R](backingSeq.mkDataBuffer(size*4))

  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[Vec4i, R] =
    new BufferVec4i[R](backingSeq.mkDataBuffer(byteBuffer))

  def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[Vec4i, R] =
    new ViewVec4i[R](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec4i[+R <: ReadableInt](
  override val backingSeq: DataArray[Int1, R]
) extends BaseVec4i[R](backingSeq) with DataArray[Vec4i, R] {
  def asReadOnlySeq() = new ArrayVec4i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Int1, R]]
  )
}

private[buffer] final class BufferVec4i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R]
) extends BaseVec4i[R](backingSeq) with DataBuffer[Vec4i, R] {
  def asReadOnlySeq() = new BufferVec4i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]]
  )
}

private[buffer] final class ViewVec4i[+R <: ReadableInt](
  override val backingSeq: DataBuffer[Int1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec4i[R](backingSeq) with DataView[Vec4i, R] {
  def asReadOnlySeq() = new ViewVec4i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]], offset, stride
  )
}
