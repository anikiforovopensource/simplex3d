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
  backing: ContiguousSeq[Int1, R], off: Int, str: Int, sz: java.lang.Integer
) extends CompositeSeq[Vec2i, R](backing, off, str, sz) {
  final def elementManifest = Vec2i.Manifest
  final def components: Int = 2

  def apply(i: Int) :ConstVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  def mkDataArray(array: R#ArrayType @uncheckedVariance)
  :DataArray[Vec2i, R] =
    new ArrayVec2i[R](
      backingSeq.mkDataArray(array).asInstanceOf[DataArray[Int1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec2i, R] =
    new BufferVec2i[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Int1, R]]
    )

  protected def mkReadDataView(byteBuffer: ByteBuffer, off: Int, str: Int, sz: java.lang.Integer)
  :ReadDataView[Vec2i, R] =
    new ViewVec2i[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Int1, R]],
      off, str, sz
    )
}

private[buffer] final class ArrayVec2i[+R <: ReadableInt](
  backingSeq: DataArray[Int1, R]
) extends BaseVec2i[R](backingSeq, 0, 2, null) with DataArray[Vec2i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Int1, R]]
  )
}

private[buffer] final class BufferVec2i[+R <: ReadableInt](
  backingSeq: DataBuffer[Int1, R]
) extends BaseVec2i[R](backingSeq, 0, 2, null) with DataBuffer[Vec2i, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]]
  )
}

private[buffer] final class ViewVec2i[+R <: ReadableInt](
  backingSeq: DataBuffer[Int1, R], off: Int, str: Int, sz: java.lang.Integer
) extends BaseVec2i[R](backingSeq, off, str, sz) with DataView[Vec2i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]],
    offset, stride, size
  )
}
