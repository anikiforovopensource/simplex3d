/*
 * Simplex3d, DoubleBuffer module
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

package simplex3d.buffer.doublem

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.doublem._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec2d[+R <: ReadableDouble](
  backing: ContiguousSeq[Double1, R], offset: Int, stride: Int
) extends CompositeSeq[Vec2d, R](backing, offset, stride) {
  final def elementManifest = Vec2d.Manifest
  final def components: Int = 2

  def apply(i: Int) :ReadVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  def mkReadDataArray(size: Int)
  :ReadDataArray[Vec2d, R] =
    new ArrayVec2d[R](
      backingSeq.mkReadDataArray(size*2).asInstanceOf[DataArray[Double1, R]]
    )

  def mkReadDataArray(array: R#ArrayType @uncheckedVariance)
  :ReadDataArray[Vec2d, R] =
    new ArrayVec2d[R](
      backingSeq.mkReadDataArray(array).asInstanceOf[DataArray[Double1, R]]
    )

  def mkReadDataBuffer(size: Int)
  :ReadDataBuffer[Vec2d, R] =
    new BufferVec2d[R](
      backingSeq.mkReadDataBuffer(size*2).asInstanceOf[DataBuffer[Double1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec2d, R] =
    new BufferVec2d[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Double1, R]]
    )

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int)
  :ReadDataView[Vec2d, R] =
    new ViewVec2d[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Double1, R]],
      offset, stride
    )
}

private[buffer] final class ArrayVec2d[+R <: ReadableDouble](
  backingSeq: DataArray[Double1, R]
) extends BaseVec2d[R](backingSeq, 0, 2) with DataArray[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Double1, R]]
  )
}

private[buffer] final class BufferVec2d[+R <: ReadableDouble](
  backingSeq: DataBuffer[Double1, R]
) extends BaseVec2d[R](backingSeq, 0, 2) with DataBuffer[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Double1, R]]
  )
}

private[buffer] final class ViewVec2d[+R <: ReadableDouble](
  backingSeq: DataBuffer[Double1, R],
  offset: Int,
  stride: Int
) extends BaseVec2d[R](backingSeq, offset, stride) with DataView[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Double1, R]],
    offset, stride
  )
}
