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

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec3f[+R <: ReadableFloat](
  backing: ContiguousSeq[Float1, R], offset: Int, stride: Int
) extends CompositeSeq[Vec3f, R](backing, offset, stride) {
  final def elementManifest = Vec3f.Manifest
  final def components: Int = 3

  def apply(i: Int) :ReadVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }

  def mkReadDataArray(size: Int)
  :ReadDataArray[Vec3f, R] =
    new ArrayVec3f[R](
      backingSeq.mkReadDataArray(size*3).asInstanceOf[DataArray[Float1, R]]
    )

  def mkReadDataArray(array: R#ArrayType @uncheckedVariance)
  :ReadDataArray[Vec3f, R] =
    new ArrayVec3f[R](
      backingSeq.mkReadDataArray(array).asInstanceOf[DataArray[Float1, R]]
    )

  def mkReadDataBuffer(size: Int)
  :ReadDataBuffer[Vec3f, R] =
    new BufferVec3f[R](
      backingSeq.mkReadDataBuffer(size*3).asInstanceOf[DataBuffer[Float1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec3f, R] =
    new BufferVec3f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]]
    )

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int)
  :ReadDataView[Vec3f, R] =
    new ViewVec3f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]],
      offset, stride
    )
}

private[buffer] final class ArrayVec3f[+R <: ReadableFloat](
  override val backingSeq: DataArray[Float1, R]
) extends BaseVec3f[R](backingSeq, 0, 3) with DataArray[Vec3f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Float1, R]]
  )
}

private[buffer] final class BufferVec3f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R]
) extends BaseVec3f[R](backingSeq, 0, 3) with DataBuffer[Vec3f, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]]
  )
}

private[buffer] final class ViewVec3f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R],
  offset: Int,
  stride: Int
) extends BaseVec3f[R](backingSeq, offset, stride) with DataView[Vec3f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]],
    offset, stride
  )
}
