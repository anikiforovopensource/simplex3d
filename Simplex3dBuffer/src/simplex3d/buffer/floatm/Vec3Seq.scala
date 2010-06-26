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
  seq: ContiguousSeq[Float1, R]
) extends CompositeSeq[Vec3f, R](seq) {
  final def elementManifest = Manifest.Vec3fClassManifest
  final def components: Int = 3

  def apply(i: Int) :AnyVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      seq(j),
      seq(j + 1),
      seq(j + 2)
    )
  }
  def update(i: Int, v: AnyVec3f) {
    val j = offset + i*stride
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
  }

  def mkDataArray(size: Int) :DataArray[Vec3f, R] =
    new ArrayVec3f[R](backingSeq.mkDataArray(size*3))

  def mkDataArray(
    array: R#ArrayType @uncheckedVariance
  ) :DataArray[Vec3f, R] =
    new ArrayVec3f[R](backingSeq.mkDataArray(array))

  def mkDataBuffer(size: Int) :DataBuffer[Vec3f, R] =
    new BufferVec3f[R](backingSeq.mkDataBuffer(size*3))

  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[Vec3f, R] =
    new BufferVec3f[R](backingSeq.mkDataBuffer(byteBuffer))

  def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[Vec3f, R] =
    new ViewVec3f[R](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec3f[+R <: ReadableFloat](
  override val backingSeq: DataArray[Float1, R]
) extends BaseVec3f[R](backingSeq) with DataArray[Vec3f, R] {
  def asReadOnlySeq() = new ArrayVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Float1, R]]
  )
}

private[buffer] final class BufferVec3f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R]
) extends BaseVec3f[R](backingSeq) with DataBuffer[Vec3f, R] {
  def asReadOnlySeq() = new BufferVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]]
  )
}

private[buffer] final class ViewVec3f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec3f[R](backingSeq) with DataView[Vec3f, R] {
  def asReadOnlySeq() = new ViewVec3f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]], offset, stride
  )
}
