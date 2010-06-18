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
private[buffer] abstract class BaseVec4f[+R <: ReadableFloat](
  seq: ContiguousSeq[Float1, R]
) extends GenericSeq[Vec4f, R](seq) {
  final def elementManifest = Manifest.Vec4fClassManifest
  final def components: Int = 4

  def apply(i: Int) :AnyVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      seq(j),
      seq(j + 1),
      seq(j + 2),
      seq(j + 3)
    )
  }
  def update(i: Int, v: AnyVec4f) {
    val j = offset + i*stride
    seq(j) = v.x
    seq(j + 1) = v.y
    seq(j + 2) = v.z
    seq(j + 3) = v.w
  }

  def mkDataArray(size: Int) :DataArray[Vec4f, R] =
    new ArrayVec4f[R](backingSeq.mkDataArray(size*4))

  def mkDataArray(
    array: R#ArrayType @uncheckedVariance
  ) :DataArray[Vec4f, R] =
    new ArrayVec4f[R](backingSeq.mkDataArray(array))

  def mkDataBuffer(size: Int) :DataBuffer[Vec4f, R] =
    new BufferVec4f[R](backingSeq.mkDataBuffer(size*4))

  def mkDataBuffer(byteBuffer: ByteBuffer) :DataBuffer[Vec4f, R] =
    new BufferVec4f[R](backingSeq.mkDataBuffer(byteBuffer))
  
  def mkDataView(
    byteBuffer: ByteBuffer, offset: Int, stride: Int
  ) :DataView[Vec4f, R] =
    new ViewVec4f[R](backingSeq.mkDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec4f[+R <: ReadableFloat](
  override val backingSeq: DataArray[Float1, R]
) extends BaseVec4f[R](backingSeq) with DataArray[Vec4f, R] {
  def asReadOnlySeq() = new ArrayVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Float1, R]]
  )
}

private[buffer] final class BufferVec4f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R]
) extends BaseVec4f[R](backingSeq) with DataBuffer[Vec4f, R] {
  def asReadOnlySeq() = new BufferVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]]
  )
}

private[buffer] final class ViewVec4f[+R <: ReadableFloat](
  override val backingSeq: DataBuffer[Float1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec4f[R](backingSeq) with DataView[Vec4f, R] {
  def asReadOnlySeq() = new ViewVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]], offset, stride
  )
}
