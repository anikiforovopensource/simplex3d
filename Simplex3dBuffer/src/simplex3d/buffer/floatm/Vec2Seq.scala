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
private[buffer] abstract class BaseVec2f[+R <: ReadableFloat](
  backing: ReadContiguousSeq[Float1, R]
) extends CompositeSeq[Vec2f, R](backing) {
  final def elementManifest = Vec2f.Manifest
  final def components: Int = 2

  def apply(i: Int) :AnyVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  def update(i: Int, v: AnyVec2f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  def mkReadDataArray(size: Int) :ReadDataArray[Vec2f, R] =
    new ArrayVec2f[R](backingSeq.mkReadDataArray(size*2))

  def mkReadDataArray(array: R#ArrayType @uncheckedVariance) :ReadDataArray[Vec2f, R] =
    new ArrayVec2f[R](backingSeq.mkReadDataArray(array))

  def mkReadDataBuffer(size: Int) :ReadDataBuffer[Vec2f, R] =
    new BufferVec2f[R](backingSeq.mkReadDataBuffer(size*2))

  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[Vec2f, R] =
    new BufferVec2f[R](backingSeq.mkReadDataBuffer(byteBuffer))

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[Vec2f, R] =
    new ViewVec2f[R](backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec2f[+R <: ReadableFloat](
  backing: ReadDataArray[Float1, R]
) extends BaseVec2f[R](backing) with DataArray[Vec2f, R] {
  val backingSeq = backing.asInstanceOf[DataArray[Float1, R]]
  def asReadOnlySeq() = new ArrayVec2f(backingSeq.asReadOnlySeq())
}

private[buffer] final class BufferVec2f[+R <: ReadableFloat](
  backing: ReadDataBuffer[Float1, R]
) extends BaseVec2f[R](backing) with DataBuffer[Vec2f, R] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1, R]]
  def asReadOnlySeq() = new BufferVec2f(backingSeq.asReadOnlySeq())
}

private[buffer] final class ViewVec2f[+R <: ReadableFloat](
  backing: ReadDataBuffer[Float1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec2f[R](backing) with DataView[Vec2f, R] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Float1, R]]
  def asReadOnlySeq() = new ViewVec2f(
    backingSeq.asReadOnlySeq(), offset, stride
  )
}
