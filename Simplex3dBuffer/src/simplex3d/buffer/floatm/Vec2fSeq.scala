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
private[buffer] abstract class BaseVec2f[+R <: DefinedFloat](
  backing: ContiguousSeq[Float1, R], off: Int, str: Int
) extends CompositeSeq[Vec2f, R](backing, off, str) {
  final def elementManifest = Vec2f.Manifest
  final def components: Int = 2

  def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backingSeq(j),
      backingSeq(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
  }

  def mkDataArray(array: R#ArrayType @uncheckedVariance)
  :DataArray[Vec2f, R] =
    new ArrayVec2f[R](
      backingSeq.mkDataArray(array).asInstanceOf[DataArray[Float1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec2f, R] =
    new BufferVec2f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec2f, R] =
    new ViewVec2f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]],
      off, str
    )
}

private[buffer] final class ArrayVec2f[+R <: DefinedFloat](
  backingSeq: DataArray[Float1, R]
) extends BaseVec2f[R](backingSeq, 0, 2) with DataArray[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Float1, R]]
  )
}

private[buffer] final class BufferVec2f[+R <: DefinedFloat](
  backingSeq: DataBuffer[Float1, R]
) extends BaseVec2f[R](backingSeq, 0, 2) with DataBuffer[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]]
  )
}

private[buffer] final class ViewVec2f[+R <: DefinedFloat](
  backingSeq: DataBuffer[Float1, R], off: Int, str: Int
) extends BaseVec2f[R](backingSeq, off, str) with DataView[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]],
    offset, stride
  )
}
