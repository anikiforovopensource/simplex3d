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
private[buffer] abstract class BaseVec4d[+R <: DefinedDouble](
  backing: ContiguousSeq[Double1, R], off: Int, str: Int
) extends CompositeSeq[Vec4d, R](backing, off, str) {
  final def elementManifest = Vec4d.Manifest
  final def components: Int = 4

  def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  def mkDataArray(array: R#ArrayType @uncheckedVariance)
  :DataArray[Vec4d, R] =
    new ArrayVec4d[R](
      backingSeq.mkDataArray(array).asInstanceOf[DataArray[Double1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec4d, R] =
    new BufferVec4d[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Double1, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec4d, R] =
    new ViewVec4d[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Double1, R]],
      off, str
    )
}

private[buffer] final class ArrayVec4d[+R <: DefinedDouble](
  backingSeq: DataArray[Double1, R]
) extends BaseVec4d[R](backingSeq, 0, 4) with DataArray[Vec4d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Double1, R]]
  )
}

private[buffer] final class BufferVec4d[+R <: DefinedDouble](
  backingSeq: DataBuffer[Double1, R]
) extends BaseVec4d[R](backingSeq, 0, 4) with DataBuffer[Vec4d, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Double1, R]]
  )
}

private[buffer] final class ViewVec4d[+R <: DefinedDouble](
  backingSeq: DataBuffer[Double1, R], off: Int, str: Int
) extends BaseVec4d[R](backingSeq, off, str) with DataView[Vec4d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4d(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Double1, R]],
    offset, stride
  )
}
