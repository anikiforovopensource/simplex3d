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
  backing: ReadContiguousSeq[Int1, R]
) extends CompositeSeq[Vec4i, R](backing) {
  final def elementManifest = Vec4i.Manifest
  final def components: Int = 4

  def apply(i: Int) :AnyVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  def update(i: Int, v: AnyVec4i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }

  def mkReadDataArray(size: Int) :ReadDataArray[Vec4i, R] =
    new ArrayVec4i[R](backingSeq.mkReadDataArray(size*4))

  def mkReadDataArray(array: R#ArrayType @uncheckedVariance) :ReadDataArray[Vec4i, R] =
    new ArrayVec4i[R](backingSeq.mkReadDataArray(array))

  def mkReadDataBuffer(size: Int) :ReadDataBuffer[Vec4i, R] =
    new BufferVec4i[R](backingSeq.mkReadDataBuffer(size*4))

  def mkReadDataBuffer(byteBuffer: ByteBuffer) :ReadDataBuffer[Vec4i, R] =
    new BufferVec4i[R](backingSeq.mkReadDataBuffer(byteBuffer))

  def mkReadDataView(byteBuffer: ByteBuffer, offset: Int, stride: Int) :ReadDataView[Vec4i, R] =
    new ViewVec4i[R](backingSeq.mkReadDataBuffer(byteBuffer), offset, stride)
}

private[buffer] final class ArrayVec4i[+R <: ReadableInt](
  backing: ReadDataArray[Int1, R]
) extends BaseVec4i[R](backing) with DataArray[Vec4i, R] {
  val backingSeq = backing.asInstanceOf[DataArray[Int1, R]]
  def asReadOnlySeq() = new ArrayVec4i(backingSeq.asReadOnlySeq())
}

private[buffer] final class BufferVec4i[+R <: ReadableInt](
  backing: ReadDataBuffer[Int1, R]
) extends BaseVec4i[R](backing) with DataBuffer[Vec4i, R] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Int1, R]]
  def asReadOnlySeq() = new BufferVec4i(backingSeq.asReadOnlySeq())
}

private[buffer] final class ViewVec4i[+R <: ReadableInt](
  backing: ReadDataBuffer[Int1, R],
  override val offset: Int,
  override val stride: Int
) extends BaseVec4i[R](backing) with DataView[Vec4i, R] {
  val backingSeq = backing.asInstanceOf[DataBuffer[Int1, R]]
  def asReadOnlySeq() = new ViewVec4i(
    backingSeq.asReadOnlySeq(), offset, stride
  )
}
