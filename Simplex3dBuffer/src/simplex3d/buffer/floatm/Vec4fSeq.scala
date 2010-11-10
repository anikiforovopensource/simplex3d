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
private[buffer] abstract class BaseVec4f[+R <: DefinedFloat](
  backing: ContiguousSeq[Float1, R], off: Int, str: Int
) extends CompositeSeq[Vec4f, R](backing, off, str) {
  final def elementManifest = Vec4f.ReadManifest
  final def components: Int = 4

  def mkDataArray(array: R#ArrayType @uncheckedVariance)
  :DataArray[Vec4f, R] =
    new ArrayVec4f[R](
      backingSeq.mkDataArray(array).asInstanceOf[DataArray[Float1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec4f, R] =
    new BufferVec4f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec4f, R] =
    new ViewVec4f[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Float1, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableFloatData(components, rawType)
}

private[buffer] final class ArrayVec4f[+R <: DefinedFloat](
  backingSeq: DataArray[Float1, R]
) extends BaseVec4f[R](backingSeq, 0, 4) with DataArray[Vec4f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Float1, R]]
  )

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}

private[buffer] final class BufferVec4f[+R <: DefinedFloat](
  backingSeq: DataBuffer[Float1, R]
) extends BaseVec4f[R](backingSeq, 0, 4) with DataBuffer[Vec4f, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]]
  )

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}

private[buffer] final class ViewVec4f[+R <: DefinedFloat](
  backingSeq: DataBuffer[Float1, R], off: Int, str: Int
) extends BaseVec4f[R](backingSeq, off, str) with DataView[Vec4f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec4f(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Float1, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2),
      backingSeq(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
    backingSeq(j + 3) = v.w
  }
}
