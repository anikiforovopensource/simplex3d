/*
 * Simplex3d, CoreBuffer module
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

package simplex3d.buffer

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec3i[+R <: DefinedInt](
  backing: ContiguousSeq[Int1, R], off: Int, str: Int
) extends CompositeSeq[Vec3i, R](backing, off, str) {
  final def elementManifest = Vec3i.ReadManifest
  final def components: Int = 3

  def mkDataArray(array: R#ArrayType @uncheckedVariance)
  :DataArray[Vec3i, R] =
    new ArrayVec3i[R](
      backingSeq.mkDataArray(array).asInstanceOf[DataArray[Int1, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec3i, R] =
    new BufferVec3i[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Int1, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec3i, R] =
    new ViewVec3i[R](
      backingSeq.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[Int1, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableIntData(components, rawType)
}

private[buffer] final class ArrayVec3i[+R <: DefinedInt](
  backingSeq: DataArray[Int1, R]
) extends BaseVec3i[R](backingSeq, 0, 3) with DataArray[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataArray[Int1, R]]
  )

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3i[+R <: DefinedInt](
  backingSeq: DataBuffer[Int1, R]
) extends BaseVec3i[R](backingSeq, 0, 3) with DataBuffer[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]]
  )

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3i[+R <: DefinedInt](
  backingSeq: DataBuffer[Int1, R], off: Int, str: Int
) extends BaseVec3i[R](backingSeq, off, str) with DataView[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3i(
    backingSeq.asReadOnlySeq().asInstanceOf[DataBuffer[Int1, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backingSeq(j),
      backingSeq(j + 1),
      backingSeq(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backingSeq(j) = v.x
    backingSeq(j + 1) = v.y
    backingSeq(j + 2) = v.z
  }
}
