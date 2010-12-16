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
  primitive: Contiguous[SInt, R], off: Int, str: Int
) extends CompositeSeq[Vec3i, R](primitive, off, str) {
  final def elemManifest = Vec3i.Manifest
  final def readManifest = Vec3i.ReadManifest
  final def components: Int = 3

  def mkDataArray(array: R#Array @uncheckedVariance)
  :DataArray[Vec3i, R] =
    new ArrayVec3i[R](
      backing.mkDataArray(array).asInstanceOf[DataArray[SInt, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec3i, R] =
    new BufferVec3i[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[SInt, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec3i, R] =
    new ViewVec3i[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[SInt, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableIntData(components, rawType)
}

private[buffer] final class ArrayVec3i[+R <: DefinedInt](
  backing: DataArray[SInt, R]
) extends BaseVec3i[R](backing, 0, 3) with DataArray[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3i(
    backing.asReadOnly().asInstanceOf[DataArray[SInt, R]]
  )

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3i[+R <: DefinedInt](
  backing: DataBuffer[SInt, R]
) extends BaseVec3i[R](backing, 0, 3) with DataBuffer[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3i(
    backing.asReadOnly().asInstanceOf[DataBuffer[SInt, R]]
  )

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3i[+R <: DefinedInt](
  backing: DataBuffer[SInt, R], off: Int, str: Int
) extends BaseVec3i[R](backing, off, str) with DataView[Vec3i, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3i(
    backing.asReadOnly().asInstanceOf[DataBuffer[SInt, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}
