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
private[buffer] abstract class BaseVec3d[+R <: DefinedDouble](
  primitive: Contiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec3d, R](primitive, off, str) {
  final def elemManifest = Vec3d.Manifest
  final def readManifest = Vec3d.ReadManifest
  final def components: Int = 3

  def mkDataArray(array: R#Array @uncheckedVariance)
  :DataArray[Vec3d, R] =
    new ArrayVec3d[R](
      backing.mkDataArray(array).asInstanceOf[DataArray[RDouble, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec3d, R] =
    new BufferVec3d[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RDouble, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec3d, R] =
    new ViewVec3d[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RDouble, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
}

private[buffer] final class ArrayVec3d[+R <: DefinedDouble](
  backing: DataArray[RDouble, R]
) extends BaseVec3d[R](backing, 0, 3) with DataArray[Vec3d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec3d(
    backing.asReadOnly().asInstanceOf[DataArray[RDouble, R]]
  )

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3d[+R <: DefinedDouble](
  backing: DataBuffer[RDouble, R]
) extends BaseVec3d[R](backing, 0, 3) with DataBuffer[Vec3d, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec3d(
    backing.asReadOnly().asInstanceOf[DataBuffer[RDouble, R]]
  )

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3d[+R <: DefinedDouble](
  backing: DataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec3d[R](backing, off, str) with DataView[Vec3d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec3d(
    backing.asReadOnly().asInstanceOf[DataBuffer[RDouble, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}
