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
private[buffer] abstract class BaseVec2d[+R <: DefinedDouble](
  primitive: Contiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec2d, R](primitive, off, str) {
  final def elemManifest = Vec2d.Manifest
  final def readManifest = Vec2d.ReadManifest
  final def components: Int = 2

  def mkDataArray(array: R#Array @uncheckedVariance)
  :DataArray[Vec2d, R] =
    new ArrayVec2d[R](
      backing.mkDataArray(array).asInstanceOf[DataArray[RDouble, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec2d, R] =
    new BufferVec2d[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RDouble, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec2d, R] =
    new ViewVec2d[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RDouble, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
}

private[buffer] final class ArrayVec2d[+R <: DefinedDouble](
  backing: DataArray[RDouble, R]
) extends BaseVec2d[R](backing, 0, 2) with DataArray[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2d(
    backing.asReadOnly().asInstanceOf[DataArray[RDouble, R]]
  )

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class BufferVec2d[+R <: DefinedDouble](
  backing: DataBuffer[RDouble, R]
) extends BaseVec2d[R](backing, 0, 2) with DataBuffer[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2d(
    backing.asReadOnly().asInstanceOf[DataBuffer[RDouble, R]]
  )

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class ViewVec2d[+R <: DefinedDouble](
  backing: DataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec2d[R](backing, off, str) with DataView[Vec2d, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2d(
    backing.asReadOnly().asInstanceOf[DataBuffer[RDouble, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}
