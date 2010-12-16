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
  primitive: Contiguous[RFloat, R], off: Int, str: Int
) extends CompositeSeq[Vec2f, R](primitive, off, str) {
  final def elemManifest = Vec2f.Manifest
  final def readManifest = Vec2f.ReadManifest
  final def components: Int = 2

  def mkDataArray(array: R#Array @uncheckedVariance)
  :DataArray[Vec2f, R] =
    new ArrayVec2f[R](
      backing.mkDataArray(array).asInstanceOf[DataArray[RFloat, R]]
    )

  def mkReadDataBuffer(byteBuffer: ByteBuffer)
  :ReadDataBuffer[Vec2f, R] =
    new BufferVec2f[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RFloat, R]]
    )

  protected def mkReadDataViewInstance(byteBuffer: ByteBuffer, off: Int, str: Int)
  :ReadDataView[Vec2f, R] =
    new ViewVec2f[R](
      backing.mkReadDataBuffer(byteBuffer).asInstanceOf[DataBuffer[RFloat, R]],
      off, str
    )

  override def mkSerializableInstance() = new SerializableFloatData(components, rawType)
}

private[buffer] final class ArrayVec2f[+R <: DefinedFloat](
  backing: DataArray[RFloat, R]
) extends BaseVec2f[R](backing, 0, 2) with DataArray[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ArrayVec2f(
    backing.asReadOnly().asInstanceOf[DataArray[RFloat, R]]
  )

  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class BufferVec2f[+R <: DefinedFloat](
  backing: DataBuffer[RFloat, R]
) extends BaseVec2f[R](backing, 0, 2) with DataBuffer[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new BufferVec2f(
    backing.asReadOnly().asInstanceOf[DataBuffer[RFloat, R]]
  )

  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class ViewVec2f[+R <: DefinedFloat](
  backing: DataBuffer[RFloat, R], off: Int, str: Int
) extends BaseVec2f[R](backing, off, str) with DataView[Vec2f, R] {
  protected[buffer] def mkReadOnlyInstance() = new ViewVec2f(
    backing.asReadOnly().asInstanceOf[DataBuffer[RFloat, R]],
    offset, stride
  )

  def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}
