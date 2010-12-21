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
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec2d[+R <: DefinedDouble](
  primitive: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec2d, R, DefinedDouble](primitive, off, str) {
  final def elemManifest = Vec2d.Manifest
  final def readManifest = Vec2d.ReadManifest
  final def components: Int = 2

  final def mkReadDataArray[P <: DefinedDouble](primitive: ReadDataArray[Vec2d#Component, P])
  :ReadDataArray[Vec2d, P] = {
    (primitive.rawType match {
      case RFloat => new impl.ArrayVec2dRFloat(primitive.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec2d(primitive)
    }).asInstanceOf[ReadDataArray[Vec2d, P]]
  }
  final def mkReadDataBuffer[P <: DefinedDouble](primitive: ReadDataBuffer[Vec2d#Component, P])
  :ReadDataBuffer[Vec2d, P] = {
    (primitive.rawType match {
      case RFloat => new impl.BufferVec2dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec2d(primitive)
    }).asInstanceOf[ReadDataBuffer[Vec2d, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    primitive: ReadDataBuffer[Vec2d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec2d, P] = {
    (primitive.rawType match {
      case RFloat => new impl.ViewVec2dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec2d(primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec2d, P]]
  }

  override def mkSerializableInstance() = new SerializableDoubleData(components, rawType)
}

private[buffer] final class ArrayVec2d[+R <: DefinedDouble](
  primitive: ReadDataArray[RDouble, R]
) extends BaseVec2d[R](primitive, 0, 2) with DataArray[Vec2d, R] {
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
  primitive: ReadDataBuffer[RDouble, R]
) extends BaseVec2d[R](primitive, 0, 2) with DataBuffer[Vec2d, R] {
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
  primitive: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec2d[R](primitive, off, str) with DataView[Vec2d, R] {
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
