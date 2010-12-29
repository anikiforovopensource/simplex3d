/*
 * Simplex3d, DoubleData module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dData.
 *
 * Simplex3dData is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.data.double

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.doublex._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec3d[+R <: DefinedDouble](
  primitive: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec3d, R, DefinedDouble](primitive, off, str) {
  final def elemManifest = Vec3d.Manifest
  final def readManifest = Vec3d.ReadManifest
  final def components: Int = 3

  final def mkReadDataArray[P <: DefinedDouble](primitive: ReadDataArray[Vec3d#Component, P])
  :ReadDataArray[Vec3d, P] = {
    (primitive.rawType match {
      case UByte => new impl.ArrayVec3dUByte(primitive.asInstanceOf[ArrayRDoubleUByte])
      case RFloat => new impl.ArrayVec3dRFloat(primitive.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec3d(primitive)
    }).asInstanceOf[ReadDataArray[Vec3d, P]]
  }
  final def mkReadDataBuffer[P <: DefinedDouble](primitive: ReadDataBuffer[Vec3d#Component, P])
  :ReadDataBuffer[Vec3d, P] = {
    (primitive.rawType match {
      case UByte => new impl.BufferVec3dUByte(primitive.asInstanceOf[BufferRDoubleUByte])
      case RFloat => new impl.BufferVec3dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec3d(primitive)
    }).asInstanceOf[ReadDataBuffer[Vec3d, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    primitive: ReadDataBuffer[Vec3d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3d, P] = {
    (primitive.rawType match {
      case UByte => new impl.ViewVec3dUByte(primitive.asInstanceOf[BufferRDoubleUByte], off, str)
      case RFloat => new impl.ViewVec3dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec3d(primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec3d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec3d[+R <: DefinedDouble](
  primitive: ReadDataArray[RDouble, R]
) extends BaseVec3d[R](primitive, 0, 3) with DataArray[Vec3d, R] {
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

private[data] final class BufferVec3d[+R <: DefinedDouble](
  primitive: ReadDataBuffer[RDouble, R]
) extends BaseVec3d[R](primitive, 0, 3) with DataBuffer[Vec3d, R] {
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

private[data] final class ViewVec3d[+R <: DefinedDouble](
  primitive: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec3d[R](primitive, off, str) with DataView[Vec3d, R] {
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
