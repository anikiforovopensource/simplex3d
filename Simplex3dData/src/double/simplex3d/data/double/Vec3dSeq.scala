/*
 * Simplex3dData - Double Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d.data
package double

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.doublex._
import simplex3d.data.extension._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec3d[+R <: TangibleDouble](
  prim: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec3d, R, TangibleDouble](prim, off, str) {
  final def formatManifest = Vec3d.Manifest
  final def accessorManifest = Vec3d.Manifest
  final def components: Int = 3

  final def mkReadDataArray[P <: TangibleDouble](prim: ReadDataArray[Vec3d#Component, P])
  :ReadDataArray[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new ArrayVec3dUByte(prim.asInstanceOf[ArrayRDoubleUByte])
      case RFloat => new ArrayVec3dRFloat(prim.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec3d(prim)
    }).asInstanceOf[ReadDataArray[Vec3d, P]]
  }
  final def mkReadDataBuffer[P <: TangibleDouble](prim: ReadDataBuffer[Vec3d#Component, P])
  :ReadDataBuffer[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new BufferVec3dUByte(prim.asInstanceOf[BufferRDoubleUByte])
      case RFloat => new BufferVec3dRFloat(prim.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec3d(prim)
    }).asInstanceOf[ReadDataBuffer[Vec3d, P]]
  }
  protected final def mkReadDataViewInstance[P <: TangibleDouble](
    prim: ReadDataBuffer[Vec3d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new ViewVec3dUByte(prim.asInstanceOf[BufferRDoubleUByte], off, str)
      case RFloat => new ViewVec3dRFloat(prim.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec3d(prim, off, str)
    }).asInstanceOf[ReadDataView[Vec3d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec3d[+R <: TangibleDouble](
  prim: ReadDataArray[RDouble, R]
) extends BaseVec3d[R](prim, 0, 3) with DataArray[Vec3d, R] {
  type Read = ReadDataArray[Vec3d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
  }
}

private[data] final class BufferVec3d[+R <: TangibleDouble](
  prim: ReadDataBuffer[RDouble, R]
) extends BaseVec3d[R](prim, 0, 3) with DataBuffer[Vec3d, R] {
  type Read = ReadDataBuffer[Vec3d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
  }
}

private[data] final class ViewVec3d[+R <: TangibleDouble](
  prim: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec3d[R](prim, off, str) with DataView[Vec3d, R] {
  type Read = ReadDataView[Vec3d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
  }
}
