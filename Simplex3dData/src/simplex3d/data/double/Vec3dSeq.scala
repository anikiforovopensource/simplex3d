/*
 * Simplex3d, DoubleData module
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
  prim: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec3d, R, DefinedDouble](prim, off, str) {
  final def elemManifest = Vec3d.Manifest
  final def readManifest = Vec3d.ReadManifest
  final def components: Int = 3

  final def mkReadDataArray[P <: DefinedDouble](prim: ReadDataArray[Vec3d#Component, P])
  :ReadDataArray[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new impl.ArrayVec3dUByte(prim.asInstanceOf[ArrayRDoubleUByte])
      case RFloat => new impl.ArrayVec3dRFloat(prim.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec3d(prim)
    }).asInstanceOf[ReadDataArray[Vec3d, P]]
  }
  final def mkReadDataBuffer[P <: DefinedDouble](prim: ReadDataBuffer[Vec3d#Component, P])
  :ReadDataBuffer[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new impl.BufferVec3dUByte(prim.asInstanceOf[BufferRDoubleUByte])
      case RFloat => new impl.BufferVec3dRFloat(prim.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec3d(prim)
    }).asInstanceOf[ReadDataBuffer[Vec3d, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    prim: ReadDataBuffer[Vec3d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3d, P] = {
    (prim.rawType match {
      case UByte => new impl.ViewVec3dUByte(prim.asInstanceOf[BufferRDoubleUByte], off, str)
      case RFloat => new impl.ViewVec3dRFloat(prim.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec3d(prim, off, str)
    }).asInstanceOf[ReadDataView[Vec3d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec3d[+R <: DefinedDouble](
  prim: ReadDataArray[RDouble, R]
) extends BaseVec3d[R](prim, 0, 3) with DataArray[Vec3d, R] {
  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[data] final class BufferVec3d[+R <: DefinedDouble](
  prim: ReadDataBuffer[RDouble, R]
) extends BaseVec3d[R](prim, 0, 3) with DataBuffer[Vec3d, R] {
  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[data] final class ViewVec3d[+R <: DefinedDouble](
  prim: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec3d[R](prim, off, str) with DataView[Vec3d, R] {
  def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}
