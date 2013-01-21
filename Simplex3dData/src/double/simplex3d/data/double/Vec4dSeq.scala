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
import RawEnum._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec4d[+R <: TangibleDouble](
  prim: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec4d, R, TangibleDouble](prim, off, str) {
  final def formatTag = Vec4d.Tag
  final def accessorTag = Vec4d.Tag
  final def components: Int = 4

  final def mkReadDataArray[P <: TangibleDouble](prim: ReadDataArray[Vec4d#Component, P])
  :ReadDataArray[Vec4d, P] = {
    (prim.rawEnum match {
      case UByte => new ArrayVec4dUByte(prim.asInstanceOf[ArrayRDoubleUByte])
      case RFloat => new ArrayVec4dRFloat(prim.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec4d(prim)
    }).asInstanceOf[ReadDataArray[Vec4d, P]]
  }
  final def mkReadDataBuffer[P <: TangibleDouble](prim: ReadDataBuffer[Vec4d#Component, P])
  :ReadDataBuffer[Vec4d, P] = {
    (prim.rawEnum match {
      case UByte => new BufferVec4dUByte(prim.asInstanceOf[BufferRDoubleUByte])
      case RFloat => new BufferVec4dRFloat(prim.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec4d(prim)
    }).asInstanceOf[ReadDataBuffer[Vec4d, P]]
  }
  protected final def mkReadDataViewInstance[P <: TangibleDouble](
    prim: ReadDataBuffer[Vec4d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec4d, P] = {
    (prim.rawEnum match {
      case UByte => new ViewVec4dUByte(prim.asInstanceOf[BufferRDoubleUByte], off, str)
      case RFloat => new ViewVec4dRFloat(prim.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec4d(prim, off, str)
    }).asInstanceOf[ReadDataView[Vec4d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec4d[+R <: TangibleDouble](
  prim: ReadDataArray[RDouble, R]
) extends BaseVec4d[R](prim, 0, 4) with DataArray[Vec4d, R] {
  type Read = ReadDataArray[Vec4d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2),
      primitives(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
    primitives(j + 3) = v.w
  }
}

private[data] final class BufferVec4d[+R <: TangibleDouble](
  prim: ReadDataBuffer[RDouble, R]
) extends BaseVec4d[R](prim, 0, 4) with DataBuffer[Vec4d, R] {
  type Read = ReadDataBuffer[Vec4d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2),
      primitives(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
    primitives(j + 3) = v.w
  }
}

private[data] final class ViewVec4d[+R <: TangibleDouble](
  prim: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec4d[R](prim, off, str) with DataView[Vec4d, R] {
  type Read = ReadDataView[Vec4d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      primitives(j),
      primitives(j + 1),
      primitives(j + 2),
      primitives(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    primitives(j) = v.x
    primitives(j + 1) = v.y
    primitives(j + 2) = v.z
    primitives(j + 3) = v.w
  }
}
