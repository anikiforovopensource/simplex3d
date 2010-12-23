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

package simplex3d.data.doublem

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.doublem._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec4d[+R <: DefinedDouble](
  primitive: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec4d, R, DefinedDouble](primitive, off, str) {
  final def elemManifest = Vec4d.Manifest
  final def readManifest = Vec4d.ReadManifest
  final def components: Int = 4

  final def mkReadDataArray[P <: DefinedDouble](primitive: ReadDataArray[Vec4d#Component, P])
  :ReadDataArray[Vec4d, P] = {
    (primitive.rawType match {
      case UByte => new impl.ArrayVec4dUByte(primitive.asInstanceOf[ArrayRDoubleUByte])
      case RFloat => new impl.ArrayVec4dRFloat(primitive.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec4d(primitive)
    }).asInstanceOf[ReadDataArray[Vec4d, P]]
  }
  final def mkReadDataBuffer[P <: DefinedDouble](primitive: ReadDataBuffer[Vec4d#Component, P])
  :ReadDataBuffer[Vec4d, P] = {
    (primitive.rawType match {
      case UByte => new impl.BufferVec4dUByte(primitive.asInstanceOf[BufferRDoubleUByte])
      case RFloat => new impl.BufferVec4dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec4d(primitive)
    }).asInstanceOf[ReadDataBuffer[Vec4d, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    primitive: ReadDataBuffer[Vec4d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec4d, P] = {
    (primitive.rawType match {
      case UByte => new impl.ViewVec4dUByte(primitive.asInstanceOf[BufferRDoubleUByte], off, str)
      case RFloat => new impl.ViewVec4dRFloat(primitive.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec4d(primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec4d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec4d[+R <: DefinedDouble](
  primitive: ReadDataArray[RDouble, R]
) extends BaseVec4d[R](primitive, 0, 4) with DataArray[Vec4d, R] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class BufferVec4d[+R <: DefinedDouble](
  primitive: ReadDataBuffer[RDouble, R]
) extends BaseVec4d[R](primitive, 0, 4) with DataBuffer[Vec4d, R] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class ViewVec4d[+R <: DefinedDouble](
  primitive: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec4d[R](primitive, off, str) with DataView[Vec4d, R] {
  def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}
