/*
 * Simplex3d, FloatData module
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

package simplex3d.data.floatm

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.floatm._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec4f[+R <: DefinedFloat](
  primitive: ReadContiguous[RFloat, R], off: Int, str: Int
) extends CompositeSeq[Vec4f, R, DefinedFloat](primitive, off, str) {
  final def elemManifest = Vec4f.Manifest
  final def readManifest = Vec4f.ReadManifest
  final def components: Int = 4

  final def mkReadDataArray[P <: DefinedFloat](primitive: ReadDataArray[Vec4f#Component, P])
  :ReadDataArray[Vec4f, P] = {
    (primitive.rawType match {
      case UByte => new impl.ArrayVec4fUByte(primitive.asInstanceOf[ArrayRFloatUByte])
      case RFloat => new impl.ArrayVec4fRFloat(primitive.asInstanceOf[ArrayRFloatRFloat])
      case _ => new ArrayVec4f(primitive)
    }).asInstanceOf[ReadDataArray[Vec4f, P]]
  }
  final def mkReadDataBuffer[P <: DefinedFloat](primitive: ReadDataBuffer[Vec4f#Component, P])
  :ReadDataBuffer[Vec4f, P] = {
    (primitive.rawType match {
      case UByte => new impl.BufferVec4fUByte(primitive.asInstanceOf[BufferRFloatUByte])
      case RFloat => new impl.BufferVec4fRFloat(primitive.asInstanceOf[BufferRFloatRFloat])
      case _ => new BufferVec4f(primitive)
    }).asInstanceOf[ReadDataBuffer[Vec4f, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedFloat](
    primitive: ReadDataBuffer[Vec4f#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec4f, P] = {
    (primitive.rawType match {
      case UByte => new impl.ViewVec4fUByte(primitive.asInstanceOf[BufferRFloatUByte], off, str)
      case RFloat => new impl.ViewVec4fRFloat(primitive.asInstanceOf[BufferRFloatRFloat], off, str)
      case _ => new ViewVec4f(primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec4f, P]]
  }

  final override def mkSerializableInstance() = new CompositeRFloat(components)
}

private[data] final class ArrayVec4f[+R <: DefinedFloat](
  primitive: ReadDataArray[RFloat, R]
) extends BaseVec4f[R](primitive, 0, 4) with DataArray[Vec4f, R] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class BufferVec4f[+R <: DefinedFloat](
  primitive: ReadDataBuffer[RFloat, R]
) extends BaseVec4f[R](primitive, 0, 4) with DataBuffer[Vec4f, R] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class ViewVec4f[+R <: DefinedFloat](
  primitive: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseVec4f[R](primitive, off, str) with DataView[Vec4f, R] {
  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}
