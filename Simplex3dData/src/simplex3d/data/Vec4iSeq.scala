/*
 * Simplex3d, CoreData module
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

package simplex3d.data

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec4i[+R <: DefinedInt](
  primitive: ReadContiguous[SInt, R], off: Int, str: Int
) extends CompositeSeq[Vec4i, R, DefinedInt](primitive, off, str) {
  final def elemManifest = Vec4i.Manifest
  final def readManifest = Vec4i.ReadManifest
  final def components: Int = 4

  final def mkReadDataArray[P <: DefinedInt](primitive: ReadDataArray[Vec4i#Component, P])
  :ReadDataArray[Vec4i, P] = new ArrayVec4i(primitive)
  final def mkReadDataBuffer[P <: DefinedInt](primitive: ReadDataBuffer[Vec4i#Component, P])
  :ReadDataBuffer[Vec4i, P] = new BufferVec4i(primitive)
  protected final def mkReadDataViewInstance[P <: DefinedInt](
    primitive: ReadDataBuffer[Vec4i#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec4i, P] = new ViewVec4i(primitive, off, str)

  final override def mkSerializableInstance() = new CompositeSInt(components)
}

private[data] final class ArrayVec4i[+R <: DefinedInt](
  primitive: ReadDataArray[SInt, R]
) extends BaseVec4i[R](primitive, 0, 4) with DataArray[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class BufferVec4i[+R <: DefinedInt](
  primitive: ReadDataBuffer[SInt, R]
) extends BaseVec4i[R](primitive, 0, 4) with DataBuffer[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = i*4
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}

private[data] final class ViewVec4i[+R <: DefinedInt](
  primitive: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseVec4i[R](primitive, off, str) with DataView[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      backing(j),
      backing(j + 1),
      backing(j + 2),
      backing(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
    backing(j + 3) = v.w
  }
}
