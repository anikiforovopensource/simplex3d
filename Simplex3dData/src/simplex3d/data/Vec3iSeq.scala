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
private[data] abstract class BaseVec3i[+R <: DefinedInt](
  primitive: ReadContiguous[SInt, R], off: Int, str: Int
) extends CompositeSeq[Vec3i, R, DefinedInt](primitive, off, str) {
  final def elemManifest = Vec3i.Manifest
  final def readManifest = Vec3i.ReadManifest
  final def components: Int = 3

  final def mkReadDataArray[P <: DefinedInt](primitive: ReadDataArray[Vec3i#Component, P])
  :ReadDataArray[Vec3i, P] = new ArrayVec3i(primitive)
  final def mkReadDataBuffer[P <: DefinedInt](primitive: ReadDataBuffer[Vec3i#Component, P])
  :ReadDataBuffer[Vec3i, P] = new BufferVec3i(primitive)
  protected final def mkReadDataViewInstance[P <: DefinedInt](
    primitive: ReadDataBuffer[Vec3i#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3i, P] = new ViewVec3i(primitive, off, str)

  final override def mkSerializableInstance() = new CompositeSInt(components)
}

private[data] final class ArrayVec3i[+R <: DefinedInt](
  primitive: ReadDataArray[SInt, R]
) extends BaseVec3i[R](primitive, 0, 3) with DataArray[Vec3i, R] {
  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[data] final class BufferVec3i[+R <: DefinedInt](
  primitive: ReadDataBuffer[SInt, R]
) extends BaseVec3i[R](primitive, 0, 3) with DataBuffer[Vec3i, R] {
  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[data] final class ViewVec3i[+R <: DefinedInt](
  primitive: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseVec3i[R](primitive, off, str) with DataView[Vec3i, R] {
  def apply(i: Int) :ConstVec3i = {
    val j = offset + i*stride
    ConstVec3i(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}
