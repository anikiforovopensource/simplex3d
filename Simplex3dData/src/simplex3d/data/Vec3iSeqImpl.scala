/*
 * Simplex3d, CoreData module
 * Copyright (C) 2011, Aleksey Nikiforov
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
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] final class ArrayVec3iUShort(
  prim: ArraySIntUShort
) extends BaseVec3i[UShort](prim, 0, 3) with DataArray[Vec3i, UShort] {
  type Read = ReadDataArray[Vec3i, UShort]

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3iUShort(
  prim: BufferSIntUShort
) extends BaseVec3i[UShort](prim, 0, 3) with DataBuffer[Vec3i, UShort] {
  type Read = ReadDataBuffer[Vec3i, UShort]

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}


// UInt
private[data] final class ArrayVec3iUInt(
  prim: ArraySIntUInt
) extends BaseVec3i[UInt](prim, 0, 3) with DataArray[Vec3i, UInt] {
  type Read = ReadDataArray[Vec3i, UInt]

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3iUInt(
  prim: BufferSIntUInt
) extends BaseVec3i[UInt](prim, 0, 3) with DataBuffer[Vec3i, UInt] {
  type Read = ReadDataBuffer[Vec3i, UInt]

  def apply(i: Int) :ConstVec3i = {
    val j = i*3
    ConstVec3i(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3i) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}
