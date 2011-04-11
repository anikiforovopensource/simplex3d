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
package impl

import java.nio._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] final class ArrayVec2iUShort(
  prim: ArraySIntUShort
) extends BaseVec2i[UShort](prim, 0, 2) with DataArray[Vec2i, UShort] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}

private[data] final class BufferVec2iUShort(
  prim: BufferSIntUShort
) extends BaseVec2i[UShort](prim, 0, 2) with DataBuffer[Vec2i, UShort] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}


// UInt
private[data] final class ArrayVec2iUInt(
  prim: ArraySIntUInt
) extends BaseVec2i[UInt](prim, 0, 2) with DataArray[Vec2i, UInt] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}

private[data] final class BufferVec2iUInt(
  prim: BufferSIntUInt
) extends BaseVec2i[UInt](prim, 0, 2) with DataBuffer[Vec2i, UInt] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}
