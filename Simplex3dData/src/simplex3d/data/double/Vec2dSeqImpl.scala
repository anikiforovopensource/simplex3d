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
import simplex3d.math.doublex._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2d RFloat
private[data] final class ArrayVec2dRFloat(
  prim: ArrayRDoubleRFloat
) extends BaseVec2d[RFloat](prim, 0, 2) with DataArray[Vec2d, RFloat] {
  type Read = ReadDataArray[Vec2d, RFloat]

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}

private[data] final class BufferVec2dRFloat(
  prim: BufferRDoubleRFloat
) extends BaseVec2d[RFloat](prim, 0, 2) with DataBuffer[Vec2d, RFloat] {
  type Read = ReadDataBuffer[Vec2d, RFloat]

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}

private[data] final class ViewVec2dRFloat(
  prim: BufferRDoubleRFloat, off: Int, str: Int
) extends BaseVec2d[RFloat](prim, off, str) with DataView[Vec2d, RFloat] {
  type Read = ReadDataView[Vec2d, RFloat]

  def apply(i: Int) :ConstVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      prim(j),
      prim(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
  }
}
