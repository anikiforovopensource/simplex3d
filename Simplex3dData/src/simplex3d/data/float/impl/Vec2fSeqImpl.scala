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

package simplex3d.data.float
package impl

import java.nio._
import simplex3d.math.floatx._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2f RFloat
private[data] final class ArrayVec2fRFloat(
  primitive: ArrayRFloatRFloat
) extends BaseVec2f[RFloat](primitive, 0, 2) with DataArray[Vec2f, RFloat] {
  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[data] final class BufferVec2fRFloat(
  primitive: BufferRFloatRFloat
) extends BaseVec2f[RFloat](primitive, 0, 2) with DataBuffer[Vec2f, RFloat] {
  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[data] final class ViewVec2fRFloat(
  primitive: BufferRFloatRFloat, off: Int, str: Int
) extends BaseVec2f[RFloat](primitive, off, str) with DataView[Vec2f, RFloat] {
  def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}
