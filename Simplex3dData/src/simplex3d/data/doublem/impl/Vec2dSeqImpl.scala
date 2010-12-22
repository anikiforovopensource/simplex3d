/*
 * Simplex3d, DoubleBuffer module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dBuffer.
 *
 * Simplex3dBuffer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dBuffer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.buffer.doublem
package impl

import java.nio._
import simplex3d.math.doublem._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec2d RFloat
private[buffer] final class ArrayVec2dRFloat(
  primitive: ArrayRDoubleRFloat
) extends BaseVec2d[RFloat](primitive, 0, 2) with DataArray[Vec2d, RFloat] {
  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[buffer] final class BufferVec2dRFloat(
  primitive: BufferRDoubleRFloat
) extends BaseVec2d[RFloat](primitive, 0, 2) with DataBuffer[Vec2d, RFloat] {
  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[buffer] final class ViewVec2dRFloat(
  primitive: BufferRDoubleRFloat, off: Int, str: Int
) extends BaseVec2d[RFloat](primitive, off, str) with DataView[Vec2d, RFloat] {
  def apply(i: Int) :ConstVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}
