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
package impl

import java.nio._
import simplex3d.math.doublem._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec4d RFloat
private[data] final class ArrayVec4dRFloat(
  primitive: ArrayRDoubleRFloat
) extends BaseVec4d[RFloat](primitive, 0, 4) with DataArray[Vec4d, RFloat] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class BufferVec4dRFloat(
  primitive: BufferRDoubleRFloat
) extends BaseVec4d[RFloat](primitive, 0, 4) with DataBuffer[Vec4d, RFloat] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class ViewVec4dRFloat(
  primitive: BufferRDoubleRFloat, off: Int, str: Int
) extends BaseVec4d[RFloat](primitive, off, str) with DataView[Vec4d, RFloat] {
  def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}


// Vec4d UByte
private[data] final class ArrayVec4dUByte(
  primitive: ArrayRDoubleUByte
) extends BaseVec4d[UByte](primitive, 0, 4) with DataArray[Vec4d, UByte] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class BufferVec4dUByte(
  primitive: BufferRDoubleUByte
) extends BaseVec4d[UByte](primitive, 0, 4) with DataBuffer[Vec4d, UByte] {
  def apply(i: Int) :ConstVec4d = {
    val j = i*4
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class ViewVec4dUByte(
  primitive: BufferRDoubleUByte, off: Int, str: Int
) extends BaseVec4d[UByte](primitive, off, str) with DataView[Vec4d, UByte] {
  def apply(i: Int) :ConstVec4d = {
    val j = offset + i*stride
    ConstVec4d(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4d) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}
