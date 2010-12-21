/*
 * Simplex3d, FloatBuffer module
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

package simplex3d.buffer.floatm
package impl

import java.nio._
import simplex3d.math.floatm._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec4f RFloat
private[buffer] final class ArrayVec4fRFloat(
  primitive: ArrayRFloatRFloat
) extends BaseVec4f[RFloat](primitive, 0, 4) with DataArray[Vec4f, RFloat] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[buffer] final class BufferVec4fRFloat(
  primitive: BufferRFloatRFloat
) extends BaseVec4f[RFloat](primitive, 0, 4) with DataBuffer[Vec4f, RFloat] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[buffer] final class ViewVec4fRFloat(
  primitive: BufferRFloatRFloat, off: Int, str: Int
) extends BaseVec4f[RFloat](primitive, off, str) with DataView[Vec4f, RFloat] {
  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}


// Vec4f UByte
private[buffer] final class ArrayVec4fUByte(
  primitive: ArrayRFloatUByte
) extends BaseVec4f[UByte](primitive, 0, 4) with DataArray[Vec4f, UByte] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[buffer] final class BufferVec4fUByte(
  primitive: BufferRFloatUByte
) extends BaseVec4f[UByte](primitive, 0, 4) with DataBuffer[Vec4f, UByte] {
  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[buffer] final class ViewVec4fUByte(
  primitive: BufferRFloatUByte, off: Int, str: Int
) extends BaseVec4f[UByte](primitive, off, str) with DataView[Vec4f, UByte] {
  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}
