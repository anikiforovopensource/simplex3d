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
// Vec3f RFloat
private[buffer] final class ArrayVec3fRFloat(
  primitive: ArrayRFloatRFloat
) extends BaseVec3f[RFloat](primitive, 0, 3) with DataArray[Vec3f, RFloat] {
  def this() = this(new ArrayRFloatRFloat)

  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3fRFloat(
  primitive: BufferRFloatRFloat
) extends BaseVec3f[RFloat](primitive, 0, 3) with DataBuffer[Vec3f, RFloat] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3fRFloat(
  primitive: BufferRFloatRFloat, off: Int, str: Int
) extends BaseVec3f[RFloat](primitive, off, str) with DataView[Vec3f, RFloat] {
  def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}


// Vec3f UByte
private[buffer] final class ArrayVec3fUByte(
  primitive: ArrayRFloatUByte
) extends BaseVec3f[UByte](primitive, 0, 3) with DataArray[Vec3f, UByte] {
  def this() = this(new ArrayRFloatUByte)

  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3fUByte(
  primitive: BufferRFloatUByte
) extends BaseVec3f[UByte](primitive, 0, 3) with DataBuffer[Vec3f, UByte] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3fUByte(
  primitive: BufferRFloatUByte, off: Int, str: Int
) extends BaseVec3f[UByte](primitive, off, str) with DataView[Vec3f, UByte] {
  def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
  }
}
