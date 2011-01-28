/*
 * Simplex3d, FloatData module
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

package simplex3d.data.float
package impl

import java.nio._
import simplex3d.math.floatx._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec3f RFloat
private[data] final class ArrayVec3fRFloat(
  prim: ArrayRFloatRFloat
) extends BaseVec3f[RFloat](prim, 0, 3) with DataArray[Vec3f, RFloat] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3fRFloat(
  prim: BufferRFloatRFloat
) extends BaseVec3f[RFloat](prim, 0, 3) with DataBuffer[Vec3f, RFloat] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class ViewVec3fRFloat(
  prim: BufferRFloatRFloat, off: Int, str: Int
) extends BaseVec3f[RFloat](prim, off, str) with DataView[Vec3f, RFloat] {
  def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}


// Vec3f UByte
private[data] final class ArrayVec3fUByte(
  prim: ArrayRFloatUByte
) extends BaseVec3f[UByte](prim, 0, 3) with DataArray[Vec3f, UByte] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3fUByte(
  prim: BufferRFloatUByte
) extends BaseVec3f[UByte](prim, 0, 3) with DataBuffer[Vec3f, UByte] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class ViewVec3fUByte(
  prim: BufferRFloatUByte, off: Int, str: Int
) extends BaseVec3f[UByte](prim, off, str) with DataView[Vec3f, UByte] {
  def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}
