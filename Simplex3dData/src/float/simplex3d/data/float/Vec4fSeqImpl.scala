/*
 * Simplex3dData - Float Module
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

import java.nio._
import simplex3d.math.floatx._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
// Vec4f RFloat
private[data] final class ArrayVec4fRFloat(
  prim: ArrayRFloatRFloat
) extends BaseVec4f[RFloat](prim, 0, 4) with DataArray[Vec4f, RFloat] {
  type Read = ReadDataArray[Vec4f, RFloat]

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}

private[data] final class BufferVec4fRFloat(
  prim: BufferRFloatRFloat
) extends BaseVec4f[RFloat](prim, 0, 4) with DataBuffer[Vec4f, RFloat] {
  type Read = ReadDataBuffer[Vec4f, RFloat]

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}

private[data] final class ViewVec4fRFloat(
  prim: BufferRFloatRFloat, off: Int, str: Int
) extends BaseVec4f[RFloat](prim, off, str) with DataView[Vec4f, RFloat] {
  type Read = ReadDataView[Vec4f, RFloat]

  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}


// Vec4f UByte
private[data] final class ArrayVec4fUByte(
  prim: ArrayRFloatUByte
) extends BaseVec4f[UByte](prim, 0, 4) with DataArray[Vec4f, UByte] {
  type Read = ReadDataArray[Vec4f, UByte]

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}

private[data] final class BufferVec4fUByte(
  prim: BufferRFloatUByte
) extends BaseVec4f[UByte](prim, 0, 4) with DataBuffer[Vec4f, UByte] {
  type Read = ReadDataBuffer[Vec4f, UByte]

  def apply(i: Int) :ConstVec4f = {
    val j = i*4
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = i*4
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}

private[data] final class ViewVec4fUByte(
  prim: BufferRFloatUByte, off: Int, str: Int
) extends BaseVec4f[UByte](prim, off, str) with DataView[Vec4f, UByte] {
  type Read = ReadDataView[Vec4f, UByte]

  def apply(i: Int) :ConstVec4f = {
    val j = offset + i*stride
    ConstVec4f(
      prim(j),
      prim(j + 1),
      prim(j + 2),
      prim(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4f) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
    prim(j + 3) = v.w
  }
}
