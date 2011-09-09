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
// Vec3d RFloat
private[data] final class ArrayVec3dRFloat(
  prim: ArrayRDoubleRFloat
) extends BaseVec3d[RFloat](prim, 0, 3) with DataArray[Vec3d, RFloat] {
  type Read = ReadDataArray[Vec3d, RFloat]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3dRFloat(
  prim: BufferRDoubleRFloat
) extends BaseVec3d[RFloat](prim, 0, 3) with DataBuffer[Vec3d, RFloat] {
  type Read = ReadDataBuffer[Vec3d, RFloat]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class ViewVec3dRFloat(
  prim: BufferRDoubleRFloat, off: Int, str: Int
) extends BaseVec3d[RFloat](prim, off, str) with DataView[Vec3d, RFloat] {
  type Read = ReadDataView[Vec3d, RFloat]

  def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}


// Vec3d UByte
private[data] final class ArrayVec3dUByte(
  prim: ArrayRDoubleUByte
) extends BaseVec3d[UByte](prim, 0, 3) with DataArray[Vec3d, UByte] {
  type Read = ReadDataArray[Vec3d, UByte]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class BufferVec3dUByte(
  prim: BufferRDoubleUByte
) extends BaseVec3d[UByte](prim, 0, 3) with DataBuffer[Vec3d, UByte] {
  type Read = ReadDataBuffer[Vec3d, UByte]

  def apply(i: Int) :ConstVec3d = {
    val j = i*3
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = i*3
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}

private[data] final class ViewVec3dUByte(
  prim: BufferRDoubleUByte, off: Int, str: Int
) extends BaseVec3d[UByte](prim, off, str) with DataView[Vec3d, UByte] {
  type Read = ReadDataView[Vec3d, UByte]

  def apply(i: Int) :ConstVec3d = {
    val j = offset + i*stride
    ConstVec3d(
      prim(j),
      prim(j + 1),
      prim(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3d) {
    val j = offset + i*stride
    prim(j) = v.x
    prim(j + 1) = v.y
    prim(j + 2) = v.z
  }
}
