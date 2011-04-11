/*
 * Simplex3d, CoreData module
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

package simplex3d.data

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec4i[+R <: DefinedInt](
  prim: ReadContiguous[SInt, R], off: Int, str: Int
) extends CompositeSeq[Vec4i, R, DefinedInt](prim, off, str) {
  final def metaManifest = Vec4i.Manifest
  final def readManifest = Vec4i.ReadManifest
  final def components: Int = 4

  final def mkReadDataArray[P <: DefinedInt](prim: ReadDataArray[Vec4i#Component, P])
  :ReadDataArray[Vec4i, P] = new ArrayVec4i(prim)
  final def mkReadDataBuffer[P <: DefinedInt](prim: ReadDataBuffer[Vec4i#Component, P])
  :ReadDataBuffer[Vec4i, P] = new BufferVec4i(prim)
  protected final def mkReadDataViewInstance[P <: DefinedInt](
    prim: ReadDataBuffer[Vec4i#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec4i, P] = new ViewVec4i(prim, off, str)

  final override def mkSerializableInstance() = new CompositeSInt(components)
}

private[data] final class ArrayVec4i[+R <: DefinedInt](
  prim: ReadDataArray[SInt, R]
) extends BaseVec4i[R](prim, 0, 4) with DataArray[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class BufferVec4i[+R <: DefinedInt](
  prim: ReadDataBuffer[SInt, R]
) extends BaseVec4i[R](prim, 0, 4) with DataBuffer[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = i*4
    ConstVec4i(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = i*4
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}

private[data] final class ViewVec4i[+R <: DefinedInt](
  prim: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseVec4i[R](prim, off, str) with DataView[Vec4i, R] {
  def apply(i: Int) :ConstVec4i = {
    val j = offset + i*stride
    ConstVec4i(
      primitive(j),
      primitive(j + 1),
      primitive(j + 2),
      primitive(j + 3)
    )
  }
  def update(i: Int, v: ReadVec4i) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
    primitive(j + 2) = v.z
    primitive(j + 3) = v.w
  }
}
