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
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec2i[+R <: DefinedInt](
  prim: ReadContiguous[SInt, R], off: Int, str: Int
) extends CompositeSeq[Vec2i, R, DefinedInt](prim, off, str) {
  final def metaManifest = Vec2i.Manifest
  final def readManifest = Vec2i.ReadManifest
  final def components: Int = 2

  final def mkReadDataArray[P <: DefinedInt](prim: ReadDataArray[Vec2i#Component, P])
  :ReadDataArray[Vec2i, P] = {
    (prim.rawType match {
      case UShort => new impl.ArrayVec2iUShort(prim.asInstanceOf[ArraySIntUShort])
      case UInt => new impl.ArrayVec2iUInt(prim.asInstanceOf[ArraySIntUInt])
      case _ => new ArrayVec2i(prim)
    }).asInstanceOf[ReadDataArray[Vec2i, P]]
  }
  final def mkReadDataBuffer[P <: DefinedInt](prim: ReadDataBuffer[Vec2i#Component, P])
  :ReadDataBuffer[Vec2i, P] = {
    (prim.rawType match {
      case UShort => new impl.BufferVec2iUShort(prim.asInstanceOf[BufferSIntUShort])
      case UInt => new impl.BufferVec2iUInt(prim.asInstanceOf[BufferSIntUInt])
      case _ => new BufferVec2i(prim)
    }).asInstanceOf[ReadDataBuffer[Vec2i, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedInt](
    prim: ReadDataBuffer[Vec2i#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec2i, P] = new ViewVec2i(prim, off, str)

  final override def mkSerializableInstance() = new CompositeSInt(components)
}

private[data] final class ArrayVec2i[+R <: DefinedInt](
  prim: ReadDataArray[SInt, R]
) extends BaseVec2i[R](prim, 0, 2) with DataArray[Vec2i, R] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[data] final class BufferVec2i[+R <: DefinedInt](
  prim: ReadDataBuffer[SInt, R]
) extends BaseVec2i[R](prim, 0, 2) with DataBuffer[Vec2i, R] {
  def apply(i: Int) :ConstVec2i = {
    val j = i*2
    ConstVec2i(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = i*2
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}

private[data] final class ViewVec2i[+R <: DefinedInt](
  prim: ReadDataBuffer[SInt, R], off: Int, str: Int
) extends BaseVec2i[R](prim, off, str) with DataView[Vec2i, R] {
  def apply(i: Int) :ConstVec2i = {
    val j = offset + i*stride
    ConstVec2i(
      primitive(j),
      primitive(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2i) {
    val j = offset + i*stride
    primitive(j) = v.x
    primitive(j + 1) = v.y
  }
}
