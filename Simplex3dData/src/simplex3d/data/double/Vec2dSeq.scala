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
import scala.annotation.unchecked._
import simplex3d.math.doublex._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec2d[+R <: DefinedDouble](
  prim: ReadContiguous[RDouble, R], off: Int, str: Int
) extends CompositeSeq[Vec2d, R, DefinedDouble](prim, off, str) {
  final def metaManifest = Vec2d.Manifest
  final def readManifest = Vec2d.ReadManifest
  final def components: Int = 2

  final def mkReadDataArray[P <: DefinedDouble](prim: ReadDataArray[Vec2d#Component, P])
  :ReadDataArray[Vec2d, P] = {
    (prim.rawType match {
      case RFloat => new impl.ArrayVec2dRFloat(prim.asInstanceOf[ArrayRDoubleRFloat])
      case _ => new ArrayVec2d(prim)
    }).asInstanceOf[ReadDataArray[Vec2d, P]]
  }
  final def mkReadDataBuffer[P <: DefinedDouble](prim: ReadDataBuffer[Vec2d#Component, P])
  :ReadDataBuffer[Vec2d, P] = {
    (prim.rawType match {
      case RFloat => new impl.BufferVec2dRFloat(prim.asInstanceOf[BufferRDoubleRFloat])
      case _ => new BufferVec2d(prim)
    }).asInstanceOf[ReadDataBuffer[Vec2d, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedDouble](
    prim: ReadDataBuffer[Vec2d#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec2d, P] = {
    (prim.rawType match {
      case RFloat => new impl.ViewVec2dRFloat(prim.asInstanceOf[BufferRDoubleRFloat], off, str)
      case _ => new ViewVec2d(prim, off, str)
    }).asInstanceOf[ReadDataView[Vec2d, P]]
  }

  final override def mkSerializableInstance() = new CompositeRDouble(components)
}

private[data] final class ArrayVec2d[+R <: DefinedDouble](
  prim: ReadDataArray[RDouble, R]
) extends BaseVec2d[R](prim, 0, 2) with DataArray[Vec2d, R] {
  type Read = ReadDataArray[Vec2d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      primitives(j),
      primitives(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    primitives(j) = v.x
    primitives(j + 1) = v.y
  }
}

private[data] final class BufferVec2d[+R <: DefinedDouble](
  prim: ReadDataBuffer[RDouble, R]
) extends BaseVec2d[R](prim, 0, 2) with DataBuffer[Vec2d, R] {
  type Read = ReadDataBuffer[Vec2d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec2d = {
    val j = i*2
    ConstVec2d(
      primitives(j),
      primitives(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = i*2
    primitives(j) = v.x
    primitives(j + 1) = v.y
  }
}

private[data] final class ViewVec2d[+R <: DefinedDouble](
  prim: ReadDataBuffer[RDouble, R], off: Int, str: Int
) extends BaseVec2d[R](prim, off, str) with DataView[Vec2d, R] {
  type Read = ReadDataView[Vec2d, R @uncheckedVariance]

  def apply(i: Int) :ConstVec2d = {
    val j = offset + i*stride
    ConstVec2d(
      primitives(j),
      primitives(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2d) {
    val j = offset + i*stride
    primitives(j) = v.x
    primitives(j + 1) = v.y
  }
}
