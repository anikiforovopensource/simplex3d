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

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.floatx._
import simplex3d.data._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[data] abstract class BaseVec3f[+R <: DefinedFloat](
  prim: ReadContiguous[RFloat, R], off: Int, str: Int
) extends CompositeSeq[Vec3f, R, DefinedFloat](prim, off, str) {
  final def metaManifest = Vec3f.Manifest
  final def readManifest = Vec3f.ReadManifest
  final def components: Int = 3

  final def mkReadDataArray[P <: DefinedFloat](prim: ReadDataArray[Vec3f#Component, P])
  :ReadDataArray[Vec3f, P] = {
    (prim.rawType match {
      case UByte => new impl.ArrayVec3fUByte(prim.asInstanceOf[ArrayRFloatUByte])
      case RFloat => new impl.ArrayVec3fRFloat(prim.asInstanceOf[ArrayRFloatRFloat])
      case _ => new ArrayVec3f(prim)
    }).asInstanceOf[ReadDataArray[Vec3f, P]]
  }
  final def mkReadDataBuffer[P <: DefinedFloat](prim: ReadDataBuffer[Vec3f#Component, P])
  :ReadDataBuffer[Vec3f, P] = {
    (prim.rawType match {
      case UByte => new impl.BufferVec3fUByte(prim.asInstanceOf[BufferRFloatUByte])
      case RFloat => new impl.BufferVec3fRFloat(prim.asInstanceOf[BufferRFloatRFloat])
      case _ => new BufferVec3f(prim)
    }).asInstanceOf[ReadDataBuffer[Vec3f, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedFloat](
    prim: ReadDataBuffer[Vec3f#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3f, P] = {
    (prim.rawType match {
      case UByte => new impl.ViewVec3fUByte(prim.asInstanceOf[BufferRFloatUByte], off, str)
      case RFloat => new impl.ViewVec3fRFloat(prim.asInstanceOf[BufferRFloatRFloat], off, str)
      case _ => new ViewVec3f(prim, off, str)
    }).asInstanceOf[ReadDataView[Vec3f, P]]
  }

  final override def mkSerializableInstance() = new CompositeRFloat(components)
}

private[data] final class ArrayVec3f[+R <: DefinedFloat](
  prim: ReadDataArray[RFloat, R]
) extends BaseVec3f[R](prim, 0, 3) with DataArray[Vec3f, R] {
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

private[data] final class BufferVec3f[+R <: DefinedFloat](
  prim: ReadDataBuffer[RFloat, R]
) extends BaseVec3f[R](prim, 0, 3) with DataBuffer[Vec3f, R] {
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

private[data] final class ViewVec3f[+R <: DefinedFloat](
  prim: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseVec3f[R](prim, off, str) with DataView[Vec3f, R] {
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
