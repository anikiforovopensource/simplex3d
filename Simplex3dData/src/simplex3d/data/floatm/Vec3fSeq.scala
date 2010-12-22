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

import java.nio._
import scala.annotation.unchecked._
import simplex3d.math.floatm._
import simplex3d.buffer._
import RawType._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[buffer] abstract class BaseVec3f[+R <: DefinedFloat](
  primitive: ReadContiguous[RFloat, R], off: Int, str: Int
) extends CompositeSeq[Vec3f, R, DefinedFloat](primitive, off, str) {
  final def elemManifest = Vec3f.Manifest
  final def readManifest = Vec3f.ReadManifest
  final def components: Int = 3

  final def mkReadDataArray[P <: DefinedFloat](primitive: ReadDataArray[Vec3f#Component, P])
  :ReadDataArray[Vec3f, P] = {
    (primitive.rawType match {
      case UByte => new impl.ArrayVec3fUByte(primitive.asInstanceOf[ArrayRFloatUByte])
      case RFloat => new impl.ArrayVec3fRFloat(primitive.asInstanceOf[ArrayRFloatRFloat])
      case _ => new ArrayVec3f(primitive)
    }).asInstanceOf[ReadDataArray[Vec3f, P]]
  }
  final def mkReadDataBuffer[P <: DefinedFloat](primitive: ReadDataBuffer[Vec3f#Component, P])
  :ReadDataBuffer[Vec3f, P] = {
    (primitive.rawType match {
      case UByte => new impl.BufferVec3fUByte(primitive.asInstanceOf[BufferRFloatUByte])
      case RFloat => new impl.BufferVec3fRFloat(primitive.asInstanceOf[BufferRFloatRFloat])
      case _ => new BufferVec3f(primitive)
    }).asInstanceOf[ReadDataBuffer[Vec3f, P]]
  }
  protected final def mkReadDataViewInstance[P <: DefinedFloat](
    primitive: ReadDataBuffer[Vec3f#Component, P], off: Int, str: Int
  ) :ReadDataView[Vec3f, P] = {
    (primitive.rawType match {
      case UByte => new impl.ViewVec3fUByte(primitive.asInstanceOf[BufferRFloatUByte], off, str)
      case RFloat => new impl.ViewVec3fRFloat(primitive.asInstanceOf[BufferRFloatRFloat], off, str)
      case _ => new ViewVec3f(primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec3f, P]]
  }

  final override def mkSerializableInstance() = new CompositeRFloat(components)
}

private[buffer] final class ArrayVec3f[+R <: DefinedFloat](
  primitive: ReadDataArray[RFloat, R]
) extends BaseVec3f[R](primitive, 0, 3) with DataArray[Vec3f, R] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class BufferVec3f[+R <: DefinedFloat](
  primitive: ReadDataBuffer[RFloat, R]
) extends BaseVec3f[R](primitive, 0, 3) with DataBuffer[Vec3f, R] {
  def apply(i: Int) :ConstVec3f = {
    val j = i*3
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = i*3
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}

private[buffer] final class ViewVec3f[+R <: DefinedFloat](
  primitive: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseVec3f[R](primitive, off, str) with DataView[Vec3f, R] {
  def apply(i: Int) :ConstVec3f = {
    val j = offset + i*stride
    ConstVec3f(
      backing(j),
      backing(j + 1),
      backing(j + 2)
    )
  }
  def update(i: Int, v: ReadVec3f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
    backing(j + 2) = v.z
  }
}
