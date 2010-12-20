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
private[buffer] abstract class BaseVec2f[+R <: Raw](
  primitive: ReadContiguous[RFloat, R], off: Int, str: Int
) extends CompositeSeq[Vec2f, R](primitive, off, str) {
  final def elemManifest = Vec2f.Manifest
  final def readManifest = Vec2f.ReadManifest
  final def components: Int = 2

  final def mkReadDataArray[P <: Defined](primitive: ReadDataArray[Vec2f#Component, P])
  :ReadDataArray[Vec2f, P] = {
    (primitive.rawType match {
      case RFloat => new impl.ArrayVec2fRFloat(primitive.asInstanceOf[ArrayRFloatRFloat])
      case _ => new ArrayVec2f[P](primitive)
    }).asInstanceOf[ReadDataArray[Vec2f, P]]
  }
  final def mkReadDataBuffer[P <: Defined](primitive: ReadDataBuffer[Vec2f#Component, P])
  :ReadDataBuffer[Vec2f, P] = {
    (primitive.rawType match {
      case RFloat => new impl.BufferVec2fRFloat(primitive.asInstanceOf[BufferRFloatRFloat])
      case _ => new BufferVec2f[P](primitive)
    }).asInstanceOf[ReadDataBuffer[Vec2f, P]]
  }
  final def mkReadDataView[P <: Defined](primitive: ReadDataBuffer[Vec2f#Component, P], off: Int, str: Int)
  :ReadDataView[Vec2f, P] = {
    (primitive.rawType match {
      case RFloat => new impl.ViewVec2fRFloat(primitive.asInstanceOf[BufferRFloatRFloat], off, str)
      case _ => new ViewVec2f[P](primitive, off, str)
    }).asInstanceOf[ReadDataView[Vec2f, P]]
  }

  override def mkSerializableInstance() = new SerializableFloatData(components, rawType)
}

private[buffer] final class ArrayVec2f[+R <: Raw](
  primitive: ReadDataArray[RFloat, R]
) extends BaseVec2f[R](primitive, 0, 2) with DataArray[Vec2f, R] {
  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class BufferVec2f[+R <: Raw](
  primitive: ReadDataBuffer[RFloat, R]
) extends BaseVec2f[R](primitive, 0, 2) with DataBuffer[Vec2f, R] {
  def apply(i: Int) :ConstVec2f = {
    val j = i*2
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = i*2
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}

private[buffer] final class ViewVec2f[+R <: Raw](
  primitive: ReadDataBuffer[RFloat, R], off: Int, str: Int
) extends BaseVec2f[R](primitive, off, str) with DataView[Vec2f, R] {
  def apply(i: Int) :ConstVec2f = {
    val j = offset + i*stride
    ConstVec2f(
      backing(j),
      backing(j + 1)
    )
  }
  def update(i: Int, v: ReadVec2f) {
    val j = offset + i*stride
    backing(j) = v.x
    backing(j + 1) = v.y
  }
}
