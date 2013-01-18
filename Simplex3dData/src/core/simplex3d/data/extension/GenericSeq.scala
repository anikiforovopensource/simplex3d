/*
 * Simplex3dData - Core Module
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

package simplex3d.data.extension

import java.nio._
import scala.language.existentials
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class GenericSeq[F <: CompositeFormat, +R <: Raw, B <: Raw with Tangible](
  adapter: DataAdapter[F, B], prim: ReadContiguous[F#Component, R], off: Int, str: Int
) extends CompositeSeq[F, R, B](prim, off, str) {
  final def formatTag = adapter.formatTag
  final def accessorTag = adapter.accessorTag
  final def components: Int = adapter.components

  def apply(i: Int) :F#Accessor#Const = adapter.apply(primitives, offset + i*stride)
  def update(i: Int, v: F#Accessor#Read) { adapter.update(primitives, offset + i*stride, v) }

  def mkReadDataArray[P <: B](primitives: ReadDataArray[F#Component, P])
  :ReadDataArray[F, P] = adapter.mkReadDataArray(primitives)
  def mkReadDataBuffer[P <: B](primitives: ReadDataBuffer[F#Component, P])
  :ReadDataBuffer[F, P] = adapter.mkReadDataBuffer(primitives)
  protected def mkReadDataViewInstance[P <: B](primitives: ReadDataBuffer[F#Component, P], offset: Int, stride: Int)
  :ReadDataView[F, P] = adapter.mkReadDataViewInstance(primitives, offset, stride)

  protected[data] final override def mkSerializableInstance() = new SerializableGeneric(adapter)
}

private[data] final class SerializableGeneric(val adapter: DataAdapter[_, _]) extends SerializableComposite {
  protected def toReadDataArray(
    primitives: ReadDataArray[_ <: PrimitiveFormat, _]
  ): ReadDataArray[_ <: CompositeFormat, _] = {
    type F = T forSome { type T <: CompositeFormat }
    val primitiveArray = primitives.asInstanceOf[ReadDataArray[F#Component, Raw with Tangible]]
    adapter.asInstanceOf[DataAdapter[F, Raw with Tangible]].mkReadDataArray(primitiveArray)
  }
}

final class GenericArray[F <: CompositeFormat, +R <: Raw, B <: Raw with Tangible](
  adapter: DataAdapter[F, B], prim: ReadDataArray[F#Component, R]
) extends GenericSeq[F, R, B](adapter, prim, 0, adapter.components) with DataArray[F, R] {
  type Read = ReadDataArray[F, R @uncheckedVariance]
}

final class GenericBuffer[F <: CompositeFormat, +R <: Raw, B <: Raw with Tangible](
  adapter: DataAdapter[F, B], prim: ReadDataBuffer[F#Component, R]
) extends GenericSeq[F, R, B](adapter, prim, 0, adapter.components) with DataBuffer[F, R] {
  type Read = ReadDataBuffer[F, R @uncheckedVariance]
}

final class GenericView[F <: CompositeFormat, +R <: Raw, B <: Raw with Tangible](
  adapter: DataAdapter[F, B], prim: ReadDataBuffer[F#Component, R], off: Int, str: Int
) extends GenericSeq[F, R, B](adapter, prim, off, str) with DataView[F, R] {
  type Read = ReadDataView[F, R @uncheckedVariance]
}
