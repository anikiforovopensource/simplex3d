/*
 * Simplex3d, CoreData module
 * Copyright (C) 2010, Simplex3d Team
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
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.data._


/**
 * @author Aleksey Nikiforov (lex)
 */
sealed abstract class GenericSeq[E <: Composite, +R <: Raw, B <: Defined](
  adapter: DataAdapter[E, B], primitive: ReadContiguous[E#Component, R], off: Int, str: Int
) extends CompositeSeq[E, R, B](primitive, off, str) {
  final def elemManifest = adapter.elemManifest
  final def readManifest = adapter.readManifest
  final def components: Int = adapter.components

  def apply(i: Int) :E#Const = adapter.apply(backing, offset + i*stride)
  def update(i: Int, v: E#Read) { adapter.update(backing, offset + i*stride, v) }

  def mkReadDataArray[P <: B](primitive: ReadDataArray[E#Component, P])
  :ReadDataArray[E, P] = adapter.mkReadDataArray(primitive)
  def mkReadDataBuffer[P <: B](primitive: ReadDataBuffer[E#Component, P])
  :ReadDataBuffer[E, P] = adapter.mkReadDataBuffer(primitive)
  protected def mkReadDataViewInstance[P <: B](primitive: ReadDataBuffer[E#Component, P], offset: Int, stride: Int)
  :ReadDataView[E, P] = adapter.mkReadDataViewInstance(primitive, offset, stride)

  protected[buffer] final override def mkSerializableInstance() = new SerializableGeneric(adapter)
}

private[buffer] final class SerializableGeneric(val adapter: DataAdapter[_, _]) extends SerializableComposite {
  protected def toReadDataArray(primitive: ReadDataArray[_ <: Primitive, _]): ReadDataArray[_ <: Composite, _] = {
    type E = T forSome { type T <: Composite }
    val primitiveArray = primitive.asInstanceOf[ReadDataArray[E#Component, Defined]]
    adapter.asInstanceOf[DataAdapter[E, Defined]].mkReadDataArray(primitiveArray)
  }
}

final class GenericArray[E<: Composite, +R <: Raw, B <: Defined](
  adapter: DataAdapter[E, B], primitive: ReadDataArray[E#Component, R]
) extends GenericSeq[E, R, B](adapter, primitive, 0, adapter.components) with DataArray[E, R]

final class GenericBuffer[E<: Composite, +R <: Raw, B <: Defined](
  adapter: DataAdapter[E, B], primitive: ReadDataBuffer[E#Component, R]
) extends GenericSeq[E, R, B](adapter, primitive, 0, adapter.components) with DataBuffer[E, R]

final class GenericView[E<: Composite, +R <: Raw, B <: Defined](
  adapter: DataAdapter[E, B], primitive: ReadDataBuffer[E#Component, R], off: Int, str: Int
) extends GenericSeq[E, R, B](adapter, primitive, off, str) with DataView[E, R]
