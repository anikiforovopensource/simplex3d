/*
 * Simplex3d, CoreBuffer module
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

package simplex3d.buffer

import java.nio._
import scala.reflect._
import scala.annotation.unchecked._
import simplex3d.math._
import simplex3d.buffer._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class DataAdapter[E <: Meta, RawSelection](
  final val elemManifest: ClassManifest[E],
  final val readManifest: ClassManifest[E#Read],
  final val components: Int
) {
  def apply(backing: inContiguous[E#Component, Raw], j: Int) :E#Const
  def update(backing: outContiguous[E#Component, Raw], j: Int, value: E#Read) :Unit
}

sealed abstract class GenericSeq[E <: Composite, +R <: Raw](
  adapter: DataAdapter[E, _], primitive: ReadContiguous[E#Component, R], off: Int, str: Int
) extends CompositeSeq[E, R](primitive, off, str) {
  final def elemManifest = adapter.elemManifest
  final def readManifest = adapter.readManifest
  final def components: Int = adapter.components

  def apply(i: Int) :E#Const = adapter.apply(backing, offset + i*stride)
  def update(i: Int, v: E#Read) { adapter.update(backing, offset + i*stride, v) }

  def mkReadDataArray[P <: Defined](primitive: ReadDataArray[E#Component, P])
  :ReadDataArray[E, P] = new GenericArray[E, P](adapter, primitive)
  def mkReadDataBuffer[P <: Defined](primitive: ReadDataBuffer[E#Component, P])
  :ReadDataBuffer[E, P] = new GenericBuffer[E, P](adapter, primitive)
  def mkReadDataView[P <: Defined](primitive: ReadDataBuffer[E#Component, P], off: Int, str: Int)
  :ReadDataView[E, P] = new GenericView[E, P](adapter, primitive, off, str)

  override def mkSerializableInstance() = null
}

final class GenericArray[E<: Composite, +R <: Raw](
  adapter: DataAdapter[E, _], primitive: ReadDataArray[E#Component, R]
) extends GenericSeq[E, R](adapter, primitive, 0, adapter.components) with DataArray[E, R]

final class GenericBuffer[E<: Composite, +R <: Raw](
  adapter: DataAdapter[E, _], primitive: ReadDataBuffer[E#Component, R]
) extends GenericSeq[E, R](adapter, primitive, 0, adapter.components) with DataBuffer[E, R]

final class GenericView[E<: Composite, +R <: Raw](
  adapter: DataAdapter[E, _], primitive: ReadDataBuffer[E#Component, R], off: Int, str: Int
) extends GenericSeq[E, R](adapter, primitive, off, str) with DataView[E, R]
