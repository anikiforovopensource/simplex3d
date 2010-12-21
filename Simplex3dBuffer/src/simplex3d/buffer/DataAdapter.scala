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
abstract class DataAdapter[E <: Composite, B <: Defined](
  final val elemManifest: ClassManifest[E],
  final val readManifest: ClassManifest[E#Read],
  final val components: Int
) extends CompositionFactory[E, B] {
  def apply(backing: inContiguous[E#Component, Raw], j: Int) :E#Const
  def update(backing: outContiguous[E#Component, Raw], j: Int, value: E#Read) :Unit

  def mkReadDataArray[P <: B](primitive: ReadDataArray[E#Component, P])
  :ReadDataArray[E, P] = new GenericArray(this, primitive)
  def mkReadDataBuffer[P <: B](primitive: ReadDataBuffer[E#Component, P])
  :ReadDataBuffer[E, P] = new GenericBuffer(this, primitive)
  protected def mkReadDataViewInstance[P <: B](primitive: ReadDataBuffer[E#Component, P], off: Int, str: Int)
  :ReadDataView[E, P] = new GenericView(this, primitive, off, str)
}
