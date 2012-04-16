/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics

import simplex3d.math.types._
import simplex3d.data._
import simplex3d.engine.util._


sealed abstract class AttributeBinding[F <: Format with MathType, R <: Raw](
  implicit listener: StructuralChangeListener
)
extends SharedRef[Attributes[F, R]](listener) { self: AccessibleSharedRef =>
  def isAccessible = (isDefined && this.get.isAccessible)
  def isWritable = (isDefined && this.get.isWritable)
  
  def read: ReadDataView[F, R] = this.get.read
  def write: DataView[F, R] = this.get.write
  def write(first: Int, count: Int): DataView[F, R] = this.get.write(first, count)
  def src: DirectSrc = this.get.src
}

sealed class AccessibleAttributeBinding[F <: Format with MathType, R <: Raw](
  implicit listener: StructuralChangeListener
)
extends AttributeBinding[F, R] with AccessibleSharedRef {
  import AccessChanges._
  
  def hasRefChanges = reassigned
  def clearRefChanges() { reassigned = false }
  def hasChanges = (hasRefChanges || (isDefined && this.get.sharedState.hasDataChanges))
}

object AttributeBinding {
  def apply[F <: Format with MathType, R <: Raw](implicit listener: StructuralChangeListener)
  :AttributeBinding[F, R] = new AccessibleAttributeBinding[F, R]
}
