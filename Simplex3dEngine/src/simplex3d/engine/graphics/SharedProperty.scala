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


sealed abstract class SharedProperty[T <: AnyRef](listener: StructuralChangeListener) {
  private[this] final var value: T = _
  protected final var reassigned = true // Initialize as reassigned.
  
  final def defined: T = value
  final def isDefined = (value != null)
  final def undefine() { defineAs(null.asInstanceOf[T]) }
  
  final def defineAs(value: T) {
    if (isDefined) {
      if (value == null) listener.signalStructuralChanges()
    }
    else if (value != null) listener.signalStructuralChanges()
    
    if (this.value ne value) reassigned = true
    this.value = value
  }

  final def hasRefChanges = reassigned
  
  final override def toString() :String =
    "SharedProperty(" + (if (isDefined) defined.toString else "undefined" ) + ")(refChanges = " + hasRefChanges + ")"
}

final class AccessibleSharedProperty[T <: AnyRef](listener: StructuralChangeListener)
extends SharedProperty[T](listener) {
  def clearRefChanges() { reassigned = false }
}

object SharedProperty {
  def apply[T <: AnyRef](listener: StructuralChangeListener) :SharedProperty[T] = new AccessibleSharedProperty[T](listener)
}


sealed abstract class SharedAttributes[F <: Format with MathType, R <: Raw](listener: StructuralChangeListener)
extends SharedProperty[Attributes[F, R]](listener) {
  def isAccessible = (isDefined && defined.isAccessible)
  def isWritable = (isDefined && defined.isWritable)
  
  def read: ReadDataView[F, R] = if (isDefined) defined.read else null
  def write: DataView[F, R] = if (isDefined) defined.write else null
  def write(first: Int, count: Int): DataView[F, R] = if (isDefined) defined.write(first, count) else null
  def src: DirectSrc = if (isDefined) defined.src else null
  
  def hasChanges = (hasRefChanges || (isDefined && defined.sharedState.hasDataChanges))
}

sealed class AccessibleSharedAttributes[F <: Format with MathType, R <: Raw](listener: StructuralChangeListener)
extends SharedAttributes[F, R](listener) {
  def clearRefChanges() { reassigned = false }
}

object SharedAttributes {
  def apply[F <: Format with MathType, R <: Raw](listener: StructuralChangeListener)
  :SharedAttributes[F, R] = new AccessibleSharedAttributes[F, R](listener)
}
