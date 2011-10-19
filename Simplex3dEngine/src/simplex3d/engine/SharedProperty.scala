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

import simplex3d.math.types._
import simplex3d.data._


abstract class SharedProperty[T <: AnyRef](listener: StructuralChangeListener)
{ self: AccessibleSharedProperty =>
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

trait AccessibleSharedProperty {
  def clearRefChanges() :Unit
}

private[engine] final class AccessibleSharedPropertyImpl[T <: AnyRef](listener: StructuralChangeListener)
extends SharedProperty[T](listener) with AccessibleSharedProperty {
  def clearRefChanges() { reassigned = false }
}

object SharedProperty {
  def apply[T <: AnyRef](listener: StructuralChangeListener)
  :SharedProperty[T] = new AccessibleSharedPropertyImpl[T](listener)
}
