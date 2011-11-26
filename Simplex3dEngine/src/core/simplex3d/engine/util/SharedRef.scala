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

package simplex3d.engine.util

import simplex3d.math.types._
import simplex3d.data._


abstract class SharedRef[T <: AnyRef](listener: StructuralChangeListener)
{ self: AccessibleSharedRef =>
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
  
  final def set(r: SharedRef[T]) {
    if (r.isDefined) defineAs(r.defined) else undefine()
  }

  final def hasRefChanges = reassigned
  
  final override def toString() :String =
    "SharedRef(" + (if (isDefined) defined.toString else "undefined" ) + ")(refChanges = " + hasRefChanges + ")"
}

trait AccessibleSharedRef {
  def clearRefChanges() :Unit
}

private[engine] final class AccessibleSharedRefImpl[T <: AnyRef](listener: StructuralChangeListener)
extends SharedRef[T](listener) with AccessibleSharedRef {
  def clearRefChanges() { reassigned = false }
}

object SharedRef {
  def apply[T <: AnyRef](listener: StructuralChangeListener)
  :SharedRef[T] = new AccessibleSharedRefImpl[T](listener)
}
