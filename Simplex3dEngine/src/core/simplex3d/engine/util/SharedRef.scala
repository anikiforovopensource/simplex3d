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

import simplex3d.data._


// XXX This class will be removed. Do not use.
@deprecated("This class is getting removed.", since = "")
abstract class SharedRef[T <: AnyRef] { self: AccessibleSharedRef =>
  
  private[this] final var value: T = _
  protected final var reassigned = true // Initialize as reassigned.
  
  final def get: T = if (value == null) throw new NoSuchElementException else value
  final def isDefined = (value != null)
  final def undefine() {
    value = null.asInstanceOf[T]
  }
  
  final def :=(value: T) {
    if (value == null) throw new NullPointerException
    
    if (this.value ne value) reassigned = true
    this.value = value
  }
  
  final def :=(r: SharedRef[T]) {
    if (r.isDefined) this := r.get else undefine()
  }
  
  final override def toString :String =
    "SharedRef(" + (if (isDefined) get.toString else "undefined" ) + ")(refChanges = " + hasRefChanges + ")"
}

trait AccessibleSharedRef {
  def hasRefChanges: Boolean
  def clearRefChanges() :Unit
}

private[engine] final class AccessibleSharedRefImpl[T <: AnyRef]
extends SharedRef[T] with AccessibleSharedRef {
  def hasRefChanges = reassigned
  def clearRefChanges() { reassigned = false }
}

object SharedRef {
  def apply[T <: AnyRef]() :SharedRef[T] = new AccessibleSharedRefImpl[T]
}
