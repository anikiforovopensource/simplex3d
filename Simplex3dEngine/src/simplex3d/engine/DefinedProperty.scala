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


abstract class DefinedProperty[W <: Writable[W]] private[engine] (initialValue: Readable[W])
{ self: AccessibleDefinedProperty =>
  private[this] final var value: W = initialValue.mutableCopy()
  protected final var changed = true // Initialize as changed.
  
  final def defined: W#Read = value
  final def isDefined = true
  
  final def mutable: W = {
    changed = true
    value
  }
  
  final def hasDataChanges = changed
  
  final override def toString() :String =
    "DefinedProperty(" + defined.toString + ")(changed = " + hasDataChanges + ")"
}

trait AccessibleDefinedProperty {
  def clearDataChanges(): Unit
}

private[engine] final class AccessibleDefinedPropertyImpl[W <: Writable[W]](initialValue: Readable[W])
extends DefinedProperty[W](initialValue) with AccessibleDefinedProperty {
  def clearDataChanges() { changed = false }
}

object DefinedProperty {
  def apply[W <: Writable[W] with Binding](initialValue: Readable[W])
  :DefinedProperty[W] = new AccessibleDefinedPropertyImpl(initialValue)
}
