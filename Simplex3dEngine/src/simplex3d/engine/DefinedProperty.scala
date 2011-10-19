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


abstract class DefinedProperty[R <: Readable[R]] private[engine] (initialValue: R)
{ self: AccessibleDefinedProperty =>
  private[this] final var value: R#Mutable = initialValue.mutableCopy()
  protected final var changed = true // Initialize as changed.
  
  final def defined: R = value
  final def isDefined = true
  
  final def mutable: R#Mutable = {
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

private[engine] final class AccessibleDefinedPropertyImpl[R <: Readable[R]](factory: R)
extends DefinedProperty[R](factory) with AccessibleDefinedProperty {
  def clearDataChanges() { changed = false }
}

object DefinedProperty {
  def apply[R <: Readable[R] with Binding](factory: R)
  :DefinedProperty[R] = new AccessibleDefinedPropertyImpl(factory)
}
