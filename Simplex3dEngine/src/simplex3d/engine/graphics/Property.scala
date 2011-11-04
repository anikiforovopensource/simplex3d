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

package simplex3d.engine.graphics

import simplex3d.math.types._
import simplex3d.engine.common._


sealed abstract class Property[W <: Writable[W]] private[engine]
{
  protected final var value: W = _
  protected final var changed = true // Initialize as changed.
  
  final def defined: W#Read = value
  final def isDefined = (value != null)
  def mutable: W
  
  final def hasDataChanges = changed
}


sealed abstract class DefinedProperty[W <: Writable[W]] private[engine] (initialValue: Readable[W])
extends Property[W]
{
  value = initialValue.mutableCopy()
  
  final def mutable: W = {
    changed = true
    value
  }
  
  final override def toString() :String =
    "DefinedProperty(" + defined.toString + ")(changed = " + hasDataChanges + ")"
}

final class AccessibleDefinedProperty[W <: Writable[W]] private[engine] (initialValue: Readable[W])
extends DefinedProperty[W](initialValue) {
  def clearDataChanges() { changed = false }
}

object DefinedProperty {
  def apply[W <: Writable[W] with Binding](initialValue: Readable[W])
  :DefinedProperty[W] = new AccessibleDefinedProperty(initialValue)
}


sealed abstract class OptionalProperty[W <: Writable[W]] private[engine] (
  factory: Readable[W], listener: StructuralChangeListener
) extends Property[W] {
  
  final def undefine() {
    if (isDefined) {
      listener.signalStructuralChanges()
      changed = true
      value = null.asInstanceOf[W]
    }
  }
  
  final def mutable: W = {
    if (!isDefined) {
      listener.signalStructuralChanges()
      value = factory.mutableCopy()
    }
    changed = true
    value
  }
  
  final def set(p: OptionalProperty[W]) {
    if (p.isDefined) mutable := p.defined else undefine()
  }
  
  final override def toString() :String =
    "Property(" + (if (isDefined) defined.toString else "undefined" ) + ")(changed = " + hasDataChanges + ")"
}

final class AccessibleOptionalProperty[W <: Writable[W]] private[engine] (
  factory: Readable[W], listener: StructuralChangeListener
) extends OptionalProperty[W](factory, listener) {
  def clearDataChanges() { changed = false }
}

object OptionalProperty {
  def apply[W <: Writable[W]](factory: Readable[W], listener: StructuralChangeListener)
  :OptionalProperty[W] = new AccessibleOptionalProperty(factory, listener)
}
