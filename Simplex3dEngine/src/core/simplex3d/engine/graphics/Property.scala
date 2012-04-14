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
import simplex3d.engine.util._


sealed abstract class Property[W <: Writable[W]] private[engine]
{
  protected final var value: W = _
  protected final var changed = true // Initialize as changed.
  
  final def get: W#Read = if (value == null) throw new NoSuchElementException else value
  final def isDefined = (value != null)
  def mutable: W
}


sealed abstract class Defined[W <: Writable[W]] private[engine] (initialValue: Readable[W])
extends Property[W]
{
  value = initialValue.mutableCopy()
  
  final def mutable: W = {
    changed = true
    value
  }
  
  final override def toString() :String =
    "Defined(" + get.toString + ")"
}

final class AccessibleDefined[W <: Writable[W]] private[engine] (initialValue: Readable[W])
extends Defined[W](initialValue) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
}

object Defined {
  def apply[W <: Writable[W]](initialValue: Readable[W])
  :Defined[W] = new AccessibleDefined(initialValue)
}


sealed abstract class Optional[W <: Writable[W]] private[engine]
  (factory: Readable[W])
  (implicit listener: StructuralChangeListener)
extends Property[W]
{
  
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
  
  final def :=(p: Optional[W]) {
    if (p.isDefined) mutable := p.get else undefine()
  }
  
  final override def toString() :String =
    "Property(" + (if (isDefined) get.toString else "undefined" ) + ")"
}

final class AccessibleOptional[W <: Writable[W]] private[engine]
  (factory: Readable[W])
  (implicit listener: StructuralChangeListener)
 extends Optional[W](factory) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
}

object Optional {
  def apply[W <: Writable[W]](factory: Readable[W])(implicit listener: StructuralChangeListener)
  :Optional[W] = new AccessibleOptional(factory)
}
