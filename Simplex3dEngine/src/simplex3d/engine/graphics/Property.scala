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


sealed abstract class Property[R <: Readable[R]] private[engine] (
  factory: R, listener: StructuralChangeListener
) {
  private[this] final var value: R#Mutable = _
  protected final var changed = true // Initialize as changed.
  
  final def defined: R = value
  final def isDefined = (value != null)
  
  final def undefine() {
    if (isDefined) {
      listener.signalStructuralChanges()
      changed = true
      value = null.asInstanceOf[R#Mutable]
    }
  }
  
  final def mutable: R#Mutable = {
    if (!isDefined) {
      listener.signalStructuralChanges()
      value = factory.mutableCopy()
    }
    changed = true
    value
  }
  
  final def hasChanged = changed
  
  final override def toString() :String =
    "Property(" + (if (isDefined) defined.toString else "undefined" ) + ")(changed = " + hasChanged + ")"
}

final class AccessibleProperty[R <: Readable[R]](
  factory: R, listener: StructuralChangeListener
) extends Property[R](factory, listener) {
  def clearChanges() { changed = false }
}

object Property {
  def apply[R <: Readable[R]](factory: R, listener: StructuralChangeListener)
  :Property[R] = new AccessibleProperty(factory, listener)
}


sealed abstract class ValueProperty[R <: Readable[R] with Binding] private[engine] (
  factory: R, listener: StructuralChangeListener
) extends Property[R](factory, listener)

final class AccessibleValueProperty[R <: Readable[R] with Binding](
  factory: R, listener: StructuralChangeListener
) extends ValueProperty[R](factory, listener) {
  def clearChanges() { changed = false }
}

object ValueProperty {
  def apply[R <: Readable[R] with Binding](factory: R, listener: StructuralChangeListener)
  :ValueProperty[R] = new AccessibleValueProperty(factory, listener)
}


sealed abstract class EnvironmentalProperty[R <: EnvironmentalEffect[R]] private[engine] (
  factory: R, listener: StructuralChangeListener
) extends Property[R](factory, listener)

final class AccessibleEnvironmentalProperty[R <: EnvironmentalEffect[R]](
  factory: R, listener: StructuralChangeListener
) extends EnvironmentalProperty[R](factory, listener) {
  def clearChanges() { changed = false }
}

object EnvironmentalProperty {
  def apply[R <: EnvironmentalEffect[R]](factory: R, listener: StructuralChangeListener)
  :EnvironmentalProperty[R] = new AccessibleEnvironmentalProperty(factory, listener)
}
