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


sealed abstract class Property[T <: Accessible] private[engine] ()
{
  protected final var listener: StructuralChangeListener = _
  final def register(listener: StructuralChangeListener) { this.listener = listener } //XXX hide this
  
  
  private[this] final var value: T = _
  protected final var changed = true // Initialize as changed.
  
  
  final def get: T#Read = if (value == null) throw new NoSuchElementException else value.asInstanceOf[T#Read]
  final def isDefined = (value != null)
  
  final def undefine() {
    if (isDefined) {
      if (listener != null) listener.signalStructuralChanges()
      changed = true
      value match { case n: StructuralChangeNotifier => n.unregister(); case _ => /* ignore */ }
      value = null.asInstanceOf[T]
    }
  }
  
  final def update: T = {
    if (!isDefined) throw new NoSuchElementException
    changed = true
    value
  }
  
  final def :=(t: T#Read) {
    if (isDefined && (value.readType eq t.readType)) {
      val stable = value
      stable := t.asInstanceOf[stable.Read]
    }
    else {
      value match { case n: StructuralChangeNotifier => n.unregister(); case _ => /* ignore */ }
      value = t.mutableCopy().asInstanceOf[T]
      value match { case n: StructuralChangeNotifier => n.register(listener); case _ => /* ignore */ }
      if (listener != null) listener.signalStructuralChanges()
    }
    changed = true
  }
  
  final def :=(p: Property[T]) {
    if (p.isDefined) this := p.get else undefine()
  }
  
  final override def toString() :String = {
    "Property(" + (if (isDefined) get.toString else "undefined" ) + ")"
  }
}

final class AccessibleProperty[T <: Accessible] private[engine] ()
extends Property[T] {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
}

object Property {
  def apply[T <: Accessible]() :Property[T] = new AccessibleProperty
  
  def apply[T <: Accessible](value: T#Read) :Property[T] = {
    val prop = new AccessibleProperty[T]()
    prop := value
    prop
  }
}
