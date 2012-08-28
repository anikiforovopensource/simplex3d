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


sealed abstract class Property[T <: Accessible] private[engine]
{
  protected final var value: T = _
  protected final var changed = true // Initialize as changed.
  
  final def get: T#Read = if (value == null) throw new NoSuchElementException else value.asInstanceOf[T#Read]
  final def isDefined = (value != null)
  def update: T
}


sealed abstract class Defined[T <: Accessible] private[engine] (initialValue: T#Read)
extends Property[T]
{
  value = initialValue.mutableCopy().asInstanceOf[T]
  
  final def update: T = {
    changed = true
    value
  }
  
  final override def toString() :String = "Defined(" + get.toString + ")"
}

// XXX rename to SomethingDefined or DefinedSomething
final class AccessibleDefined[T <: Accessible] private[engine] (initialValue: T#Read)
extends Defined[T](initialValue) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
}

object Defined {
  def apply[T <: Accessible](initialValue: T#Read)
  :Defined[T] = new AccessibleDefined(initialValue)
}


sealed abstract class Optional[T <: Accessible] private[engine] (
  implicit listener: StructuralChangeListener
)
extends Property[T]
{
  
  final def undefine() {
    if (isDefined) {
      listener.signalStructuralChanges()
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
      listener.signalStructuralChanges()
    }
    changed = true
  }
  
  final def :=(p: Optional[T]) {
    if (p.isDefined) this := p.get else undefine()
  }
  
  final override def toString() :String = {
    "Optional(" + (if (isDefined) get.toString else "undefined" ) + ")"
  }
}

final class AccessibleOptional[T <: Accessible] private[engine] (
  implicit listener: StructuralChangeListener
)
extends Optional[T] {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
}

object Optional {
  def apply[T <: Accessible](implicit listener: StructuralChangeListener)
  :Optional[T] = new AccessibleOptional
}
