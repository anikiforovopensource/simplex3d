/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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


sealed abstract class Reassignable[T <: Accessible] private[engine] (
  private[this] final val enforceDefined: Boolean
) extends StructuralChangeNotifier
{
  
  //*** StructuralChangeListener Code *********************************************************************************
  
  protected final var listener: StructuralChangeListener = _
  
  private[engine] final override def register(listener: StructuralChangeListener) {
    if (this.listener != null) throw new IllegalStateException("The property can register StructuralChangeListener only once.")
    this.listener = listener
  }
  
  private[engine] final override def unregister() {
    throw new UnsupportedOperationException("Properties cannot unregister StructuralChangeListeners.")
  }
  
  protected final def registerStructuralChangeListener(listener: StructuralChangeListener) {}
  protected final def unregisterStructuralChangeListener() {}
  
  
  //*** Property Code *************************************************************************************************
  
  private[this] final var value: T = _
  protected final var changed = true // Initialize as changed.
  
  final def get: T = if (value == null) throw new NoSuchElementException else value
  final def isDefined = (value != null)
  
  final def undefine() {
    if (enforceDefined) throw new UnsupportedOperationException("The property was declared as Reassignable.defined() and cannot be undefined.")
    
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
  
  private final def init(value: T) {
    this.value = value
    value match { case n: StructuralChangeNotifier => n.register(listener); case _ => /* ignore */ }
    changed = true
  }
  
  final def :=(t: T) {
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
  
  final def :=(p: Reassignable[T]) {
    if (p.isDefined) this := p.get else undefine()
  }
  
  final override def toString() :String = {
    "Reassignable(" + (if (isDefined) get.toString else "undefined" ) + ")"
  }
}

final class AccessibleReassignable[T <: Accessible] private[engine] (
  enforceDefined: Boolean
)
extends Reassignable[T](enforceDefined) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
  def signalDataChanges() { changed = true }
}

object Reassignable {
  def optional[T <: Accessible]() :Reassignable[T] = {
    new AccessibleReassignable[T](false)
  }
  
  def defined[T <: Accessible](value: T) :Reassignable[T] = {
    val prop = new AccessibleReassignable[T](true)
    prop.init(value)
    prop
  }
}
