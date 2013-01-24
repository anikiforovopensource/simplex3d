/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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
package util

import simplex3d.math.types._


sealed abstract class Value[T <: Accessible] private[engine] (
  private[this] final val value: T
) extends Updatable[T]
{
  
  //*** PropertyContext Code ******************************************************************************************
  
  protected final var propertyContext: PropertyContext = _
  
  private[engine] final def register(context: PropertyContext) {
    if (context == null) throw new NullPointerException
    if (this.propertyContext != null && (this.propertyContext ne context)) throw new IllegalStateException(
      "Value can register PropertyContext only once."
    )
    this.propertyContext = context
  }
  
  private[engine] final def unregister() {
    throw new UnsupportedOperationException("Value cannot unregister PropertyContexts.")
  }
    
  
  //*** Value Code *************************************************************************************************
  
  /** getter navigates to the desired field of the value of this property.
   * function modifies the field and returns true to run next frame or false to be removed.
   */
  final def controller[A](getter: T => A)(function: (A, TimeStamp) => Boolean) {
    if (propertyContext == null || propertyContext.controllerContext == null) {
      throw new UnsupportedOperationException("ControllerContext is not defined.")
    }
    PropertyUpdater.register(propertyContext.controllerContext, true, this)(getter, function)
  }
  
  /** function modifies the field and returns true to run next frame or false to be removed.
   */
  final def controller(function: (T, TimeStamp) => Boolean) {
    if (propertyContext == null || propertyContext.controllerContext == null) {
      throw new UnsupportedOperationException("ControllerContext is not defined.")
    }
    PropertyUpdater.register(propertyContext.controllerContext, true, this)(Property.identityGetter, function)
  }
  
  
  protected final var changed = true // Initialize as changed.
  
  final def get: T#Read = value.asInstanceOf[T#Read]
  final def isDefined = true
  
  final def update: T = {
    changed = true
    value
  }
  
  final def :=(p: Value[T]) {
    val stable = this.update
    stable := p.get.asInstanceOf[stable.Read]
  }
  
  final override def toString :String = {
    "Value(" + (if (isDefined) get.toString else "undefined" ) + ")"
  }
}

final class AccessibleValue[T <: Accessible] private[engine] (value: T)
extends Value[T](value) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
  def signalDataChanges() { changed = true }
}

object Value {
  def apply[T <: Accessible](value: T) :Value[T] = {
    new AccessibleValue[T](value)
  }
}
