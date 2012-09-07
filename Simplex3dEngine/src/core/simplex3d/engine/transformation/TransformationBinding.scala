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

package simplex3d.engine.transformation

import simplex3d.math.double._


sealed abstract class TransformationBinding[T <: Transformation] private[engine] (
  private[this] final val factory: () => T
)
{
  private[this] final var value: T = _
  protected final var changed = true // Initialize as changed.
  private[this] final var updateMatrix = true
  private[this] final var cachedMatrix: Mat4x3 = _
  
  final def get: T#Read = if (value == null) throw new NoSuchElementException else value.asInstanceOf[T#Read]
  final def isDefined = (value != null)
  
  final def undefine() {
    if (isDefined) {
      changed = true
      updateMatrix = true
      value = null.asInstanceOf[T]
    }
  }
  
  final def update: T = {
    if (!isDefined) value = factory()
    changed = true
    updateMatrix = true
    value
  }
  
  final def matrix :ReadMat4x3 = {
    if (updateMatrix) {
      updateMatrix = false
      if (isDefined) {
        if (cachedMatrix == null) cachedMatrix = value.matrix
        else cachedMatrix := value.matrix
      }
      else {
        cachedMatrix = null
      }
    }
    
    if (cachedMatrix != null) cachedMatrix else Mat4x3.Identity
  }
  
  final def :=(p: TransformationBinding[T]) {
    if (p.isDefined) {
      val stable = this.update
      stable := p.get.asInstanceOf[stable.Read]
    }
    else undefine()
  }
  
  final override def toString() :String = {
    "TransformationBinding(" + (if (isDefined) get.toString else "undefined" ) + ")"
  }
}

final class AccessibleTransformationBinding[T <: Transformation] private[engine] (
  factory: () => T
)
extends TransformationBinding[T](factory) {
  def hasDataChanges = changed
  def clearDataChanges() { changed = false }
  def signalDataChanges() { changed = true }
}

object TransformationBinding {
  def apply[T <: Transformation](factory: () => T) :TransformationBinding[T] = {
    new AccessibleTransformationBinding[T](factory)
  }
}
