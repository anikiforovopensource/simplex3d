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


sealed abstract class DefinedProperty[R <: Readable[R]] private[engine] (initialValue: R) {
  private[this] final var value: R#Mutable = initialValue.mutableCopy()
  protected final var changed = true // Initialize as changed.
  
  final def defined: R = value
  final def isDefined = true
  
  final def mutable: R#Mutable = {
    changed = true
    value
  }
  
  final def hasChanged = changed
  
  final override def toString() :String =
    "DefinedProperty(" + defined.toString + ")(changed = " + hasChanged + ")"
}

final class AccessibleDefinedProperty[R <: Readable[R]](factory: R)
extends DefinedProperty[R](factory) {
  def clearChanges() { changed = false }
}

object DefinedProperty {
  def apply[R <: Readable[R] with Binding](factory: R)
  :DefinedProperty[R] = new AccessibleDefinedProperty(factory)
}


sealed abstract class ShaderProperty[R <: Readable[R] with NestedBinding] private[engine] (factory: R)
extends DefinedProperty[R](factory)

final class AccessibleShaderProperty[R <: Readable[R] with NestedBinding](factory: R)
extends ShaderProperty[R](factory) {
  def clearChanges() { changed = false }
}

object ShaderProperty {
  def apply[R <: Readable[R] with NestedBinding](factory: R)
  :ShaderProperty[R] = new AccessibleShaderProperty(factory)
}
