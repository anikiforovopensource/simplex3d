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
import simplex3d.engine.util._


trait ReadEnvironmentalEffect extends Protected with StructuralChangeNotifier {
  type Read <: ReadEnvironmentalEffect
  type Mutable <: EnvironmentalEffect
  
  def propagate(parentVal: Read, result: Mutable) :Unit //XXX hide this as well
  
  
  private[this] var localBinding: Binding = null
  protected def resolveBinding() :Binding
  
  /** Must return a stable binding that will no change until the next binding change event.
   */
  final def binding: Binding = {
    if (localBinding == null) localBinding = resolveBinding()
    localBinding
  }
  
  
  private[this] var bindingChanges = true
  
  /** This method must return true to signal binding changes and indicate that a new binding must be resolved.
   */
  final def hasBindingChanges: Boolean = bindingChanges//XXX hide this
  final def clearBindingChanges() { bindingChanges = false }
  
  protected final def signalBindingChanges() {
    bindingChanges = true
    localBinding = null
  }
}

trait EnvironmentalEffect extends ReadEnvironmentalEffect with Accessible {
  protected def mkMutable() :Mutable
  
  override def mutableCopy(): Mutable = {
    val copy = mkMutable()
    copy := this.asInstanceOf[copy.Read]
    copy
  }
}


//XXX split into pass-level and mesh-level updatables, also better names
trait ReadUpdatableEnvironmentalEffect extends ReadEnvironmentalEffect with Protected {
  type Read <: ReadUpdatableEnvironmentalEffect
  type Mutable <: UpdatableEnvironmentalEffect
  
  /** Update the stable binding with a set of predefined uniforms that are only available at the time of rendering.
   * 
   * Values that depend on camera must be taken from predefined uniforms,
   * so that passes with different cameras can be rendered correctly. 
   */
  def updateBinding(predefinedUniforms: ReadPredefinedUniforms) :Unit //XXX hide this from the client code
}

trait UpdatableEnvironmentalEffect extends ReadUpdatableEnvironmentalEffect with EnvironmentalEffect {
}
