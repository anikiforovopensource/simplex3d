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
package prototype

import simplex3d.engine.util._


trait Environment extends graphics.Environment {

  private[this] var _propertyNames: ReadArray[String] = null
  private[this] var _properties: ReadArray[Property[EnvironmentalEffect]] = null

  private[this] var initialized = false 
  protected final def init(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (pn, pv) = FieldReflection.getValueMap(
      this,
      classOf[Property[_]], FieldReflection.EnvironmentalEffectFilter,
      Nil
    )
    _propertyNames = pn
    _properties = pv.asInstanceOf[ReadArray[Property[EnvironmentalEffect]]]
    
    StructuralChangeListener.register(this, properties)
    initialized = true
  }
  
  override def propertyNames: ReadArray[String] = _propertyNames
  override def properties: ReadArray[Property[EnvironmentalEffect]] = _properties
}
