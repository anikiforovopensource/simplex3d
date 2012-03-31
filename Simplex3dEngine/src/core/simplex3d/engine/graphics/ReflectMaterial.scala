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


trait ReflectMaterial extends Material {
  
  private[this] var _uniformNames: ReadArray[String] = null
  private[this] var _uniforms: ReadArray[Property[UncheckedBinding]] = null
  
  private[this] var initialized = false 
  protected final def reflect(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (un, uv) = FieldReflection.getValueMap(
      this, classOf[Property[_]], FieldReflection.TechniqueBindingFilter, ReflectMaterial.UniformBlacklist
    )
    _uniformNames = un
    _uniforms = uv.asInstanceOf[ReadArray[Property[UncheckedBinding]]]
    
    initialized = true
  }
  
  override def uniformNames: ReadArray[String] = _uniformNames
  override def uniforms: ReadArray[Property[UncheckedBinding]] = _uniforms
}

object ReflectMaterial {
  private val UniformBlacklist = List[String]()
}
