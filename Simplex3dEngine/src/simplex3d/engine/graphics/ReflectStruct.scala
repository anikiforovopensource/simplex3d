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

import java.util.logging._
import simplex3d.math.types._


trait ReflectStruct[C <: ReflectStruct[C]] extends Struct[C] { self: C =>
  import ReflectStruct.logger._
  
  
  private[this] var _fieldNames: ReadArray[String] = null
  private[this] var _fields: ReadArray[TechniqueBinding] = null

  private[this] var initialized = false 
  protected final def reflect(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (fn, fv) = FieldReflection.getValueMap(this, classOf[NestedBinding], Nil, ReflectStruct.Blacklist)
    _fieldNames = fn
    _fields = fv.asInstanceOf[ReadArray[TechniqueBinding]]
    
    var rebuild = false
    
    var i = 0; while (i < fv.length) {
      if(!fv(i).isInstanceOf[Readable[_]]) {
        log(
          Level.SEVERE, this.getClass.getSimpleName + " value '" + fieldNames(i) +
          "' must be an instance of 'Readable[_]'."
        )
        rebuild = true
      }
      i += 1
    }
    
    if (rebuild) {
      _fields = new ReadArray(_fields.filter(_.isInstanceOf[Readable[_]]).toArray)
    }
    
    initialized = true
  }
  
  override def fieldNames: ReadArray[String] = _fieldNames
  override def fields: ReadArray[TechniqueBinding] = _fields
}

object ReflectStruct {
  private final val logger = Logger.getLogger(classOf[ReflectStruct[_]].getName)
  
  private final val Blacklist = List("mutableCopy", "mkMutable")
}
