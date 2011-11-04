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

import java.util.logging._
import simplex3d.math.types._
import simplex3d.engine.common._


trait ReflectStruct[S <: ReflectStruct[S]] extends Struct[S] { self: S =>
  import ReflectStruct._
  
  
  private final var _fieldNames: ReadArray[String] = null
  private final var _fields: ReadArray[UncheckedBinding] = null

  private[this] var initialized = false 
  protected final def reflect(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (fn, fv) = FieldReflection.getValueMap(this, classOf[NestedBinding], Nil, Blacklist)
    _fieldNames = fn
    _fields = fv.asInstanceOf[ReadArray[UncheckedBinding]]
    
    var rebuild = false
    
    var i = 0; while (i < fv.length) {
      if(!fv(i).isInstanceOf[Writable[_]]) {
        logger.log(
          Level.SEVERE, this.getClass.getSimpleName + " value '" + fieldNames(i) +
          "' must be an instance of 'Writable[_]'."
        )
        rebuild = true
      }
      i += 1
    }
    
    if (rebuild) {
      val array = _fields.filter(_.isInstanceOf[Writable[_]]).toArray(ClassManifest.Any)
      _fields = (new ReadArray(array)).asInstanceOf[ReadArray[UncheckedBinding]]
    }
    
    initialized = true
  }
  
  override def fieldNames: ReadArray[String] = _fieldNames
  override def fields: ReadArray[TechniqueBinding] = _fields
  
  final def :=(r: Readable[S]) {
    val s = r.asInstanceOf[ReflectStruct[S]]
    val size = _fields.length; var i = 0; while (i < size) {
      _fields(i) := s._fields(i)
      i += 1
    }
  }
}

object ReflectStruct {
  private final val logger = Logger.getLogger(classOf[ReflectStruct[_]].getName)
  private final val Blacklist = List("mutableCopy", "mkMutable")
}
