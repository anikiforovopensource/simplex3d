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

import java.util.logging._
import simplex3d.math.types._
import simplex3d.engine.util._


trait ReadStruct extends graphics.ReadStruct {
  type Read <: ReadStruct
  type Mutable <: Struct
}

trait Struct extends ReadStruct with graphics.Struct {
  import Struct._
  
  private final var _fieldNames: ReadArray[String] = null
  private final var _fields: ReadArray[UncheckedValue] = null
  private final var _listDeclarations: ReadArray[ListDeclaration] = null

  private[this] var initialized = false 
  protected final def init(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (fn, fv) = FieldReflection.valueMap(this, classOf[Accessible], Nil, Blacklist)
    _fieldNames = fn
    _fields = fv.asInstanceOf[ReadArray[UncheckedValue]]
    _listDeclarations = findListDeclarations()
    
    initialized = true
  }
  
  override def fieldNames: ReadArray[String] = _fieldNames
  override def fields: ReadArray[UncheckedValue] = _fields
  override def listDeclarations: ReadArray[ListDeclaration] = _listDeclarations
  
  
  private[engine] override def register(context: PropertyContext) {
    val s = fields.length; var i = 0; while (i < s) {
      fields(i) match { case d: PropertyContextDependent => d.register(context); case _ => /* ignore */ }
      i += 1
    }
  }
  private[engine] override def unregister() {
    val s = fields.length; var i = 0; while (i < s) {
      fields(i) match { case d: PropertyContextDependent => d.unregister(); case _ => /* ignore */ }
      i += 1
    }
  }
  protected def registerPropertyContext(context: PropertyContext) {}
  protected def unregisterPropertyContext() {}
}

object Struct {
  private final val logger = Logger.getLogger(classOf[Struct].getName)
  private final val Blacklist = List("mutableCopy", "mkMutable", "binding", "resolveBinding")
}
