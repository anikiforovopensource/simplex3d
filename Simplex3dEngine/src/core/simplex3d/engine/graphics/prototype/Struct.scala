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
import java.util.HashMap
import simplex3d.math.types._
import simplex3d.engine.util._


trait ReadStruct extends graphics.ReadStruct {
  type Read <: ReadStruct
  type Mutable <: Struct
}

trait Struct extends ReadStruct with graphics.Struct {
  import Struct._
  
  private final var _fieldNames: ReadArray[String] = null
  private final var _fields: ReadArray[UncheckedBinding] = null
  private final var _listDeclarations: ReadArray[ListDeclaration] = null

  private[this] var initialized = false 
  protected final def init(clazz: Class[_]) {
    if (clazz != this.getClass) return // Allows correct sub-classing.
    if (initialized) return
    
    val (fn, fv) = FieldReflection.getValueMap(this, classOf[Binding], Nil, Blacklist)
    _fieldNames = fn
    _fields = fv.asInstanceOf[ReadArray[UncheckedBinding]]
    
    var rebuild = false
    
    var i = 0; while (i < fv.length) {
      if(!fv(i).isInstanceOf[Accessible]) {
        logger.log(
          Level.SEVERE, ClassUtil.simpleName(this.getClass) + " value '" + fieldNames(i) +
          "' must be an instance of 'Writable[_]'."
        )
        rebuild = true
      }
      i += 1
    }
    
    if (rebuild) {
      val array = _fields.filter(_.isInstanceOf[Accessible]).toArray(ClassManifest.Any)
      _fields = (new ReadArray(array)).asInstanceOf[ReadArray[UncheckedBinding]]
    }
    
    
    // Extract list declarations.
    val parentType = ClassUtil.simpleName(clazz)
    val declarations = new HashMap[(String, String), List[BindingList[_]]]
    
    def register(nameKey: (String, String), list: BindingList[_]) {
      var existing = declarations.get(nameKey)
      if (existing == null) {
        existing = Nil
      }
      declarations.put(nameKey, list :: existing)
    }
    
    def registerStruct(s: Struct) {
      val nestedDeclarations = s.listDeclarations
      var j = 0; while (j < nestedDeclarations.size) {
        val dec = nestedDeclarations(j)
        
        var k = 0; while (k < dec.lists.size) {
          register(dec.nameKey, dec.lists(k))
          k += 1
        }
        
        j += 1
      }
    }
    
    i = 0; while (i < fieldNames.length) {
      fields(i) match {
        
        case list: BindingList[_] =>
          register((parentType, fieldNames(i)), list)
          val erasure = list.elementManifest.erasure
          if (classOf[Struct].isAssignableFrom(erasure)) {
            registerStruct(erasure.newInstance().asInstanceOf[Struct])
          }
          
        case s: Struct =>
          registerStruct(s)
          
        case _ =>
          // do nothing
      }
      
      i += 1
    }
    
    val listDeclarations = new Array[ListDeclaration](declarations.size)
    val iter = declarations.entrySet().iterator()
    i = 0; while (iter.hasNext()) {
      val entry = iter.next()
      val key = entry.getKey
      
      listDeclarations(i) = new ListDeclaration(key._1, key._2, new ReadArray(entry.getValue.toArray))
      
      i += 1
    }
    
    _listDeclarations = new ReadArray(listDeclarations)
    
    
    initialized = true
  }
  
  override def fieldNames: ReadArray[String] = _fieldNames
  override def fields: ReadArray[Binding] = _fields
  override def listDeclarations: ReadArray[ListDeclaration] = _listDeclarations
  
  final def :=(r: Read) {
    val s = r.asInstanceOf[Struct]
    val size = _fields.length; var i = 0; while (i < size) {
      _fields(i) := s._fields(i)
      i += 1
    }
  }
  

  private[engine] override def register(listener: StructuralChangeListener) {
    val s = fields.length; var i = 0; while (i < s) {
      fields(i) match { case n: StructuralChangeNotifier => n.register(listener); case _ => /* ignore */ }
      i += 1
    }
  }
  private[engine] override def unregister() {
    val s = fields.length; var i = 0; while (i < s) {
      fields(i) match { case n: StructuralChangeNotifier => n.unregister(); case _ => /* ignore */ }
      i += 1
    }
  }
}

object Struct {
  private final val logger = Logger.getLogger(classOf[Struct].getName)
  private final val Blacklist = List("mutableCopy", "mkMutable", "binding", "resolveBinding")
}
