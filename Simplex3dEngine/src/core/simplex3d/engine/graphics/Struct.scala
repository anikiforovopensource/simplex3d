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
import java.util.HashMap
import simplex3d.math.types._
import simplex3d.engine.util._


/** All Struct subclasses must define a no-argument constructor.
 */
trait ReadStruct extends Protected with Binding with PropertyContextDependent {
  type Read <: ReadStruct
  type Mutable <: Struct
}
  
/** All Struct subclasses must define a no-argument constructor.
 */
trait Struct extends ReadStruct with Accessible {
  import Struct.logger._
  
  protected def mkMutable() :Mutable
  
  override def mutableCopy(): Mutable = {
    val copy = mkMutable()
    copy := this.asInstanceOf[copy.Read]
    copy
  }
  
  def fieldNames: ReadArray[String]
  def fields: ReadArray[UncheckedValue]
  def listDeclarations: ReadArray[ListDeclaration]
  
  private def getField(name: String) :AnyRef = {
    val id = PathUtil.find(fieldNames, name)
    if (id == -1) null else fields(id)
  }
  
  final def resolve[T](path: String) :T = {
    val res = path match {
      
      case PathUtil.NameIndexRest(name, index, rest) =>
        PathUtil.resolveAsList(index.toInt, rest, getField(name))
        
      case PathUtil.NameRest(name, rest) =>
        PathUtil.resolveAsValue(rest, getField(name))
        
      case _ =>
        null
    }
    
    res.asInstanceOf[T]
  }
  
  def :=(r: Read) {
    val s = r.asInstanceOf[Struct]
    val size = fields.length; var i = 0; while (i < size) {
      fields(i) := s.fields(i)
      i += 1
    }
  }
  
  protected def findListDeclarations() :ReadArray[ListDeclaration] = {
    val clazz = this.getClass
    val parentType = ClassUtil.simpleName(clazz)
    val declarations = new HashMap[(String, String), List[BindingList[_]]]
    
    def register(nameKey: (String, String), list: BindingList[_]) {
      var existing = declarations.get(nameKey)
      if (existing == null) existing = Nil
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
    
    var i = 0; while (i < fieldNames.length) {
      fields(i).asInstanceOf[AnyRef] match {
        
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
    
    new ReadArray(listDeclarations)
  }
}

object Struct {
  private final val logger = Logger.getLogger(classOf[RenderContext].getName)
}
