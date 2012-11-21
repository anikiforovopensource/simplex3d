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
import java.util.HashSet
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
  
  def getKeys() :(Array[ListSizeKey], Array[EnumKey]) = {
    val lists = new HashMap[ListNameKey, Integer]
    val enums = new HashMap[String, Object]
    
    def putListKey(nameKey: ListNameKey, size: Int) {
      var existing = lists.get(nameKey)
      if (existing == null || size < existing) lists.put(nameKey, size)
    }
    
    def putEnumKey(path: String, value: Object) {
      enums.put(path, value)
    }
    
    def rec(path: String, struct: Struct) {
      val fieldNames = struct.fieldNames
      val fields = struct.fields
      val parentType = ClassUtil.simpleName(struct.getClass)
      
      var i = 0; while (i < fieldNames.length) {
        fields(i).asInstanceOf[AnyRef] match {
          
          case list: BindingList[_] =>
            putListKey(new ListNameKey(parentType, fieldNames(i)), list.size)
            if (list.size > 0) {
              val subPath = path + "." + fieldNames(i)
              if (list.size > 0) {
                if (list(0).isInstanceOf[Struct]) {
                  var j = 0; while (j < list.size) {
                    rec(subPath + "[" + j + "]", list(j).asInstanceOf[Struct])
                    j += 1
                  }
                }
                else if (list(0).isInstanceOf[EnumRef[_]]) {
                  var j = 0; while (j < list.size) {
                    putEnumKey(subPath + "[" + j + "]", list(j).asInstanceOf[EnumRef[_]].toConst)
                    j += 1
                  }
                }
              }
            }
            
          case enum: EnumRef[_] =>
            putEnumKey(path + "." + fieldNames(i), enum.toConst)
            
          case s: Struct =>
            rec(path + "." + fieldNames(i), s)
            
          case _ =>
            // do nothing
        }
        
        i += 1
      }
    }
    
    val listKeys = new Array[ListSizeKey](lists.size);
    {
      val iter = lists.entrySet().iterator()
      var i = 0; while (iter.hasNext()) {
        val entry = iter.next()

        listKeys(i) = new ListSizeKey(entry.getKey, entry.getValue)
        
        i += 1
      }
    }
    
    val enumKeys = new Array[EnumKey](enums.size);
    {
      val iter = enums.entrySet().iterator()
      var i = 0; while (iter.hasNext()) {
        val entry = iter.next()

        enumKeys(i) = new EnumKey(entry.getKey, entry.getValue)
        
        i += 1
      }
    }
    
    (listKeys, enumKeys)
  }
  
  def getUnsizedListKeys() :Array[ListNameKey] = {
    val nameKeys = new HashSet[ListNameKey]
        
    def rec(struct: Struct) {
      val fieldNames = struct.fieldNames
      val fields = struct.fields
      val parentType = ClassUtil.simpleName(struct.getClass)
      
      var i = 0; while (i < fieldNames.length) {
        fields(i).asInstanceOf[AnyRef] match {
          
          case list: BindingList[_] =>
            nameKeys.add(new ListNameKey(parentType, fieldNames(i)))
            val erasure = list.elementManifest.erasure
            if (classOf[Struct].isAssignableFrom(erasure)) rec(erasure.newInstance().asInstanceOf[Struct])

          case s: Struct =>
            rec(s)
            
          case _ =>
            // do nothing
        }
        
        i += 1
      }
    }
    
    nameKeys.toArray(new Array[ListNameKey](0))
  }
}

object Struct {
  private final val logger = Logger.getLogger(classOf[RenderContext].getName)
}
