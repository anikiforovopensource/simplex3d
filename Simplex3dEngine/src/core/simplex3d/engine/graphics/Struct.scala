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

import java.lang.Integer
import java.util.logging._
import java.util.HashMap
import java.util.HashSet
import scala.reflect.runtime.universe._
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
  def fields: ReadArray[UncheckedRef]
  
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
  
  def getSizeKeys() :Array[ListSizeKey] = {//XXX get rid of this method
    val lists = new HashMap[ListNameKey, Integer]
    val enums = new HashMap[String, Object]
    
    collectKeys("", lists, enums)
    
    val listKeys = new Array[ListSizeKey](lists.size);
    {
      val iter = lists.entrySet().iterator()
      var i = 0; while (iter.hasNext()) {
        val entry = iter.next()

        listKeys(i) = new ListSizeKey(entry.getKey, entry.getValue)
        
        i += 1
      }
    }
    
    listKeys
  }
  
  def collectKeys(path: String, lists: HashMap[ListNameKey, Integer], enums: HashMap[String, Object]) {
    
    def appendPath(name: String) = if (path.isEmpty) name else path + "." + name
    val parentType = ClassUtil.simpleName(this.getClass)
    
    var i = 0; while (i < fieldNames.length) {
      fields(i).asInstanceOf[AnyRef] match {
        
        case seq: BindingSeq[_] =>
          seq.collectKeys(appendPath(fieldNames(i)), new ListNameKey(parentType, fieldNames(i)), lists, enums)
          
        case enum: EnumRef[_] =>
          enum.collectKeys(appendPath(fieldNames(i)), enums)
          
        case s: Struct =>
          s.collectKeys(appendPath(fieldNames(i)), lists, enums)
          
        case _ => // do nothing
      }
      
      i += 1
    }
  }
  
  def samplerRemapping(path: String, remapping: HashMap[String, String]) {
    def appendPath(name: String) = if (path.isEmpty) name else path + "." + name
    
    var i = 0; while (i < fieldNames.length) {
      fields(i).asInstanceOf[AnyRef] match {
        case seq: BindingSeq[_] => seq.samplerRemapping(appendPath(fieldNames(i)), remapping)
        case s: Struct =>  s.samplerRemapping(appendPath(fieldNames(i)), remapping)
        case t: TextureBinding[_] => t.samplerRemapping(appendPath(fieldNames(i)), remapping)
        case _ => // do nothing
      }
      
      i += 1
    }
  }
  
  def getUnsizedListKeys() :Array[ListNameKey] = {
    val nameKeys = new HashSet[ListNameKey]
        
    def rec(struct: Struct) {
      val fieldNames = struct.fieldNames
      val fields = struct.fields
      val parentType = ClassUtil.simpleName(struct.getClass)
      
      var i = 0; while (i < fieldNames.length) {
        fields(i).asInstanceOf[AnyRef] match {
          
          case seq: BindingSeq[_] =>
            nameKeys.add(new ListNameKey(parentType, fieldNames(i)))
            if (seq.elementTag.tpe <:< Types.Struct)
              rec(ClassUtil.runtimeClass(seq.elementTag.tpe).newInstance().asInstanceOf[Struct])

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
