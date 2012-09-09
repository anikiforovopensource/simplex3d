/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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

package simplex3d.engine.util

import simplex3d.math.types._
import simplex3d.engine.graphics._


private[engine] object PathUtil {
  
  /* case NameIndex(name, index, rest) */
  private val NameIndexRest = """(\w+)\[(\d+)\]\.?(.*)""".r
  
  /* case NameRest(name, rest) */
  private val NameRest = """(\w+)\.?(.*)""".r
  
  
  def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    
    -1
  }
  
  
  def resolve(path: String, bindingFromName: String => AnyRef) :AnyRef = {
    
    def resolveRest(value: AnyRef, rest: String) :AnyRef = {
      value match {
                  
        case s: Struct =>
          s.resolve(rest)

        /* Replace when 2.10 is out.
        case t: ReadTextureBinding[_] if t.isBound =>
          rest match {
            case "sampler" => t
            case "dimensions" => t.bound.dimensions
            case _ => null
          }
          
        case _ =>
          null*/
          
        case _ =>
          if (TextureBinding.avoidCompilerCrashB(value)) {
            val t = TextureBinding.avoidCompilerCrash(value)
            if (t.isBound) {
              rest match {
                case "sampler" => t
                case "dimensions" => t.bound.asInstanceOf[Texture[_]].bindingDimensions
                case _ => null
              }
            }
            else null
          }
          else null
      }
    }
    
    path match {
      
      case NameIndexRest(name, index, rest) =>
        val res = bindingFromName(name)
        if (res == null) null else res match {
          
          case list: BindingList[_] =>
            val id = index.toInt
            if (id >= list.size) null else {
              
              val indexed = list(id)
              if (rest.isEmpty) indexed
              else resolveRest(indexed, rest)
            }
            
          case _ =>
            null
        }
        
      case NameRest(name, rest) =>
        val res = bindingFromName(name)
        if (res == null) null else if (rest.isEmpty) res else resolveRest(res, rest)
        
      case _ =>
        null
    }
  }
}
