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


private[simplex3d] object PathUtil {
  
  /* case NameIndex(name, index) */
  val NameIndex = """(\w+)\[(\d+)\]""".r
  
  /* case NameIndexRest(name, index, rest) */
  val NameIndexRest = """(\w+)\[(\d+)\]\.?(.*)""".r
  
  /* case NameRest(name, rest) */
  val NameRest = """(\w+)\.?(.*)""".r
  
  
  def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    
    -1
  }
  
  
  private def resolveRest(value: AnyRef, rest: String) :AnyRef = {
    value match {
      case s: Struct => s.resolve(rest)
      case _ => null
    }
  }
  
  def resolveAsList(index: Int, rest: String, list: AnyRef) :AnyRef = {
    if (list == null) null else list match {
      case seq: BindingSeq[_] =>
        if (index >= seq.size) null else {
          
          val element = seq(index)
          if (rest.isEmpty) element
          else resolveRest(element, rest)
        }
        
      case _ =>
        null
    }
  }
  
  def resolveAsValue(rest: String, value: AnyRef) :AnyRef = {
    if (value == null) return null
    
    if (rest.isEmpty) {
      value match {
        // Allow non-indexed path to map to the first element of a list.
        case seq: BindingSeq[_] => if (seq.size > 0) seq(0) else null
        case _ => value
      }
    }
    else {
      value match {
        // Allow non-indexed path to map to the first element of a list.
        case seq: BindingSeq[_] => if (seq.size > 0) resolveRest(seq(0), rest) else null
        case _ => resolveRest(value, rest)
      }
    }
  }
}
