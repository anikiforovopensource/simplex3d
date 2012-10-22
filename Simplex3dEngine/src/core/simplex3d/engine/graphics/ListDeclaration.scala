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

package simplex3d.engine
package graphics

import java.util.logging._
import simplex3d.engine.util._


final class ListDeclaration(val nameKey: ListNameKey, val lists: ReadArray[BindingList[_]]) {
  import ListDeclaration._
  
  if (lists.isEmpty) throw new IllegalArgumentException("One or more lists must be provided.")
  
  
  def sizeKey() = {
    var differentSizes = false
    var min = lists(0).size
    
    val s = lists.size; var i = 1; while (i < s) {
      val size = lists(i).size
      
      if (size != min) {
        differentSizes = true
        if (size < min) min = lists(i).size
      }
      
      i += 1
    }
    
    if (differentSizes) logger.log(
      Level.WARNING,
      this.toString() + " resolves to BindingList instances with different sizes, minimum size will be used."
    )
    
    new ListSizeKey(nameKey, min)
  }
  
  override def toString() :String = {
    "ListDeclaration(" + nameKey.parentType + "." + nameKey.name + ")"
  }
}

object ListDeclaration {
  private val logger = Logger.getLogger(this.getClass.getName)
}


final class ListNameKey(val parentType: String, val name: String) {
  override def equals(other: Any) :Boolean = {
    if (this.eq(other.asInstanceOf[AnyRef])) true
    else other match {
      case a: ListNameKey =>
        a.parentType == parentType &&
        a.name == name
      case _ => false
    }
  }
  
  override val hashCode :Int = {
    41 * (
      41 + parentType.hashCode
    ) + name.hashCode
  }
  
  override def toString() :String = {
    "ListNameKey(" + parentType + "." + name + ")"
  }
}


final class ListSizeKey(val nameKey: ListNameKey, val size: Int) {
  
  override def equals(other: Any) :Boolean = {
    if (this.eq(other.asInstanceOf[AnyRef])) true
    else other match {
      case a: ListSizeKey =>
        a.nameKey == nameKey &&
        a.size == size
      case _ => false
    }
  }
  
  override def hashCode() :Int = {
    41 * (
      41 + nameKey.hashCode
    ) + size.hashCode
  }
  
  override def toString() :String = {
    "ListSizeKey(" + nameKey.parentType + "." + nameKey.name + "[" + size + "])"
  }
}
