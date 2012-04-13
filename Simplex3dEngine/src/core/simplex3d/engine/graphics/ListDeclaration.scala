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


final class ListDeclaration(val parentType: String, val name: String, val lists: ReadArray[BindingList[_]]) {
  import ListDeclaration._
  
  require(!lists.isEmpty)
  
  val nameKey = (parentType, name)
  
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
      "ListDeclaration '" + parentType + "." + name +
      "' resolves to BindingList instances with different sizes, minimum size will be used."
    )
    
    new ListDeclarationSizeKey(parentType, name, min)
  }
  
  override def toString() :String = {
    "ListDeclaration { " + parentType + "." + name + "[" + "] }"
  }
}

object ListDeclaration {
  private val logger = Logger.getLogger(this.getClass.getName)
}

final class ListDeclarationSizeKey(val parentType: String, val name: String, val size: Int) {
  val nameKey = (parentType, name)
  
  override def equals(other: Any) :Boolean = {
    if (this.eq(other.asInstanceOf[AnyRef])) true
    else other match {
      case a: ListDeclarationSizeKey =>
        a.parentType == parentType &&
        a.name == name &&
        a.size == size
      case _ => false
    }
  }
  
  override def hashCode() :Int = {
    41 * (
      41 * (
        41 + parentType.hashCode
      ) + name.hashCode
    ) + size.hashCode
  }
  
  override def toString() :String = {
    "ListDeclarationSizeKey { " + parentType + "." + name + "[" + size + "] }"
  }
}
