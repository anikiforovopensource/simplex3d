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


final class ArrayDeclaration(val parentType: String, val name: String, val size: Int) {
  val key = (parentType, name)
  
  override def equals(other: Any) :Boolean = {
    if (this.eq(other.asInstanceOf[AnyRef])) true
    else other match {
      case a: ArrayDeclaration =>
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
    "ArrayDeclaration { " + parentType + "." + name + "[" + size + "] }"
  }
}
