/*
 * Simplex3dEngine - Renderer Module
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
package graphics.pluggable


final class NestedSampler(
  val parentErasure: Class[_],
  val qualifiedType: String,
  val name: String,
  val arraySizeExpression: Option[String]
) {
  override def toString :String = {
    "NestedSampler(" + qualifiedType + " " + name + ")"
  }
  
  override def equals(o: Any) :Boolean = {
    if (this eq o.asInstanceOf[AnyRef]) {
      true
    }
    else o match {
      
      case d: NestedSampler =>
        parentErasure == d.parentErasure &&
        qualifiedType == d.qualifiedType &&
        name == d.name &&
        arraySizeExpression == d.arraySizeExpression
        
      case _ =>
        false
    }
  }
  
  override def hashCode :Int = {
    41 * (
      41 * (
        41 * (
          41 + parentErasure.hashCode
        ) + qualifiedType.hashCode
      ) + name.hashCode
    ) + arraySizeExpression.hashCode
  }
}
