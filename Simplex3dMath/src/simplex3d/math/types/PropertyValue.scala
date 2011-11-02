/*
 * Simplex3dMath - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dMath.
 *
 * Simplex3dMath is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMath is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.math.types

import simplex3d.math.integration._


/** 
 * @author Aleksey Nikiforov (lex)
 */
trait ReadPropertyValue[W <: PropertyValue[W]] extends Readable[W] with Cloneable[ReadPropertyValue[W]] { self: W#Read =>
  def toConst() :W#Const
  def mutableCopy() :W
}


/** 
 * @author Aleksey Nikiforov (lex)
 */
trait PropertyValue[W <: PropertyValue[W]] extends ReadPropertyValue[W] with Writable[W] { self: W =>
  type Read >: W <: ReadPropertyValue[W]
  type Const
  
  def :=(v: W#Const)
}
