/*
 * Simplex3d, CoreMath module
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

package simplex3d.math

import simplex3d.math.integration._


/** 
 * @author Aleksey Nikiforov (lex)
 */
trait Cloneable {
  type Clone <: Cloneable

  override def clone() :Clone = throw new UnsupportedOperationException()
}


trait Readable[R <: Readable[R]] { self: R =>
  type Mutable <: Readable[R] with scala.Mutable
  
  def mutableCopy() :R#Mutable
}


trait ReadPropertyRef[R <: ReadPropertyRef[R]] extends Readable[R] with Cloneable { self: R =>
  type Clone <: ReadPropertyRef[R] with Cloneable
  type Const
  type Mutable <: PropertyRef[R] with Cloneable

  def toConst() :R#Const
  def mutableCopy() :R#Mutable
}


trait PropertyRef[R <: ReadPropertyRef[R]] extends ReadPropertyRef[R] with Mutable { self: R =>
  type Clone <: PropertyRef[R] with Cloneable
  
  def :=(v: R) // Cleaner alternative is "def :=(v: ReadPropertyRef[R])"
  def :=(v: R#Const)
}
