/*
 * Simplex3d, Property module
 * Copyright (C) 2010, Simplex3d Team
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


/**
 * @author Aleksey Nikiforov (lex)
 */
package object property {

  type PropertyValue[T] = integration.PropertyValue[T]
  type PropertyObject[T <: AnyRef] = integration.PropertyObject[T]
  type MutableValue[T] = integration.MutableInterface[T] with Mutable
  type MutablePrimitive[T <: AnyVal] = integration.MutablePrimitive[T]
  type MutableObject[T <: AnyRef] = integration.MutableObject[T]

  
  implicit def booleanToPropertyValue(v: Boolean) :PropertyValue[Boolean] =
    new MutablePrimitive(v)

  implicit def intToPropertyValue(v: Int) :PropertyValue[Int] =
    new MutablePrimitive(v)

  implicit def floatToPropertyValue(v: Float) :PropertyValue[Float] =
    new MutablePrimitive(v)

  implicit def doubleToPropertyValue(v: Double) :PropertyValue[Double] =
    new MutablePrimitive(v)
}
