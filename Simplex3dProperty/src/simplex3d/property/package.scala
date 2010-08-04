/*
 * Simplex3d, Property module
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dProperty.
 *
 * Simplex3dProperty is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dProperty is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object property {
  val PropertyValue = integration.property.PropertyValue
  type PropertyValue[T] = integration.property.PropertyValue[T]
  type PropertyObject[T <: AnyRef] = integration.property.PropertyObject[T]
}
