/*
 * Simplex3d, BaseMath module
 * Copyright (C) 2010 Simplex3d Team
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


trait MetaType {
  type Element
  type Component <: Primitive
}

sealed trait Primitive extends MetaType {
  type Element <: AnyVal
}

sealed trait Int1 extends Primitive {
  type Element = Int
  type Component = Int1
}

sealed trait Float1 extends Primitive {
  type Element = Float
  type Component = Float1
}

sealed trait Double1 extends Primitive {
  type Element = Double
  type Component = Double1
}

trait Composite extends MetaType {
  type Element <: AnyRef
}
