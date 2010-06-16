/*
 * Simplex3d, BaseMath module
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


/** <code>ElemType</code> is used to integrate vectors with sequences.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait ElemType {
  type Element
  type Component <: Primitive
}

/** <code>Primitive</code> is a marker for sequences with primitive elements.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Primitive extends ElemType {
  type Element <: AnyVal
}

/** <code>Int1</code> is a marker for sequences of Int elements.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Int1 extends Primitive {
  type Element = Int
  type Component = Int1
}

/** <code>Float1</code> is a marker for sequences of Float elements.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Float1 extends Primitive {
  type Element = Float
  type Component = Float1
}

/** <code>Double1</code> is a marker for sequences of Double elements.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Double1 extends Primitive {
  type Element = Double
  type Component = Double1
}

/** <code>Composite</code> is a marker for sequences with composite elemets.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Composite extends ElemType {
  type Element <: AnyRef
}
