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

package simplex3d.math.integration.buffer


/** <code>MetaElement</code> is used to integrate math with sequences.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait MetaElement {
  type Element
  type Immutable <: Element
  type Component <: Primitive
}

/** <code>Primitive</code> marker indicates primitive elements/components.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Primitive extends MetaElement {
  type Element <: AnyVal
  type Immutable = Element
}

/** <code>Int1</code> marker indicates Int elements/components.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Int1 extends Primitive {
  type Element = Int
  type Component = Int1
}

/** <code>Float1</code> marker indicates Float elements/components.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Float1 extends Primitive {
  type Element = Float
  type Component = Float1
}

/** <code>Double1</code> marker indicates Double elements/components.
 *
 * @author Aleksey Nikiforov (lex)
 */
sealed trait Double1 extends Primitive {
  type Element = Double
  type Component = Double1
}

/** <code>Composite</code> marker indicates elements composed of
 * primitive components.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait Composite extends MetaElement {
  type Element <: AnyRef
}
