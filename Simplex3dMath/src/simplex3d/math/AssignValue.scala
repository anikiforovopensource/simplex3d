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


/** <code>AssignValue</code> is a trait for all mutable math objects.
 * It allows uniform treatment for all the objects with := operator.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait AssignValue[T] extends Mutable {
  def :=(value: T) :Unit
}
