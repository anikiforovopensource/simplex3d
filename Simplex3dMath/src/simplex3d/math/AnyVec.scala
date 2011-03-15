/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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


/** <code>AnyVec</code> is a base class for all vectors.
 *
 * @author Aleksey Nikiforov (lex)
 */
trait AnyVec[P] extends ReadPropertyRef {
  type Clone <: AnyVec[P]
  def components: Int
  def apply(i: Int) :P

  private[math] def bx: Boolean
  private[math] def ix: Int
  private[math] def fx: Float
  private[math] def dx: Double
}
