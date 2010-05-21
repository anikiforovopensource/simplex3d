/*
 * Simplex3d, IntMath module
 * Copyright (C) 2009-2010 Simplex3d Team
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

import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm {

  //Implicits
  implicit def imInt(s: Int) = new ExtendedInt(s)

  // In and Out aliases
  type inVec2i = AnyVec2i
  type outVec2i = Vec2i with Implicits[Off]
  @inline implicit def outVec2i(u: Vec2i) = u.asInstanceOf[outVec2i]

  type inVec3i = AnyVec3i
  type outVec3i = Vec3i with Implicits[Off]
  @inline implicit def outVec3i(u: Vec3i) = u.asInstanceOf[outVec3i]

  type inVec4i = AnyVec4i
  type outVec4i = Vec4i with Implicits[Off]
  @inline implicit def outVec4i(u: Vec4i) = u.asInstanceOf[outVec4i]
}
