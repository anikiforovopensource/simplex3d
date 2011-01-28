/*
 * Simplex3d, CoreMath module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package simplex3d

import java.nio._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object math {

  // Implicits
  implicit def intToExtendedInt(s: Int) = new ExtendedInt(s)

  // In and Out aliases

  /** <code>in</code> prefix for Vec2b.
   * Use the prefix when declaring functions.
   */
  type inVec2b = ReadVec2b

  /** <code>out</code> prefix for Vec2b.
   * Use the prefix when declaring functions.
   */
  type outVec2b = Vec2b with Implicits[Off]
  @inline implicit def outVec2b(u: Vec2b) = u.asInstanceOf[outVec2b]


  /** <code>in</code> prefix for Vec3b.
   * Use the prefix when declaring functions.
   */
  type inVec3b = ReadVec3b

  /** <code>out</code> prefix for Vec3b.
   * Use the prefix when declaring functions.
   */
  type outVec3b = Vec3b with Implicits[Off]
  @inline implicit def outVec3b(u: Vec3b) = u.asInstanceOf[outVec3b]


  /** <code>in</code> prefix for Vec4b.
   * Use the prefix when declaring functions.
   */
  type inVec4b = ReadVec4b

  /** <code>out</code> prefix for Vec4b.
   * Use the prefix when declaring functions.
   */
  type outVec4b = Vec4b with Implicits[Off]
  @inline implicit def outVec4b(u: Vec4b) = u.asInstanceOf[outVec4b]


  type inVec2i = ReadVec2i
  type outVec2i = Vec2i with Implicits[Off]
  @inline implicit def outVec2i(u: Vec2i) = u.asInstanceOf[outVec2i]

  type inVec3i = ReadVec3i
  type outVec3i = Vec3i with Implicits[Off]
  @inline implicit def outVec3i(u: Vec3i) = u.asInstanceOf[outVec3i]

  type inVec4i = ReadVec4i
  type outVec4i = Vec4i with Implicits[Off]
  @inline implicit def outVec4i(u: Vec4i) = u.asInstanceOf[outVec4i]


  // Matrix aliases
  type AnyMat2x2[P] = AnyMat2[P]
  type AnyMat3x3[P] = AnyMat3[P]
  type AnyMat4x4[P] = AnyMat4[P]
}
