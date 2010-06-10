/*
 * Simplex3d, FloatMath module
 * Copyright (C) 2009-2010, Simplex3d Team
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

package simplex3d.math.floatm

import simplex3d.math._


/**
 * Glue code to make ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {
  def *(u: inVec2f) = u*value
  def *(u: inVec3f) = u*value
  def *(u: inVec4f) = u*value

  def *(q: inQuat4f) = q*value

  def *(m: inMat2f) = m*value
  def *(m: inMat2x3f) = m*value
  def *(m: inMat2x4f) = m*value
  def *(m: inMat3x2f) = m*value
  def *(m: inMat3f) = m*value
  def *(m: inMat3x4f) = m*value
  def *(m: inMat4x2f) = m*value
  def *(m: inMat4x3f) = m*value
  def *(m: inMat4f) = m*value

  def /(u: inVec2f) = u.divideByComponent(value)
  def /(u: inVec3f) = u.divideByComponent(value)
  def /(u: inVec4f) = u.divideByComponent(value)

  def /(q: inQuat4f) = q.divideByComponent(value)

  def /(m: inMat2f) = m.divideByComponent(value)
  def /(m: inMat2x3f) = m.divideByComponent(value)
  def /(m: inMat2x4f) = m.divideByComponent(value)
  def /(m: inMat3x2f) = m.divideByComponent(value)
  def /(m: inMat3f) = m.divideByComponent(value)
  def /(m: inMat3x4f) = m.divideByComponent(value)
  def /(m: inMat4x2f) = m.divideByComponent(value)
  def /(m: inMat4x3f) = m.divideByComponent(value)
  def /(m: inMat4f) = m.divideByComponent(value)

  def +(u: inVec2f) = u + value
  def +(u: inVec3f) = u + value
  def +(u: inVec4f) = u + value

  def +(q: inQuat4f) = q + value

  def +(m: inMat2f) = m + value
  def +(m: inMat2x3f) = m + value
  def +(m: inMat2x4f) = m + value
  def +(m: inMat3x2f) = m + value
  def +(m: inMat3f) = m + value
  def +(m: inMat3x4f) = m + value
  def +(m: inMat4x2f) = m + value
  def +(m: inMat4x3f) = m + value
  def +(m: inMat4f) = m + value

  def -(u: inVec2f) =
    new Vec2f(value - u.x, value - u.y)

  def -(u: inVec3f) =
    new Vec3f(value - u.x, value - u.y, value - u.z)

  def -(u: inVec4f) =
    new Vec4f(value - u.x, value - u.y, value - u.z, value - u.w)
  

  def -(q: inQuat4f) = { val t = -q; t += value; t }

  def -(m: inMat2f) = { val t = -m; t += value; t }
  def -(m: inMat2x3f) = { val t = -m; t += value; t }
  def -(m: inMat2x4f) = { val t = -m; t += value; t }
  def -(m: inMat3x2f) = { val t = -m; t += value; t }
  def -(m: inMat3f) = { val t = -m; t += value; t }
  def -(m: inMat3x4f) = { val t = -m; t += value; t }
  def -(m: inMat4x2f) = { val t = -m; t += value; t }
  def -(m: inMat4x3f) = { val t = -m; t += value; t }
  def -(m: inMat4f) = { val t = -m; t += value; t }
}
