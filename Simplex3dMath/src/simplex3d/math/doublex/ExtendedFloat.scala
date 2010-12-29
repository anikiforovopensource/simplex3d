/*
 * Simplex3d, DoubleMath module
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

package simplex3d.math.doublex

import simplex3d.math._


/**
 * Glue code to make floats interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedFloat(val value: Float) {
  def *(u: inVec2d) = u*value
  def /(u: inVec2d) = new Vec2d(value/u.x, value/u.y)
  def +(u: inVec2d) = u + value
  def -(u: inVec2d) = new Vec2d(value - u.x, value - u.y)

  def *(u: inVec3d) = u*value
  def /(u: inVec3d) = new Vec3d(value/u.x, value/u.y, value/u.z)
  def +(u: inVec3d) = u + value
  def -(u: inVec3d) = new Vec3d(value - u.x, value - u.y, value - u.z)

  def *(u: inVec4d) = u*value
  def /(u: inVec4d) = new Vec4d(value/u.x, value/u.y, value/u.z, value/u.w)
  def +(u: inVec4d) = u + value
  def -(u: inVec4d) = new Vec4d(value - u.x, value - u.y, value - u.z, value - u.w)

  def *(q: inQuat4d) = q*value
  def /(q: inQuat4d) = new Quat4d(value/q.a, value/q.b, value/q.c, value/q.d)
  def +(q: inQuat4d) = q + value
  def -(q: inQuat4d) = new Quat4d(value - q.a, value - q.b, value - q.c, value - q.d)

  def *(m: inMat2d) = m*value
  def /(m: inMat2d) = m.divideByComponent(value)
  def +(m: inMat2d) = m + value
  def -(m: inMat2d) = { val t = -m; t += value; t }

  def *(m: inMat2x3d) = m*value
  def /(m: inMat2x3d) = m.divideByComponent(value)
  def +(m: inMat2x3d) = m + value
  def -(m: inMat2x3d) = { val t = -m; t += value; t }

  def *(m: inMat2x4d) = m*value
  def /(m: inMat2x4d) = m.divideByComponent(value)
  def +(m: inMat2x4d) = m + value
  def -(m: inMat2x4d) = { val t = -m; t += value; t }

  def *(m: inMat3x2d) = m*value
  def /(m: inMat3x2d) = m.divideByComponent(value)
  def +(m: inMat3x2d) = m + value
  def -(m: inMat3x2d) = { val t = -m; t += value; t }

  def *(m: inMat3d) = m*value
  def /(m: inMat3d) = m.divideByComponent(value)
  def +(m: inMat3d) = m + value
  def -(m: inMat3d) = { val t = -m; t += value; t }

  def *(m: inMat3x4d) = m*value
  def /(m: inMat3x4d) = m.divideByComponent(value)
  def +(m: inMat3x4d) = m + value
  def -(m: inMat3x4d) = { val t = -m; t += value; t }

  def *(m: inMat4x2d) = m*value
  def /(m: inMat4x2d) = m.divideByComponent(value)
  def +(m: inMat4x2d) = m + value
  def -(m: inMat4x2d) = { val t = -m; t += value; t }

  def *(m: inMat4x3d) = m*value
  def /(m: inMat4x3d) = m.divideByComponent(value)
  def +(m: inMat4x3d) = m + value
  def -(m: inMat4x3d) = { val t = -m; t += value; t }

  def *(m: inMat4d) = m*value
  def /(m: inMat4d) = m.divideByComponent(value)
  def +(m: inMat4d) = m + value
  def -(m: inMat4d) = { val t = -m; t += value; t }
}
