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

package simplex3d.math.doublem

import simplex3d.math._


/**
 * Glue code to promote float types to double types.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class FloatPromoter(val value: Double) {
  // Vecf to Vecd promotion
  def *(u: Read2[Float]) =
    new Vec2d(u.dx*value, u.dy*value)

  def *(u: Read3[Float]) =
    new Vec3d(u.dx*value, u.dy*value, u.dz*value)

  def *(u: Read4[Float]) =
    new Vec4d(u.dx*value, u.dy*value, u.dz*value, u.dw*value)

  def /(u: Read2[Float]) =
    new Vec2d(value/u.dx, value/u.dy)

  def /(u: Read3[Float]) =
    new Vec3d(value/u.dx, value/u.dy, value/u.dz)

  def /(u: Read4[Float]) =
    new Vec4d(value/u.dx, value/u.dy, value/u.dz, value/u.dw)


  def +(u: Read2[Float]) =
    new Vec2d(value + u.dx, value + u.dy)

  def +(u: Read3[Float]) =
    new Vec3d(value + u.dx, value + u.dy, value + u.dz)

  def +(u: Read4[Float]) =
    new Vec4d(value + u.dx, value + u.dy, value + u.dz, value + u.dw)

  def -(u: Read2[Float]) =
    new Vec2d(value - u.dx, value - u.dy)

  def -(u: Read3[Float]) =
    new Vec3d(value - u.dx, value - u.dy, value - u.dz)

  def -(u: Read4[Float]) =
    new Vec4d(value - u.dx, value - u.dy, value - u.dz, value - u.dw)


  def *(q: ReadQ[Float]) =
    new Quat4d(q.da*value, q.db*value, q.dc*value, q.dd*value)

  def /(q: ReadQ[Float]) =
    new Quat4d(value/q.da, value/q.db, value/q.dc, value/q.dd)

  def +(q: ReadQ[Float]) =
    new Quat4d(value + q.da, value + q.db, value + q.dc, value + q.dd)

  def -(q: ReadQ[Float]) =
    new Quat4d(value - q.da, value - q.db, value - q.dc, value - q.dd)


  def *(m: Read2x2[Float]) = Mat2d(m)*value
  def /(m: Read2x2[Float]) = Mat2d(m).divideByComponent(value)
  def +(m: Read2x2[Float]) = Mat2d(m) + value
  def -(m: Read2x2[Float]) = { val t = -Mat2d(m); t += value; t }

  def *(m: Read2x3[Float]) = Mat2x3d(m)*value
  def /(m: Read2x3[Float]) = Mat2x3d(m).divideByComponent(value)
  def +(m: Read2x3[Float]) = Mat2x3d(m) + value
  def -(m: Read2x3[Float]) = { val t = -Mat2x3d(m); t += value; t }

  def *(m: Read2x4[Float]) = Mat2x4d(m)*value
  def /(m: Read2x4[Float]) = Mat2x4d(m).divideByComponent(value)
  def +(m: Read2x4[Float]) = Mat2x4d(m) + value
  def -(m: Read2x4[Float]) = { val t = -Mat2x4d(m); t += value; t }

  def *(m: Read3x2[Float]) = Mat3x2d(m)*value
  def /(m: Read3x2[Float]) = Mat3x2d(m).divideByComponent(value)
  def +(m: Read3x2[Float]) = Mat3x2d(m) + value
  def -(m: Read3x2[Float]) = { val t = -Mat3x2d(m); t += value; t }

  def *(m: Read3x3[Float]) = Mat3d(m)*value
  def /(m: Read3x3[Float]) = Mat3d(m).divideByComponent(value)
  def +(m: Read3x3[Float]) = Mat3d(m) + value
  def -(m: Read3x3[Float]) = { val t = -Mat3d(m); t += value; t }

  def *(m: Read3x4[Float]) = Mat3x4d(m)*value
  def /(m: Read3x4[Float]) = Mat3x4d(m).divideByComponent(value)
  def +(m: Read3x4[Float]) = Mat3x4d(m) + value
  def -(m: Read3x4[Float]) = { val t = -Mat3x4d(m); t += value; t }

  def *(m: Read4x2[Float]) = Mat4x2d(m)*value
  def /(m: Read4x2[Float]) = Mat4x2d(m).divideByComponent(value)
  def +(m: Read4x2[Float]) = Mat4x2d(m) + value
  def -(m: Read4x2[Float]) = { val t = -Mat4x2d(m); t += value; t }

  def *(m: Read4x3[Float]) = Mat4x3d(m)*value
  def /(m: Read4x3[Float]) = Mat4x3d(m).divideByComponent(value)
  def +(m: Read4x3[Float]) = Mat4x3d(m) + value
  def -(m: Read4x3[Float]) = { val t = -Mat4x3d(m); t += value; t }

  def *(m: Read4x4[Float]) = Mat4d(m)*value
  def /(m: Read4x4[Float]) = Mat4d(m).divideByComponent(value)
  def +(m: Read4x4[Float]) = Mat4d(m) + value
  def -(m: Read4x4[Float]) = { val t = -Mat4d(m); t += value; t }
}
