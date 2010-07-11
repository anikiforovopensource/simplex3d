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

package simplex3d.math


import simplex3d.math.floatm.FloatMath._

/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

  // Implicits
  implicit def intToFloatPromoter(s: Float) = new IntPromoter(s)

  implicit def extendedFloatForFloat(s: Float) = new ExtendedFloat(s)
  implicit def extendedIntForFloat(s: Int) = new ExtendedInt(s)

  implicit def vec2IntToFloat(u: Read2[Int, _]) :ConstVec2f =
    new ConstVec2f(u.fx, u.fy)

  implicit def vec3IntToFloat(u: Read3[Int, _]) :ConstVec3f =
    new ConstVec3f(u.fx, u.fy, u.fz)

  implicit def vec4IntToFloat(u: Read4[Int, _]) :ConstVec4f =
    new ConstVec4f(u.fx, u.fy, u.fz, u.fw)

  
  // Matrix aliases
  type AnyMat2x2f = AnyMat2f
  type ConstMat2x2f = ConstMat2f
  val ConstMat2x2f = ConstMat2f
  type Mat2x2f = Mat2f
  val Mat2x2f = Mat2f

  type AnyMat3x3f = AnyMat3f
  type ConstMat3x3f = ConstMat3f
  val ConstMat3x3f = ConstMat3f
  type Mat3x3f = Mat3f
  val Mat3x3f = Mat3f

  type AnyMat4x4f = AnyMat4f
  type ConstMat4x4f = ConstMat4f
  val ConstMat4x4f = ConstMat4f
  type Mat4x4f = Mat4f
  val Mat4x4f = Mat4f

  // In and Out aliases
  type inVec2f = AnyVec2f
  type outVec2f = Vec2f with Implicits[Off]
  @inline implicit def outVec2f(u: Vec2f) = u.asInstanceOf[outVec2f]

  type inVec3f = AnyVec3f
  type outVec3f = Vec3f with Implicits[Off]
  @inline implicit def outVec3f(u: Vec3f) = u.asInstanceOf[outVec3f]

  type inVec4f = AnyVec4f
  type outVec4f = Vec4f with Implicits[Off]
  @inline implicit def outVec4f(u: Vec4f) = u.asInstanceOf[outVec4f]

  type inQuat4f = AnyQuat4f
  type outQuat4f = Quat4f with Implicits[Off]
  @inline implicit def outQuat4f(u: Quat4f) = u.asInstanceOf[outQuat4f]

  type inMat2f = AnyMat2f
  type outMat2f = Mat2f with Implicits[Off]
  @inline implicit def outMat2x2f(u: Mat2f) = u.asInstanceOf[outMat2f]

  type inMat2x3f = AnyMat2x3f
  type outMat2x3f = Mat2x3f with Implicits[Off]
  @inline implicit def outMat2x3f(u: Mat2x3f) = u.asInstanceOf[outMat2x3f]

  type inMat2x4f = AnyMat2x4f
  type outMat2x4f = Mat2x4f with Implicits[Off]
  @inline implicit def outMat2x4f(u: Mat2x4f) = u.asInstanceOf[outMat2x4f]

  type inMat3x2f = AnyMat3x2f
  type outMat3x2f = Mat3x2f with Implicits[Off]
  @inline implicit def outMat3x2f(u: Mat3x2f) = u.asInstanceOf[outMat3x2f]

  type inMat3f = AnyMat3f
  type outMat3f = Mat3f with Implicits[Off]
  @inline implicit def outMat3x3f(u: Mat3f) = u.asInstanceOf[outMat3f]

  type inMat3x4f = AnyMat3x4f
  type outMat3x4f = Mat3x4f with Implicits[Off]
  @inline implicit def outMat3x4f(u: Mat3x4f) = u.asInstanceOf[outMat3x4f]

  type inMat4x2f = AnyMat4x2f
  type outMat4x2f = Mat4x2f with Implicits[Off]
  @inline implicit def outMat4x2f(u: Mat4x2f) = u.asInstanceOf[outMat4x2f]

  type inMat4x3f = AnyMat4x3f
  type outMat4x3f = Mat4x3f with Implicits[Off]
  @inline implicit def outMat4x3f(u: Mat4x3f) = u.asInstanceOf[outMat4x3f]

  type inMat4f = AnyMat4f
  type outMat4f = Mat4f with Implicits[Off]
  @inline implicit def outMat4x4f(u: Mat4f) = u.asInstanceOf[outMat4f]

  // In and Out matrix aliases
  type inMat2x2f = inMat2f
  type outMat2x2f = outMat2f

  type inMat3x3f = inMat3f
  type outMat3x3f = outMat3f

  type inMat4x4f = inMat4f
  type outMat4x4f = outMat4f
}
