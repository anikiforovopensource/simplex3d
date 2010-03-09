/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object renamed {
  
  // Implicits
  implicit def fmIntPromoter(s: Float) = new IntPromoter(s)

  implicit def fmFloat(s: Float) = new ExtendedFloat(s)
  implicit def fmInt(s: Int) = new ExtendedInt(s)

  implicit def fmcastVec2i(u: Read2[Int]) :ConstVec2f =
    new ConstVec2f(u.fx, u.fy)

  implicit def fmcastVec3i(u: Read3[Int]) :ConstVec3f =
    new ConstVec3f(u.fx, u.fy, u.fz)

  implicit def fmcastVec4i(u: Read4[Int]) :ConstVec4f =
    new ConstVec4f(u.fx, u.fy, u.fz, u.fw)


  val FloatMath = floatm.FloatMath

  type AnyVec2 = AnyVec2f
  type ConstVec2 = ConstVec2f
  val ConstVec2 = ConstVec2f
  type Vec2 = Vec2f
  val Vec2 = Vec2f

  type AnyVec3 = AnyVec3f
  type ConstVec3 = ConstVec3f
  val ConstVec3 = ConstVec3f
  type Vec3 = Vec3f
  val Vec3 = Vec3f

  type AnyVec4 = AnyVec4f
  type ConstVec4 = ConstVec4f
  val ConstVec4 = ConstVec4f
  type Vec4 = Vec4f
  val Vec4 = Vec4f

  type AnyMat2 = AnyMat2f
  type ConstMat2 = ConstMat2f
  val ConstMat2 = ConstMat2f
  type Mat2 = Mat2f
  val Mat2 = Mat2f

  type AnyMat2x3 = AnyMat2x3f
  type ConstMat2x3 = ConstMat2x3f
  val ConstMat2x3 = ConstMat2x3f
  type Mat2x3 = Mat2x3f
  val Mat2x3 = Mat2x3f

  type AnyMat2x4 = AnyMat2x4f
  type ConstMat2x4 = ConstMat2x4f
  val ConstMat2x4 = ConstMat2x4f
  type Mat2x4 = Mat2x4f
  val Mat2x4 = Mat2x4f

  type AnyMat3x2 = AnyMat3x2f
  type ConstMat3x2 = ConstMat3x2f
  val ConstMat3x2 = ConstMat3x2f
  type Mat3x2 = Mat3x2f
  val Mat3x2 = Mat3x2f

  type AnyMat3 = AnyMat3f
  type ConstMat3 = ConstMat3f
  val ConstMat3 = ConstMat3f
  type Mat3 = Mat3f
  val Mat3 = Mat3f

  type AnyMat3x4 = AnyMat3x4f
  type ConstMat3x4 = ConstMat3x4f
  val ConstMat3x4 = ConstMat3x4f
  type Mat3x4 = Mat3x4f
  val Mat3x4 = Mat3x4f

  type AnyMat4x2 = AnyMat4x2f
  type ConstMat4x2 = ConstMat4x2f
  val ConstMat4x2 = ConstMat4x2f
  type Mat4x2 = Mat4x2f
  val Mat4x2 = Mat4x2f

  type AnyMat4x3 = AnyMat4x3f
  type ConstMat4x3 = ConstMat4x3f
  val ConstMat4x3 = ConstMat4x3f
  type Mat4x3 = Mat4x3f
  val Mat4x3 = Mat4x3f

  type AnyMat4 = AnyMat4f
  type ConstMat4 = ConstMat4f
  val ConstMat4 = ConstMat4f
  type Mat4 = Mat4f
  val Mat4 = Mat4f

  type AnyQuat4 = AnyQuat4f
  type ConstQuat4 = ConstQuat4f
  val ConstQuat4 = ConstQuat4f
  type Quat4 = Quat4f
  val Quat4 = Quat4f

  // Matrix aliases
  type AnyMat2x2 = AnyMat2f
  type ConstMat2x2 = ConstMat2f
  val ConstMat2x2 = ConstMat2f
  type Mat2x2 = Mat2f
  val Mat2x2 = Mat2f

  type AnyMat3x3 = AnyMat3f
  type ConstMat3x3 = ConstMat3f
  val ConstMat3x3 = ConstMat3f
  type Mat3x3 = Mat3f
  val Mat3x3 = Mat3f

  type AnyMat4x4 = AnyMat4f
  type ConstMat4x4 = ConstMat4f
  val ConstMat4x4 = ConstMat4f
  type Mat4x4 = Mat4f
  val Mat4x4 = Mat4f

  // In and Out aliases
  type inVec2 = AnyVec2
  type outVec2 = Vec2 with Implicits[Off]
  implicit def fmOut2(u: Vec2) = u.asInstanceOf[outVec2]

  type inVec3 = AnyVec3
  type outVec3 = Vec3 with Implicits[Off]
  implicit def fmOut3(u: Vec3) = u.asInstanceOf[outVec3]

  type inVec4 = AnyVec4
  type outVec4 = Vec4 with Implicits[Off]
  implicit def fmOut4(u: Vec4) = u.asInstanceOf[outVec4]

  type inQuat4 = AnyQuat4
  type outQuat4 = Quat4 with Implicits[Off]
  implicit def fmOutQuat(u: Quat4) = u.asInstanceOf[outQuat4]

  type inMat2 = AnyMat2
  type outMat2 = Mat2 with Implicits[Off]
  implicit def fmOut2x2(u: Mat2) = u.asInstanceOf[outMat2]

  type inMat2x3 = AnyMat2x3
  type outMat2x3 = Mat2x3 with Implicits[Off]
  implicit def fmOut2x3(u: Mat2x3) = u.asInstanceOf[outMat2x3]

  type inMat2x4 = AnyMat2x4
  type outMat2x4 = Mat2x4 with Implicits[Off]
  implicit def fmOut2x4(u: Mat2x4) = u.asInstanceOf[outMat2x4]

  type inMat3x2 = AnyMat3x2
  type outMat3x2 = Mat3x2 with Implicits[Off]
  implicit def fmOut3x2(u: Mat3x2) = u.asInstanceOf[outMat3x2]

  type inMat3 = AnyMat3
  type outMat3 = Mat3 with Implicits[Off]
  implicit def fmOut3x3(u: Mat3) = u.asInstanceOf[outMat3]

  type inMat3x4 = AnyMat3x4
  type outMat3x4 = Mat3x4 with Implicits[Off]
  implicit def fmOut3x4(u: Mat3x4) = u.asInstanceOf[outMat3x4]

  type inMat4x2 = AnyMat4x2
  type outMat4x2 = Mat4x2 with Implicits[Off]
  implicit def fmOut4x2(u: Mat4x2) = u.asInstanceOf[outMat4x2]

  type inMat4x3 = AnyMat4x3
  type outMat4x3 = Mat4x3 with Implicits[Off]
  implicit def fmOut4x3(u: Mat4x3) = u.asInstanceOf[outMat4x3]

  type inMat4 = AnyMat4
  type outMat4 = Mat4 with Implicits[Off]
  implicit def fmOut4x4(u: Mat4) = u.asInstanceOf[outMat4]

  // In and Out matrix aliases
  type inMat2x2 = inMat2
  type outMat2x2 = outMat2

  type inMat3x3 = inMat3
  type outMat3x3 = outMat3

  type inMat4x4 = inMat4
  type outMat4x4 = outMat4
}
