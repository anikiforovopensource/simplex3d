/*
 * Simplex3d, DoubleMath module
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

import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  // Implicits
  implicit def dmIntPromoter(s: Double) = new IntPromoter(s)
  implicit def dmFloatPromoter(s: Double) = new FloatPromoter(s)

  implicit def dmDouble(s: Double) = new ExtendedDouble(s)
  implicit def dmFloat(s: Float) = new ExtendedFloat(s)
  implicit def dmInt(s: Int) = new ExtendedInt(s)

  implicit def dmcastVec2i(u: Read2[Int]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def dmcastVec3i(u: Read3[Int]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def dmcastVec4i(u: Read4[Int]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)

  implicit def dmcastVec2f(u: Read2[Float]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def dmcastVec3f(u: Read3[Float]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def dmcastVec4f(u: Read4[Float]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)


  // Matrix aliases
  type AnyMat2x2d = AnyMat2d
  type ConstMat2x2d = ConstMat2d
  val ConstMat2x2d = ConstMat2d
  type Mat2x2d = Mat2d
  val Mat2x2d = Mat2d

  type AnyMat3x3d = AnyMat3d
  type ConstMat3x3d = ConstMat3d
  val ConstMat3x3d = ConstMat3d
  type Mat3x3d = Mat3d
  val Mat3x3d = Mat3d

  type AnyMat4x4d = AnyMat4d
  type ConstMat4x4d = ConstMat4d
  val ConstMat4x4d = ConstMat4d
  type Mat4x4d = Mat4d
  val Mat4x4d = Mat4d

  // In and Out aliases
  type inVec2d = AnyVec2d
  type outVec2d = Vec2d with Implicits[Off]
  implicit def dmOut2(u: Vec2d) = u.asInstanceOf[outVec2d]

  type inVec3d = AnyVec3d
  type outVec3d = Vec3d with Implicits[Off]
  implicit def dmOut3(u: Vec3d) = u.asInstanceOf[outVec3d]

  type inVec4d = AnyVec4d
  type outVec4d = Vec4d with Implicits[Off]
  implicit def dmOut4(u: Vec4d) = u.asInstanceOf[outVec4d]

  type inQuat4d = AnyQuat4d
  type outQuat4d = Quat4d with Implicits[Off]
  implicit def dmOutQuat(u: Quat4d) = u.asInstanceOf[outQuat4d]

  type inMat2d = AnyMat2d
  type outMat2d = Mat2d with Implicits[Off]
  implicit def dmOut2x2(u: Mat2d) = u.asInstanceOf[outMat2d]

  type inMat2x3d = AnyMat2x3d
  type outMat2x3d = Mat2x3d with Implicits[Off]
  implicit def dmOut2x3(u: Mat2x3d) = u.asInstanceOf[outMat2x3d]

  type inMat2x4d = AnyMat2x4d
  type outMat2x4d = Mat2x4d with Implicits[Off]
  implicit def dmOut2x4(u: Mat2x4d) = u.asInstanceOf[outMat2x4d]

  type inMat3x2d = AnyMat3x2d
  type outMat3x2d = Mat3x2d with Implicits[Off]
  implicit def dmOut3x2(u: Mat3x2d) = u.asInstanceOf[outMat3x2d]

  type inMat3d = AnyMat3d
  type outMat3d = Mat3d with Implicits[Off]
  implicit def dmOut3x3(u: Mat3d) = u.asInstanceOf[outMat3d]

  type inMat3x4d = AnyMat3x4d
  type outMat3x4d = Mat3x4d with Implicits[Off]
  implicit def dmOut3x4(u: Mat3x4d) = u.asInstanceOf[outMat3x4d]

  type inMat4x2d = AnyMat4x2d
  type outMat4x2d = Mat4x2d with Implicits[Off]
  implicit def dmOut4x2(u: Mat4x2d) = u.asInstanceOf[outMat4x2d]

  type inMat4x3d = AnyMat4x3d
  type outMat4x3d = Mat4x3d with Implicits[Off]
  implicit def dmOut4x3(u: Mat4x3d) = u.asInstanceOf[outMat4x3d]

  type inMat4d = AnyMat4d
  type outMat4d = Mat4d with Implicits[Off]
  implicit def dmOut4x4(u: Mat4d) = u.asInstanceOf[outMat4d]

  // In and Out matrix aliases
  type inMat2x2d = inMat2d
  type outMat2x2d = outMat2d

  type inMat3x3d = inMat3d
  type outMat3x3d = outMat3d

  type inMat4x4d = inMat4d
  type outMat4x4d = outMat4d
}
