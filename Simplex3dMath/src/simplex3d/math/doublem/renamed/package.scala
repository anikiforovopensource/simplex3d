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
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object renamed {

  // Implicits
  implicit def intToDoublePromoter(s: Double) = new IntPromoter(s)
  implicit def floatToDoublePromoter(s: Double) = new FloatPromoter(s)

  implicit def extendedDoubleForDouble(s: Double) = new ExtendedDouble(s)
  implicit def extendedFloatForDouble(s: Float) = new ExtendedFloat(s)
  implicit def extendedIntForDouble(s: Int) = new ExtendedInt(s)

  implicit def vec2IntToDouble(u: Read2[Int, _]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def vec3IntToDouble(u: Read3[Int, _]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def vec4IntToDouble(u: Read4[Int, _]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)

  implicit def vec2FloatToDouble(u: Read2[Float, _]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def vec3FloatToDouble(u: Read3[Float, _]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def vec4FloatToDouble(u: Read4[Float, _]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)


  implicit def quat4FloatToDouble(q: ReadQ[Float, _]) :ConstQuat4d =
    new ConstQuat4d(q.da, q.db, q.dc, q.dd)

  implicit def mat2x2FloatToDouble(m: Read2x2[Float, _]) :ConstMat2d =
    ConstMat2d(m)

  implicit def mat2x3FloatToDouble(m: Read2x3[Float, _]) :ConstMat2x3d =
    ConstMat2x3d(m)

  implicit def mat2x4FloatToDouble(m: Read2x4[Float, _]) :ConstMat2x4d =
    ConstMat2x4d(m)

  implicit def mat3x2FloatToDouble(m: Read3x2[Float, _]) :ConstMat3x2d =
    ConstMat3x2d(m)

  implicit def mat3x3FloatToDouble(m: Read3x3[Float, _]) :ConstMat3d =
    ConstMat3d(m)

  implicit def mat3x4FloatToDouble(m: Read3x4[Float, _]) :ConstMat3x4d =
    ConstMat3x4d(m)

  implicit def mat4x2FloatToDouble(m: Read4x2[Float, _]) :ConstMat4x2d =
    ConstMat4x2d(m)

  implicit def mat4x3FloatToDouble(m: Read4x3[Float, _]) :ConstMat4x3d =
    ConstMat4x3d(m)

  implicit def mat4x4FloatToDouble(m: Read4x4[Float, _]) :ConstMat4d =
    ConstMat4d(m)
  

  val DoubleMath = doublem.DoubleMath

  type AnyVec2 = AnyVec2d
  type ConstVec2 = ConstVec2d
  val ConstVec2 = ConstVec2d
  type Vec2 = Vec2d
  val Vec2 = Vec2d

  type AnyVec3 = AnyVec3d
  type ConstVec3 = ConstVec3d
  val ConstVec3 = ConstVec3d
  type Vec3 = Vec3d
  val Vec3 = Vec3d

  type AnyVec4 = AnyVec4d
  type ConstVec4 = ConstVec4d
  val ConstVec4 = ConstVec4d
  type Vec4 = Vec4d
  val Vec4 = Vec4d

  type AnyMat2 = AnyMat2d
  type ConstMat2 = ConstMat2d
  val ConstMat2 = ConstMat2d
  type Mat2 = Mat2d
  val Mat2 = Mat2d

  type AnyMat2x3 = AnyMat2x3d
  type ConstMat2x3 = ConstMat2x3d
  val ConstMat2x3 = ConstMat2x3d
  type Mat2x3 = Mat2x3d
  val Mat2x3 = Mat2x3d

  type AnyMat2x4 = AnyMat2x4d
  type ConstMat2x4 = ConstMat2x4d
  val ConstMat2x4 = ConstMat2x4d
  type Mat2x4 = Mat2x4d
  val Mat2x4 = Mat2x4d

  type AnyMat3x2 = AnyMat3x2d
  type ConstMat3x2 = ConstMat3x2d
  val ConstMat3x2 = ConstMat3x2d
  type Mat3x2 = Mat3x2d
  val Mat3x2 = Mat3x2d

  type AnyMat3 = AnyMat3d
  type ConstMat3 = ConstMat3d
  val ConstMat3 = ConstMat3d
  type Mat3 = Mat3d
  val Mat3 = Mat3d

  type AnyMat3x4 = AnyMat3x4d
  type ConstMat3x4 = ConstMat3x4d
  val ConstMat3x4 = ConstMat3x4d
  type Mat3x4 = Mat3x4d
  val Mat3x4 = Mat3x4d

  type AnyMat4x2 = AnyMat4x2d
  type ConstMat4x2 = ConstMat4x2d
  val ConstMat4x2 = ConstMat4x2d
  type Mat4x2 = Mat4x2d
  val Mat4x2 = Mat4x2d

  type AnyMat4x3 = AnyMat4x3d
  type ConstMat4x3 = ConstMat4x3d
  val ConstMat4x3 = ConstMat4x3d
  type Mat4x3 = Mat4x3d
  val Mat4x3 = Mat4x3d

  type AnyMat4 = AnyMat4d
  type ConstMat4 = ConstMat4d
  val ConstMat4 = ConstMat4d
  type Mat4 = Mat4d
  val Mat4 = Mat4d

  type AnyQuat4 = AnyQuat4d
  type ConstQuat4 = ConstQuat4d
  val ConstQuat4 = ConstQuat4d
  type Quat4 = Quat4d
  val Quat4 = Quat4d

  // Matrix aliases
  type AnyMat2x2 = AnyMat2d
  type ConstMat2x2 = ConstMat2d
  val ConstMat2x2 = ConstMat2d
  type Mat2x2 = Mat2d
  val Mat2x2 = Mat2d

  type AnyMat3x3 = AnyMat3d
  type ConstMat3x3 = ConstMat3d
  val ConstMat3x3 = ConstMat3d
  type Mat3x3 = Mat3d
  val Mat3x3 = Mat3d

  type AnyMat4x4 = AnyMat4d
  type ConstMat4x4 = ConstMat4d
  val ConstMat4x4 = ConstMat4d
  type Mat4x4 = Mat4d
  val Mat4x4 = Mat4d

  // In and Out aliases
  type inVec2 = AnyVec2
  type outVec2 = Vec2 with Implicits[Off]
  @inline implicit def outVec2d(u: Vec2) = u.asInstanceOf[outVec2]

  type inVec3 = AnyVec3
  type outVec3 = Vec3 with Implicits[Off]
  @inline implicit def outVec3d(u: Vec3) = u.asInstanceOf[outVec3]

  type inVec4 = AnyVec4
  type outVec4 = Vec4 with Implicits[Off]
  @inline implicit def outVec4d(u: Vec4) = u.asInstanceOf[outVec4]

  type inQuat4 = AnyQuat4
  type outQuat4 = Quat4 with Implicits[Off]
  @inline implicit def outQuat4d(u: Quat4) = u.asInstanceOf[outQuat4]

  type inMat2 = AnyMat2
  type outMat2 = Mat2 with Implicits[Off]
  @inline implicit def outMat2x2d(u: Mat2) = u.asInstanceOf[outMat2]

  type inMat2x3 = AnyMat2x3
  type outMat2x3 = Mat2x3 with Implicits[Off]
  @inline implicit def outMat2x3d(u: Mat2x3) = u.asInstanceOf[outMat2x3]

  type inMat2x4 = AnyMat2x4
  type outMat2x4 = Mat2x4 with Implicits[Off]
  @inline implicit def outMat2x4d(u: Mat2x4) = u.asInstanceOf[outMat2x4]

  type inMat3x2 = AnyMat3x2
  type outMat3x2 = Mat3x2 with Implicits[Off]
  @inline implicit def outMat3x2d(u: Mat3x2) = u.asInstanceOf[outMat3x2]

  type inMat3 = AnyMat3
  type outMat3 = Mat3 with Implicits[Off]
  @inline implicit def outMat3x3d(u: Mat3) = u.asInstanceOf[outMat3]

  type inMat3x4 = AnyMat3x4
  type outMat3x4 = Mat3x4 with Implicits[Off]
  @inline implicit def outMat3x4d(u: Mat3x4) = u.asInstanceOf[outMat3x4]

  type inMat4x2 = AnyMat4x2
  type outMat4x2 = Mat4x2 with Implicits[Off]
  @inline implicit def outMat4x2d(u: Mat4x2) = u.asInstanceOf[outMat4x2]

  type inMat4x3 = AnyMat4x3
  type outMat4x3 = Mat4x3 with Implicits[Off]
  @inline implicit def outMat4x3d(u: Mat4x3) = u.asInstanceOf[outMat4x3]

  type inMat4 = AnyMat4
  type outMat4 = Mat4 with Implicits[Off]
  @inline implicit def outMat4x4d(u: Mat4) = u.asInstanceOf[outMat4]

  // In and Out matrix aliases
  type inMat2x2 = inMat2
  type outMat2x2 = outMat2

  type inMat3x3 = inMat3
  type outMat3x3 = outMat3

  type inMat4x4 = inMat4
  type outMat4x4 = outMat4
}
