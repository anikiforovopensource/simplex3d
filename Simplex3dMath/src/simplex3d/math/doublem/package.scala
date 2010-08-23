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

package simplex3d.math

import simplex3d.math.doublem.DoubleMath._
import simplex3d.math.integration.property._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

  // Implicits
  implicit def intToDoublePromoter(s: Double) = new IntPromoter(s)
  implicit def floatToDoublePromoter(s: Double) = new FloatPromoter(s)

  implicit def extendedDoubleForDouble(s: Double) = new ExtendedDouble(s)
  implicit def extendedFloatForDouble(s: Float) = new ExtendedFloat(s)
  implicit def extendedIntForDouble(s: Int) = new ExtendedInt(s)

  implicit def vec2IntToDouble(u: AnyVec2[Int]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def vec3IntToDouble(u: AnyVec3[Int]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def vec4IntToDouble(u: AnyVec4[Int]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)

  implicit def vec2FloatToDouble(u: AnyVec2[Float]) :ConstVec2d =
    new ConstVec2d(u.dx, u.dy)

  implicit def vec3FloatToDouble(u: AnyVec3[Float]) :ConstVec3d =
    new ConstVec3d(u.dx, u.dy, u.dz)

  implicit def vec4FloatToDouble(u: AnyVec4[Float]) :ConstVec4d =
    new ConstVec4d(u.dx, u.dy, u.dz, u.dw)


  implicit def quat4FloatToDouble(q: AnyQuat4[Float]) :ConstQuat4d =
    new ConstQuat4d(q.da, q.db, q.dc, q.dd)

  implicit def mat2x2FloatToDouble(m: AnyMat2x2[Float]) :ConstMat2d =
    ConstMat2d(m)
  
  implicit def mat2x3FloatToDouble(m: AnyMat2x3[Float]) :ConstMat2x3d =
    ConstMat2x3d(m)
  
  implicit def mat2x4FloatToDouble(m: AnyMat2x4[Float]) :ConstMat2x4d =
    ConstMat2x4d(m)
  
  implicit def mat3x2FloatToDouble(m: AnyMat3x2[Float]) :ConstMat3x2d =
    ConstMat3x2d(m)
  
  implicit def mat3x3FloatToDouble(m: AnyMat3x3[Float]) :ConstMat3d =
    ConstMat3d(m)
  
  implicit def mat3x4FloatToDouble(m: AnyMat3x4[Float]) :ConstMat3x4d =
    ConstMat3x4d(m)
  
  implicit def mat4x2FloatToDouble(m: AnyMat4x2[Float]) :ConstMat4x2d =
    ConstMat4x2d(m)
  
  implicit def mat4x3FloatToDouble(m: AnyMat4x3[Float]) :ConstMat4x3d =
    ConstMat4x3d(m)
  
  implicit def mat4x4FloatToDouble(m: AnyMat4x4[Float]) :ConstMat4d =
    ConstMat4d(m)


  // Matrix aliases
  type ReadMat2x2d = ReadMat2d
  type ConstMat2x2d = ConstMat2d
  val ConstMat2x2d = ConstMat2d
  type Mat2x2d = Mat2d
  val Mat2x2d = Mat2d

  type ReadMat3x3d = ReadMat3d
  type ConstMat3x3d = ConstMat3d
  val ConstMat3x3d = ConstMat3d
  type Mat3x3d = Mat3d
  val Mat3x3d = Mat3d

  type ReadMat4x4d = ReadMat4d
  type ConstMat4x4d = ConstMat4d
  val ConstMat4x4d = ConstMat4d
  type Mat4x4d = Mat4d
  val Mat4x4d = Mat4d

  // In and Out aliases
  type inVec2d = ReadVec2d
  type outVec2d = Vec2d with Implicits[Off]
  @inline implicit def outVec2d(u: Vec2d) = u.asInstanceOf[outVec2d]

  type inVec3d = ReadVec3d
  type outVec3d = Vec3d with Implicits[Off]
  @inline implicit def outVec3d(u: Vec3d) = u.asInstanceOf[outVec3d]

  type inVec4d = ReadVec4d
  type outVec4d = Vec4d with Implicits[Off]
  @inline implicit def outVec4d(u: Vec4d) = u.asInstanceOf[outVec4d]

  type inQuat4d = ReadQuat4d
  type outQuat4d = Quat4d with Implicits[Off]
  @inline implicit def outQuat4d(u: Quat4d) = u.asInstanceOf[outQuat4d]

  type inMat2d = ReadMat2d
  type outMat2d = Mat2d with Implicits[Off]
  @inline implicit def outMat2x2d(u: Mat2d) = u.asInstanceOf[outMat2d]

  type inMat2x3d = ReadMat2x3d
  type outMat2x3d = Mat2x3d with Implicits[Off]
  @inline implicit def outMat2x3d(u: Mat2x3d) = u.asInstanceOf[outMat2x3d]

  type inMat2x4d = ReadMat2x4d
  type outMat2x4d = Mat2x4d with Implicits[Off]
  @inline implicit def outMat2x4d(u: Mat2x4d) = u.asInstanceOf[outMat2x4d]

  type inMat3x2d = ReadMat3x2d
  type outMat3x2d = Mat3x2d with Implicits[Off]
  @inline implicit def outMat3x2d(u: Mat3x2d) = u.asInstanceOf[outMat3x2d]

  type inMat3d = ReadMat3d
  type outMat3d = Mat3d with Implicits[Off]
  @inline implicit def outMat3x3d(u: Mat3d) = u.asInstanceOf[outMat3d]

  type inMat3x4d = ReadMat3x4d
  type outMat3x4d = Mat3x4d with Implicits[Off]
  @inline implicit def outMat3x4d(u: Mat3x4d) = u.asInstanceOf[outMat3x4d]

  type inMat4x2d = ReadMat4x2d
  type outMat4x2d = Mat4x2d with Implicits[Off]
  @inline implicit def outMat4x2d(u: Mat4x2d) = u.asInstanceOf[outMat4x2d]

  type inMat4x3d = ReadMat4x3d
  type outMat4x3d = Mat4x3d with Implicits[Off]
  @inline implicit def outMat4x3d(u: Mat4x3d) = u.asInstanceOf[outMat4x3d]

  type inMat4d = ReadMat4d
  type outMat4d = Mat4d with Implicits[Off]
  @inline implicit def outMat4x4d(u: Mat4d) = u.asInstanceOf[outMat4d]

  // In and Out matrix aliases
  type inMat2x2d = inMat2d
  type outMat2x2d = outMat2d

  type inMat3x3d = inMat3d
  type outMat3x3d = outMat3d

  type inMat4x4d = inMat4d
  type outMat4x4d = outMat4d
}
