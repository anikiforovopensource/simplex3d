/*
 * Simplex3dMath - Float Module
 * Copyright (C) 2009-2011, Aleksey Nikiforov
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

import scala.language.implicitConversions
import simplex3d.math.types._
import simplex3d.math.floatx._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object float extends ImplicitMathContext {
  
  // Implicits
  implicit def intToFloatRef(s: Int) :ReadFloatRef = new FloatRef(s)
  implicit def floatToRef(s: Float) :ReadFloatRef = new FloatRef(s)
  implicit def refToFloat(r: ReadFloatRef) = r.toConst

  implicit def vec2IntToFloat(u: AnyVec2[Int]) :ConstVec2f =
    new ConstVec2f(u.fx, u.fy)

  implicit def vec3IntToFloat(u: AnyVec3[Int]) :ConstVec3f =
    new ConstVec3f(u.fx, u.fy, u.fz)

  implicit def vec4IntToFloat(u: AnyVec4[Int]) :ConstVec4f =
    new ConstVec4f(u.fx, u.fy, u.fz, u.fw)


  type ReadFloatRef = floatx.ReadFloatRef
  type FloatRef = floatx.FloatRef
  val FloatRef = floatx.FloatRef
  val functions = floatx.functions

  type ReadVec2 = ReadVec2f
  type ConstVec2 = ConstVec2f
  val ConstVec2 = ConstVec2f
  type Vec2 = Vec2f
  val Vec2 = Vec2f

  type ReadVec3 = ReadVec3f
  type ConstVec3 = ConstVec3f
  val ConstVec3 = ConstVec3f
  type Vec3 = Vec3f
  val Vec3 = Vec3f

  type ReadVec4 = ReadVec4f
  type ConstVec4 = ConstVec4f
  val ConstVec4 = ConstVec4f
  type Vec4 = Vec4f
  val Vec4 = Vec4f

  type ReadMat2 = ReadMat2f
  type ConstMat2 = ConstMat2f
  val ConstMat2 = ConstMat2f
  type Mat2 = Mat2f
  val Mat2 = Mat2f

  type ReadMat2x3 = ReadMat2x3f
  type ConstMat2x3 = ConstMat2x3f
  val ConstMat2x3 = ConstMat2x3f
  type Mat2x3 = Mat2x3f
  val Mat2x3 = Mat2x3f

  type ReadMat2x4 = ReadMat2x4f
  type ConstMat2x4 = ConstMat2x4f
  val ConstMat2x4 = ConstMat2x4f
  type Mat2x4 = Mat2x4f
  val Mat2x4 = Mat2x4f

  type ReadMat3x2 = ReadMat3x2f
  type ConstMat3x2 = ConstMat3x2f
  val ConstMat3x2 = ConstMat3x2f
  type Mat3x2 = Mat3x2f
  val Mat3x2 = Mat3x2f

  type ReadMat3 = ReadMat3f
  type ConstMat3 = ConstMat3f
  val ConstMat3 = ConstMat3f
  type Mat3 = Mat3f
  val Mat3 = Mat3f

  type ReadMat3x4 = ReadMat3x4f
  type ConstMat3x4 = ConstMat3x4f
  val ConstMat3x4 = ConstMat3x4f
  type Mat3x4 = Mat3x4f
  val Mat3x4 = Mat3x4f

  type ReadMat4x2 = ReadMat4x2f
  type ConstMat4x2 = ConstMat4x2f
  val ConstMat4x2 = ConstMat4x2f
  type Mat4x2 = Mat4x2f
  val Mat4x2 = Mat4x2f

  type ReadMat4x3 = ReadMat4x3f
  type ConstMat4x3 = ConstMat4x3f
  val ConstMat4x3 = ConstMat4x3f
  type Mat4x3 = Mat4x3f
  val Mat4x3 = Mat4x3f

  type ReadMat4 = ReadMat4f
  type ConstMat4 = ConstMat4f
  val ConstMat4 = ConstMat4f
  type Mat4 = Mat4f
  val Mat4 = Mat4f

  type ReadQuat4 = ReadQuat4f
  type ConstQuat4 = ConstQuat4f
  val ConstQuat4 = ConstQuat4f
  type Quat4 = Quat4f
  val Quat4 = Quat4f

  // Matrix aliases
  type ReadMat2x2 = ReadMat2f
  type ConstMat2x2 = ConstMat2f
  val ConstMat2x2 = ConstMat2f
  type Mat2x2 = Mat2f
  val Mat2x2 = Mat2f

  type ReadMat3x3 = ReadMat3f
  type ConstMat3x3 = ConstMat3f
  val ConstMat3x3 = ConstMat3f
  type Mat3x3 = Mat3f
  val Mat3x3 = Mat3f

  type ReadMat4x4 = ReadMat4f
  type ConstMat4x4 = ConstMat4f
  val ConstMat4x4 = ConstMat4f
  type Mat4x4 = Mat4f
  val Mat4x4 = Mat4f

  // In aliases
  type inVec2 = ReadVec2
  type inVec3 = ReadVec3
  type inVec4 = ReadVec4
  
  type inQuat4 = ReadQuat4
  
  type inMat2 = ReadMat2
  type inMat2x3 = ReadMat2x3
  type inMat2x4 = ReadMat2x4
  
  type inMat3x2 = ReadMat3x2
  type inMat3 = ReadMat3
  type inMat3x4 = ReadMat3x4
  
  type inMat4x2 = ReadMat4x2
  type inMat4x3 = ReadMat4x3
  type inMat4 = ReadMat4

  // In matrix aliases
  type inMat2x2 = inMat2
  type inMat3x3 = inMat3
  type inMat4x4 = inMat4
}
