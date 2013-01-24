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


/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatx extends ImplicitMathContext {

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

  
  // Matrix aliases
  type ReadMat2x2f = ReadMat2f
  type ConstMat2x2f = ConstMat2f
  val ConstMat2x2f = ConstMat2f
  type Mat2x2f = Mat2f
  val Mat2x2f = Mat2f

  type ReadMat3x3f = ReadMat3f
  type ConstMat3x3f = ConstMat3f
  val ConstMat3x3f = ConstMat3f
  type Mat3x3f = Mat3f
  val Mat3x3f = Mat3f

  type ReadMat4x4f = ReadMat4f
  type ConstMat4x4f = ConstMat4f
  val ConstMat4x4f = ConstMat4f
  type Mat4x4f = Mat4f
  val Mat4x4f = Mat4f

  // In aliases
  type inVec2f = ReadVec2f
  type inVec3f = ReadVec3f
  type inVec4f = ReadVec4f
  
  type inQuat4f = ReadQuat4f
  
  type inMat2f = ReadMat2f
  type inMat2x3f = ReadMat2x3f
  type inMat2x4f = ReadMat2x4f
  
  type inMat3x2f = ReadMat3x2f
  type inMat3f = ReadMat3f
  type inMat3x4f = ReadMat3x4f
  
  type inMat4x2f = ReadMat4x2f
  type inMat4x3f = ReadMat4x3f
  type inMat4f = ReadMat4f

  // In matrix aliases
  type inMat2x2f = inMat2f
  type inMat3x3f = inMat3f
  type inMat4x4f = inMat4f
}
