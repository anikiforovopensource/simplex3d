/*
 * Simplex3dMath - Double Module
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
package object doublex extends ImplicitMathContext {

  // Implicits
  implicit def intToDoubeRef(s: Int) :ReadDoubleRef = new DoubleRef(s)
  implicit def floatToDoubeRef(s: Float) :ReadDoubleRef = new DoubleRef(s)
  implicit def doubleToRef(s: Double) :ReadDoubleRef = new DoubleRef(s)
  implicit def refToDouble(r: ReadDoubleRef) = r.toConst

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

  implicit def mat2x2FloatToDouble(m: AnyMat2[Float]) :ConstMat2d =
    ConstMat2d(m)
  
  implicit def mat2x3FloatToDouble(m: AnyMat2x3[Float]) :ConstMat2x3d =
    ConstMat2x3d(m)
  
  implicit def mat2x4FloatToDouble(m: AnyMat2x4[Float]) :ConstMat2x4d =
    ConstMat2x4d(m)
  
  implicit def mat3x2FloatToDouble(m: AnyMat3x2[Float]) :ConstMat3x2d =
    ConstMat3x2d(m)
  
  implicit def mat3x3FloatToDouble(m: AnyMat3[Float]) :ConstMat3d =
    ConstMat3d(m)
  
  implicit def mat3x4FloatToDouble(m: AnyMat3x4[Float]) :ConstMat3x4d =
    ConstMat3x4d(m)
  
  implicit def mat4x2FloatToDouble(m: AnyMat4x2[Float]) :ConstMat4x2d =
    ConstMat4x2d(m)
  
  implicit def mat4x3FloatToDouble(m: AnyMat4x3[Float]) :ConstMat4x3d =
    ConstMat4x3d(m)
  
  implicit def mat4x4FloatToDouble(m: AnyMat4[Float]) :ConstMat4d =
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

  // In aliases
  type inVec2d = ReadVec2d
  type inVec3d = ReadVec3d
  type inVec4d = ReadVec4d
  
  type inQuat4d = ReadQuat4d
  
  type inMat2d = ReadMat2d
  type inMat2x3d = ReadMat2x3d
  type inMat2x4d = ReadMat2x4d
  
  type inMat3x2d = ReadMat3x2d
  type inMat3d = ReadMat3d
  type inMat3x4d = ReadMat3x4d
  
  type inMat4x2d = ReadMat4x2d
  type inMat4x3d = ReadMat4x3d
  type inMat4d = ReadMat4d

  // In matrix aliases
  type inMat2x2d = inMat2d
  type inMat3x3d = inMat3d
  type inMat4x4d = inMat4d
}
