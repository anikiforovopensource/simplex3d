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

import simplex3d.math.types._
import simplex3d.math.doublex._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object double {

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


  type ReadDoubleRef = doublex.ReadDoubleRef
  type DoubleRef = doublex.DoubleRef
  val DoubleRef = doublex.DoubleRef
  val functions = doublex.functions

  type ReadVec2 = ReadVec2d
  type ConstVec2 = ConstVec2d
  val ConstVec2 = ConstVec2d
  type Vec2 = Vec2d
  val Vec2 = Vec2d

  type ReadVec3 = ReadVec3d
  type ConstVec3 = ConstVec3d
  val ConstVec3 = ConstVec3d
  type Vec3 = Vec3d
  val Vec3 = Vec3d

  type ReadVec4 = ReadVec4d
  type ConstVec4 = ConstVec4d
  val ConstVec4 = ConstVec4d
  type Vec4 = Vec4d
  val Vec4 = Vec4d

  type ReadMat2 = ReadMat2d
  type ConstMat2 = ConstMat2d
  val ConstMat2 = ConstMat2d
  type Mat2 = Mat2d
  val Mat2 = Mat2d

  type ReadMat2x3 = ReadMat2x3d
  type ConstMat2x3 = ConstMat2x3d
  val ConstMat2x3 = ConstMat2x3d
  type Mat2x3 = Mat2x3d
  val Mat2x3 = Mat2x3d

  type ReadMat2x4 = ReadMat2x4d
  type ConstMat2x4 = ConstMat2x4d
  val ConstMat2x4 = ConstMat2x4d
  type Mat2x4 = Mat2x4d
  val Mat2x4 = Mat2x4d

  type ReadMat3x2 = ReadMat3x2d
  type ConstMat3x2 = ConstMat3x2d
  val ConstMat3x2 = ConstMat3x2d
  type Mat3x2 = Mat3x2d
  val Mat3x2 = Mat3x2d

  type ReadMat3 = ReadMat3d
  type ConstMat3 = ConstMat3d
  val ConstMat3 = ConstMat3d
  type Mat3 = Mat3d
  val Mat3 = Mat3d

  type ReadMat3x4 = ReadMat3x4d
  type ConstMat3x4 = ConstMat3x4d
  val ConstMat3x4 = ConstMat3x4d
  type Mat3x4 = Mat3x4d
  val Mat3x4 = Mat3x4d

  type ReadMat4x2 = ReadMat4x2d
  type ConstMat4x2 = ConstMat4x2d
  val ConstMat4x2 = ConstMat4x2d
  type Mat4x2 = Mat4x2d
  val Mat4x2 = Mat4x2d

  type ReadMat4x3 = ReadMat4x3d
  type ConstMat4x3 = ConstMat4x3d
  val ConstMat4x3 = ConstMat4x3d
  type Mat4x3 = Mat4x3d
  val Mat4x3 = Mat4x3d

  type ReadMat4 = ReadMat4d
  type ConstMat4 = ConstMat4d
  val ConstMat4 = ConstMat4d
  type Mat4 = Mat4d
  val Mat4 = Mat4d

  type ReadQuat4 = ReadQuat4d
  type ConstQuat4 = ConstQuat4d
  val ConstQuat4 = ConstQuat4d
  type Quat4 = Quat4d
  val Quat4 = Quat4d

  // Matrix aliases
  type ReadMat2x2 = ReadMat2d
  type ConstMat2x2 = ConstMat2d
  val ConstMat2x2 = ConstMat2d
  type Mat2x2 = Mat2d
  val Mat2x2 = Mat2d

  type ReadMat3x3 = ReadMat3d
  type ConstMat3x3 = ConstMat3d
  val ConstMat3x3 = ConstMat3d
  type Mat3x3 = Mat3d
  val Mat3x3 = Mat3d

  type ReadMat4x4 = ReadMat4d
  type ConstMat4x4 = ConstMat4d
  val ConstMat4x4 = ConstMat4d
  type Mat4x4 = Mat4d
  val Mat4x4 = Mat4d

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
