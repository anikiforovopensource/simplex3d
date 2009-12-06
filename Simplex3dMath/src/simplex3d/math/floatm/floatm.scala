/*
 * Simplex3D, FloatMath module
 * Copyright (C) 2009 Simplex3D team
 *
 * This file is part of Simplex3d.
 *
 * Simplex3d is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3d is distributed in the hope that it will be useful,
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
    implicit def fmFloatToExtFloat(s: Float) = new ExtendedFloat(s)
    implicit def fmIntToExtInt(s: Int) = new ExtendedInt(s)
    implicit def vec2iToVec2f(u: Read2Int) :Vec2f = Vec2f(u.x, u.y)
    implicit def vec3iToVec3f(u: Read3Int) :Vec3f = Vec3f(u.x, u.y, u.z)
    implicit def vec4iToVec4f(u: Read4Int) :Vec4f = Vec4f(u.x, u.y, u.z, u.w)

    implicit def mutable2fToConst(u: Vec2f) = const(u)
    implicit def constVec2fToSwizzled(u: ConstVec2f) = new ConstVec2fSwizzled(u)
    implicit def mutable3fToConst(u: Vec3f) = const(u)
    implicit def constVec3fToSwizzled(u: ConstVec3f) = new ConstVec3fSwizzled(u)
    implicit def mutable4fToConst(u: Vec4f) = const(u)
    implicit def constVec4fToSwizzled(u: ConstVec4f) = new ConstVec4fSwizzled(u)
    implicit def mutableQfToConst(q: Quat4f) = const(q)

    implicit def mutable2fToConst(m: Mat2f) = const(m)
    implicit def mutable2x3fToConst(m: Mat2x3f) = const(m)
    implicit def mutable2x4fToConst(m: Mat2x4f) = const(m)
    implicit def mutable3x2fToConst(m: Mat3x2f) = const(m)
    implicit def mutable3fToConst(m: Mat3f) = const(m)
    implicit def mutable3x4fToConst(m: Mat3x4f) = const(m)
    implicit def mutable4x2fToConst(m: Mat4x2f) = const(m)
    implicit def mutable4x3fToConst(m: Mat4x3f) = const(m)
    implicit def mutable4fToConst(m: Mat4f) = const(m)

    // Aliases
    type AnyMat2x2f = AnyMat2f
    type ConstMat2x2f = ConstMat2f
    type Mat2x2f = Mat2f
    val Mat2x2f = Mat2f

    type AnyMat3x3f = AnyMat3f
    type ConstMat3x3f = ConstMat3f
    type Mat3x3f = Mat3f
    val Mat3x3f = Mat3f

    type AnyMat4x4f = AnyMat4f
    type ConstMat4x4f = ConstMat4f
    type Mat4x4f = Mat4f
    val Mat4x4f = Mat4f

    type ConstRotationSubMat2x2f = ConstRotationSubMat2f
    type RotationSubMat2x2f = RotationSubMat2f

    type ConstRotationSubMat3x3f = ConstRotationSubMat3f
    type RotationSubMat3x3f = RotationSubMat3f
}
