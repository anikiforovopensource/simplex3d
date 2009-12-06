/*
 * Simplex3D, DoubleMath module
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

import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

    // Implicits
    implicit def dmDoubleToExtDouble(s: Double) = new ExtendedDouble(s)
    implicit def dmFloatToExtFloat(s: Int) = new ExtendedFloat(s)
    implicit def dmIntToExtInt(s: Int) = new ExtendedInt(s)
    implicit def vec2iToVec2d(u: Read2Int) :Vec2d = Vec2d(u.x, u.y)
    implicit def vec3iToVec3d(u: Read3Int) :Vec3d = Vec3d(u.x, u.y, u.z)
    implicit def vec4iToVec4d(u: Read4Int) :Vec4d = Vec4d(u.x, u.y, u.z, u.w)
    implicit def vec2fToVec2d(u: Read2Float) :Vec2d = Vec2d(u.x, u.y)
    implicit def vec3fToVec3d(u: Read3Float) :Vec3d = Vec3d(u.x, u.y, u.z)
    implicit def vec4fToVec4d(u: Read4Float) :Vec4d = Vec4d(u.x, u.y, u.z, u.w)

    implicit def mutable2dToConst(u: Vec2d) = const(u)
    implicit def constVec2dToSwizzled(u: ConstVec2d) = new ConstVec2dSwizzled(u)
    implicit def mutable3dToConst(u: Vec3d) = const(u)
    implicit def constVec3dToSwizzled(u: ConstVec3d) = new ConstVec3dSwizzled(u)
    implicit def mutable4dToConst(u: Vec4d) = const(u)
    implicit def constVec4dToSwizzled(u: ConstVec4d) = new ConstVec4dSwizzled(u)
    implicit def mutableQdToConst(q: Quat4d) = const(q)

    implicit def mutable2dToConst(m: Mat2d) = const(m)
    implicit def mutable2x3dToConst(m: Mat2x3d) = const(m)
    implicit def mutable2x4dToConst(m: Mat2x4d) = const(m)
    implicit def mutable3x2dToConst(m: Mat3x2d) = const(m)
    implicit def mutable3dToConst(m: Mat3d) = const(m)
    implicit def mutable3x4dToConst(m: Mat3x4d) = const(m)
    implicit def mutable4x2dToConst(m: Mat4x2d) = const(m)
    implicit def mutable4x3dToConst(m: Mat4x3d) = const(m)
    implicit def mutable4dToConst(m: Mat4d) = const(m)


    // Aliases
    type AnyMat2x2d = AnyMat2d
    type ConstMat2x2d = ConstMat2d
    type Mat2x2d = Mat2d
    val Mat2x2d = Mat2d

    type AnyMat3x3d = AnyMat3d
    type ConstMat3x3d = ConstMat3d
    type Mat3x3d = Mat3d
    val Mat3x3d = Mat3d

    type AnyMat4x4d = AnyMat4d
    type ConstMat4x4d = ConstMat4d
    type Mat4x4d = Mat4d
    val Mat4x4d = Mat4d

    type ConstRotationSubMat2x2d = ConstRotationSubMat2d
    type RotationSubMat2x2d = RotationSubMat2d

    type ConstRotationSubMat3x3d = ConstRotationSubMat3d
    type RotationSubMat3x3d = RotationSubMat3d
}
