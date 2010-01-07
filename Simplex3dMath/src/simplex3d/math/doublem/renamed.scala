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

package simplex3d.math.doublem

import simplex3d.math._
import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object renamed {

    // Implicits
    implicit def dmDoubleToExtDouble(s: Double) = new ExtendedDouble(s)
    implicit def dmFloatToExtFloat(s: Float) = new ExtendedFloat(s)
    implicit def dmIntToExtInt(s: Int) = new ExtendedInt(s)
    implicit def vec2iToVec2d(u: Read2Int) :Vec2d = Vec2d(u.x, u.y)
    implicit def vec3iToVec3d(u: Read3Int) :Vec3d = Vec3d(u.x, u.y, u.z)
    implicit def vec4iToVec4d(u: Read4Int) :Vec4d = Vec4d(u.x, u.y, u.z, u.w)
    implicit def vec2fToVec2d(u: Read2Float) :Vec2d = Vec2d(u.x, u.y)
    implicit def vec3fToVec3d(u: Read3Float) :Vec3d = Vec3d(u.x, u.y, u.z)
    implicit def vec4fToVec4d(u: Read4Float) :Vec4d = Vec4d(u.x, u.y, u.z, u.w)
    

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

    val Transform = Transformd
    val InverseTransform = InverseTransformd
    val Translation = Translationd
    val Rotation = Rotationd
    val Scale = Scaled

    // Aliases
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
}
