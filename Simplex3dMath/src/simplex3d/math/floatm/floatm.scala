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


    // Aliases
    type AnyMat2x2f = AnyMat2f
    type ConstMat2x2f = ConstMat2f
    val ConstMat2x2f = ConstMat2f
    type Mat2x2f = Mat2f
    val Mat2x2f = Mat2f

    type AnyMat3x3f = AnyMat3f
    type ConstMat3x3f = ConstMat3f
    val ConstMat3x3f = ConstMat3f
    type Mat3x3f = Mat3f
    val Mat3x3f = Mat3f

    type AnyMat4x4f = AnyMat4f
    type ConstMat4x4f = ConstMat4f
    val ConstMat4x4f = ConstMat4f
    type Mat4x4f = Mat4f
    val Mat4x4f = Mat4f

    type ConstRotationSubMat2x2f = ConstRotationSubMat2f
    type RotationSubMat2x2f = RotationSubMat2f

    type ConstRotationSubMat3x3f = ConstRotationSubMat3f
    type RotationSubMat3x3f = RotationSubMat3f
}
