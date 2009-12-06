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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object renamed {
    
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
    

    type AnyVec2 = AnyVec2f
    type ConstVec2 = ConstVec2f
    type Vec2 = Vec2f
    val Vec2 = Vec2f

    type AnyVec3 = AnyVec3f
    type ConstVec3 = ConstVec3f
    type Vec3 = Vec3f
    val Vec3 = Vec3f

    type AnyVec4 = AnyVec4f
    type ConstVec4 = ConstVec4f
    type Vec4 = Vec4f
    val Vec4 = Vec4f

    type AnyMat2 = AnyMat2f
    type ConstMat2 = ConstMat2f
    type Mat2 = Mat2f
    val Mat2 = Mat2f

    type AnyMat2x3 = AnyMat2x3f
    type ConstMat2x3 = ConstMat2x3f
    type Mat2x3 = Mat2x3f
    val Mat2x3 = Mat2x3f

    type AnyMat2x4 = AnyMat2x4f
    type ConstMat2x4 = ConstMat2x4f
    type Mat2x4 = Mat2x4f
    val Mat2x4 = Mat2x4f

    type AnyMat3x2 = AnyMat3x2f
    type ConstMat3x2 = ConstMat3x2f
    type Mat3x2 = Mat3x2f
    val Mat3x2 = Mat3x2f

    type AnyMat3 = AnyMat3f
    type ConstMat3 = ConstMat3f
    type Mat3 = Mat3f
    val Mat3 = Mat3f

    type AnyMat3x4 = AnyMat3x4f
    type ConstMat3x4 = ConstMat3x4f
    type Mat3x4 = Mat3x4f
    val Mat3x4 = Mat3x4f

    type AnyMat4x2 = AnyMat4x2f
    type ConstMat4x2 = ConstMat4x2f
    type Mat4x2 = Mat4x2f
    val Mat4x2 = Mat4x2f

    type AnyMat4x3 = AnyMat4x3f
    type ConstMat4x3 = ConstMat4x3f
    type Mat4x3 = Mat4x3f
    val Mat4x3 = Mat4x3f

    type AnyMat4 = AnyMat4f
    type ConstMat4 = ConstMat4f
    type Mat4 = Mat4f
    val Mat4 = Mat4f

    type ConstRotationSubMat2 = ConstRotationSubMat2f
    type RotationSubMat2 = RotationSubMat2f

    type ConstRotationSubMat3 = ConstRotationSubMat3f
    type RotationSubMat3 = RotationSubMat3f

    type AnyQuat4 = AnyQuat4f
    type ConstQuat4 = ConstQuat4f
    type Quat4 = Quat4f
    val Quat4 = Quat4f

    val Transform = TransformF
    val InverseTransform = InverseTransformF
    val Translation = TranslationF
    val Rotation = RotationF
    val Scale = ScaleF

    // Aliases
    type AnyMat2x2 = AnyMat2f
    type ConstMat2x2 = ConstMat2f
    type Mat2x2 = Mat2f
    val Mat2x2 = Mat2f

    type AnyMat3x3 = AnyMat3f
    type ConstMat3x3 = ConstMat3f
    type Mat3x3 = Mat3f
    val Mat3x3 = Mat3f

    type AnyMat4x4 = AnyMat4f
    type ConstMat4x4 = ConstMat4f
    type Mat4x4 = Mat4f
    val Mat4x4 = Mat4f

    type ConstRotationSubMat2x2 = ConstRotationSubMat2f
    type RotationSubMat2x2 = RotationSubMat2f

    type ConstRotationSubMat3x3 = ConstRotationSubMat3f
    type RotationSubMat3x3 = RotationSubMat3f
}
