/*
 * Simplex3d, FloatMath module
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

package simplex3d.math.floatm

import simplex3d.math._
import simplex3d.math.floatm.FloatMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object renamed {
    
    // Implicits
    implicit def fintPromotion(s: Float) = new IntPromotion(s)
    
    implicit def fextendFloat(s: Float) = new ExtendedFloat(s)
    implicit def fextendInt(s: Int) = new ExtendedInt(s)

    implicit def fpromoteVec2i(u: Read2{ type T = Int }) :Vec2f =
        Vec2f(u.fx, u.fy)

    implicit def fpromoteVec3i(u: Read3{ type T = Int }) :Vec3f =
        Vec3f(u.fx, u.fy, u.fz)

    implicit def fpromoteVec4i(u: Read4{ type T = Int }) :Vec4f =
        Vec4f(u.fx, u.fy, u.fz, u.fw)


    type AnyVec2 = AnyVec2f
    type ConstVec2 = ConstVec2f
    val ConstVec2 = ConstVec2f
    type Vec2 = Vec2f
    val Vec2 = Vec2f

    type AnyVec3 = AnyVec3f
    type ConstVec3 = ConstVec3f
    val ConstVec3 = ConstVec3f
    type Vec3 = Vec3f
    val Vec3 = Vec3f

    type AnyVec4 = AnyVec4f
    type ConstVec4 = ConstVec4f
    val ConstVec4 = ConstVec4f
    type Vec4 = Vec4f
    val Vec4 = Vec4f

    type AnyMat2 = AnyMat2f
    type ConstMat2 = ConstMat2f
    val ConstMat2 = ConstMat2f
    type Mat2 = Mat2f
    val Mat2 = Mat2f

    type AnyMat2x3 = AnyMat2x3f
    type ConstMat2x3 = ConstMat2x3f
    val ConstMat2x3 = ConstMat2x3f
    type Mat2x3 = Mat2x3f
    val Mat2x3 = Mat2x3f

    type AnyMat2x4 = AnyMat2x4f
    type ConstMat2x4 = ConstMat2x4f
    val ConstMat2x4 = ConstMat2x4f
    type Mat2x4 = Mat2x4f
    val Mat2x4 = Mat2x4f

    type AnyMat3x2 = AnyMat3x2f
    type ConstMat3x2 = ConstMat3x2f
    val ConstMat3x2 = ConstMat3x2f
    type Mat3x2 = Mat3x2f
    val Mat3x2 = Mat3x2f

    type AnyMat3 = AnyMat3f
    type ConstMat3 = ConstMat3f
    val ConstMat3 = ConstMat3f
    type Mat3 = Mat3f
    val Mat3 = Mat3f

    type AnyMat3x4 = AnyMat3x4f
    type ConstMat3x4 = ConstMat3x4f
    val ConstMat3x4 = ConstMat3x4f
    type Mat3x4 = Mat3x4f
    val Mat3x4 = Mat3x4f

    type AnyMat4x2 = AnyMat4x2f
    type ConstMat4x2 = ConstMat4x2f
    val ConstMat4x2 = ConstMat4x2f
    type Mat4x2 = Mat4x2f
    val Mat4x2 = Mat4x2f

    type AnyMat4x3 = AnyMat4x3f
    type ConstMat4x3 = ConstMat4x3f
    val ConstMat4x3 = ConstMat4x3f
    type Mat4x3 = Mat4x3f
    val Mat4x3 = Mat4x3f

    type AnyMat4 = AnyMat4f
    type ConstMat4 = ConstMat4f
    val ConstMat4 = ConstMat4f
    type Mat4 = Mat4f
    val Mat4 = Mat4f

    type AnyQuat4 = AnyQuat4f
    type ConstQuat4 = ConstQuat4f
    val ConstQuat4 = ConstQuat4f
    type Quat4 = Quat4f
    val Quat4 = Quat4f

    type AnyTransform2 = AnyTransform2f
    type ConstTransform2 = ConstTransform2f
    val ConstTransform2 = ConstTransform2f
    type Transform2 = Transform2f
    val Transform2 = Transform2f

    type Transform3 = Transform3f
    val Transform3 = Transform3f

    // Aliases
    type AnyMat2x2 = AnyMat2f
    type ConstMat2x2 = ConstMat2f
    val ConstMat2x2 = ConstMat2f
    type Mat2x2 = Mat2f
    val Mat2x2 = Mat2f

    type AnyMat3x3 = AnyMat3f
    type ConstMat3x3 = ConstMat3f
    val ConstMat3x3 = ConstMat3f
    type Mat3x3 = Mat3f
    val Mat3x3 = Mat3f

    type AnyMat4x4 = AnyMat4f
    type ConstMat4x4 = ConstMat4f
    val ConstMat4x4 = ConstMat4f
    type Mat4x4 = Mat4f
    val Mat4x4 = Mat4f
}
