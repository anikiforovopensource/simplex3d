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

package simplex3d.math

import simplex3d.math.doublem.DoubleMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object doublem {

    // Implicits
    implicit def dextendDouble(s: Double) = new ExtendedDouble(s)
    implicit def dextendFloat(s: Float) = new ExtendedFloat(s)
    implicit def dextendInt(s: Int) = new ExtendedInt(s)

    implicit def dpromoteVec2i(u: Read2{ type T = Int }) :Vec2d =
        Vec2d(u.dx, u.dy)

    implicit def dpromoteVec3i(u: Read3{ type T = Int }) :Vec3d =
        Vec3d(u.dx, u.dy, u.dz)

    implicit def dpromoteVec4i(u: Read4{ type T = Int }) :Vec4d =
        Vec4d(u.dx, u.dy, u.dz, u.dw)

    implicit def dpromoteVec2f(u: Read2{ type T = Float }) :Vec2d =
        Vec2d(u.dx, u.dy)

    implicit def dpromoteVec3f(u: Read3{ type T = Float }) :Vec3d =
        Vec3d(u.dx, u.dy, u.dz)

    implicit def dpromoteVec4f(u: Read4{ type T = Float }) :Vec4d =
        Vec4d(u.dx, u.dy, u.dz, u.dw)


    // Aliases
    type AnyMat2x2d = AnyMat2d
    type ConstMat2x2d = ConstMat2d
    val ConstMat2x2d = ConstMat2d
    type Mat2x2d = Mat2d
    val Mat2x2d = Mat2d

    type AnyMat3x3d = AnyMat3d
    type ConstMat3x3d = ConstMat3d
    val ConstMat3x3d = ConstMat3d
    type Mat3x3d = Mat3d
    val Mat3x3d = Mat3d

    type AnyMat4x4d = AnyMat4d
    type ConstMat4x4d = ConstMat4d
    val ConstMat4x4d = ConstMat4d
    type Mat4x4d = Mat4d
    val Mat4x4d = Mat4d
}
