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

package simplex3d.math


import simplex3d.math.floatm.FloatMath._

/**
 * @author Aleksey Nikiforov (lex)
 */
package object floatm {

    // Implicits
    implicit def fmIntPromoter(s: Float) = new IntPromoter(s)

    implicit def fmFloat(s: Float) = new ExtendedFloat(s)
    implicit def fmInt(s: Int) = new ExtendedInt(s)

    implicit def fmcastVec2i(u: Read2[Int]) :ConstVec2f =
        new ConstVec2f(u.fx, u.fy)

    implicit def fmcastVec3i(u: Read3[Int]) :ConstVec3f =
        new ConstVec3f(u.fx, u.fy, u.fz)

    implicit def fmcastVec4i(u: Read4[Int]) :ConstVec4f =
        new ConstVec4f(u.fx, u.fy, u.fz, u.fw)


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
}
