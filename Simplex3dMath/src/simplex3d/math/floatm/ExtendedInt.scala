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


/**
 * Glue code to make ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {
    def *(u: AnyVec2f) = u*value
    def *(u: AnyVec3f) = u*value
    def *(u: AnyVec4f) = u*value

    def *(q: AnyQuat4f) = q*value

    def *(m: AnyMat2f) = m*value
    def *(m: AnyMat2x3f) = m*value
    def *(m: AnyMat2x4f) = m*value
    def *(m: AnyMat3x2f) = m*value
    def *(m: AnyMat3f) = m*value
    def *(m: AnyMat3x4f) = m*value
    def *(m: AnyMat4x2f) = m*value
    def *(m: AnyMat4x3f) = m*value
    def *(m: AnyMat4f) = m*value

    def /(u: AnyVec2f) = u.divByComponent(value)
    def /(u: AnyVec3f) = u.divByComponent(value)
    def /(u: AnyVec4f) = u.divByComponent(value)

    def /(q: AnyQuat4f) = q.divByComponent(value)

    def /(m: AnyMat2f) = m.divByComponent(value, new Mat2f)
    def /(m: AnyMat2x3f) = m.divByComponent(value, new Mat2x3f)
    def /(m: AnyMat2x4f) = m.divByComponent(value, new Mat2x4f)
    def /(m: AnyMat3x2f) = m.divByComponent(value, new Mat3x2f)
    def /(m: AnyMat3f) = m.divByComponent(value, new Mat3f)
    def /(m: AnyMat3x4f) = m.divByComponent(value, new Mat3x4f)
    def /(m: AnyMat4x2f) = m.divByComponent(value, new Mat4x2f)
    def /(m: AnyMat4x3f) = m.divByComponent(value, new Mat4x3f)
    def /(m: AnyMat4f) = m.divByComponent(value, new Mat4f)
}
