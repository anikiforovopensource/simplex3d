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

package simplex3d.math.doublem

import simplex3d.math._


/**
 * Glue code to make ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {
    def *(u: AnyVec2d) = u*value
    def *(u: AnyVec3d) = u*value
    def *(u: AnyVec4d) = u*value

    def *(q: AnyQuat4d) = q*value

    def *(m: AnyMat2d) = m*value
    def *(m: AnyMat2x3d) = m*value
    def *(m: AnyMat2x4d) = m*value
    def *(m: AnyMat3x2d) = m*value
    def *(m: AnyMat3d) = m*value
    def *(m: AnyMat3x4d) = m*value
    def *(m: AnyMat4x2d) = m*value
    def *(m: AnyMat4x3d) = m*value
    def *(m: AnyMat4d) = m*value

    def /(u: AnyVec2d) = u.divByComponent(value)
    def /(u: AnyVec3d) = u.divByComponent(value)
    def /(u: AnyVec4d) = u.divByComponent(value)

    def /(q: AnyQuat4d) = q.divByComponent(value)

    def /(m: AnyMat2d) = m.divByComponent(value, new Mat2d)
    def /(m: AnyMat2x3d) = m.divByComponent(value, new Mat2x3d)
    def /(m: AnyMat2x4d) = m.divByComponent(value, new Mat2x4d)
    def /(m: AnyMat3x2d) = m.divByComponent(value, new Mat3x2d)
    def /(m: AnyMat3d) = m.divByComponent(value, new Mat3d)
    def /(m: AnyMat3x4d) = m.divByComponent(value, new Mat3x4d)
    def /(m: AnyMat4x2d) = m.divByComponent(value, new Mat4x2d)
    def /(m: AnyMat4x3d) = m.divByComponent(value, new Mat4x3d)
    def /(m: AnyMat4d) = m.divByComponent(value, new Mat4d)
}
