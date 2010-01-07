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

    def /(u: AnyVec2d) = u.divideByComponent(value)
    def /(u: AnyVec3d) = u.divideByComponent(value)
    def /(u: AnyVec4d) = u.divideByComponent(value)

    def /(q: AnyQuat4d) = q.divideByComponent(value)

    def /(m: AnyMat2d) = m.divideByComponent(value)
    def /(m: AnyMat2x3d) = m.divideByComponent(value)
    def /(m: AnyMat2x4d) = m.divideByComponent(value)
    def /(m: AnyMat3x2d) = m.divideByComponent(value)
    def /(m: AnyMat3d) = m.divideByComponent(value)
    def /(m: AnyMat3x4d) = m.divideByComponent(value)
    def /(m: AnyMat4x2d) = m.divideByComponent(value)
    def /(m: AnyMat4x3d) = m.divideByComponent(value)
    def /(m: AnyMat4d) = m.divideByComponent(value)
}
