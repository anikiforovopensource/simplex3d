/*
 * Simplex3D, Math module
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


/**
 * Glue code to make floats interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedFloat(val value: Float) extends Read1[Float] {
    def *(u: AnyVec2) = u*value
    def *(u: AnyVec3) = u*value
    def *(u: AnyVec4) = u*value

    def *(q: AnyQuat4) = q*value

    def *(m: AnyMat2) = m*value
    def *(m: AnyMat2x3) = m*value
    def *(m: AnyMat2x4) = m*value
    def *(m: AnyMat3x2) = m*value
    def *(m: AnyMat3) = m*value
    def *(m: AnyMat3x4) = m*value
    def *(m: AnyMat4x2) = m*value
    def *(m: AnyMat4x3) = m*value
    def *(m: AnyMat4) = m*value

    def /(u: AnyVec2) = u.divideByComponent(value)
    def /(u: AnyVec3) = u.divideByComponent(value)
    def /(u: AnyVec4) = u.divideByComponent(value)

    def /(q: AnyQuat4) = q.divideByComponent(value)

    def /(m: AnyMat2) = m.divideByComponent(value)
    def /(m: AnyMat2x3) = m.divideByComponent(value)
    def /(m: AnyMat2x4) = m.divideByComponent(value)
    def /(m: AnyMat3x2) = m.divideByComponent(value)
    def /(m: AnyMat3) = m.divideByComponent(value)
    def /(m: AnyMat3x4) = m.divideByComponent(value)
    def /(m: AnyMat4x2) = m.divideByComponent(value)
    def /(m: AnyMat4x3) = m.divideByComponent(value)
    def /(m: AnyMat4) = m.divideByComponent(value)
}
