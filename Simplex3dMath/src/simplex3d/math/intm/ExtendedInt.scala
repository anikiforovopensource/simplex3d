/*
 * Simplex3d, IntMath module
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

package simplex3d.math.intm

import simplex3d.math._


/**
 * Glue code to make Ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {

    /** Multiplies this scalar by a vector.
     * @param u a vector to multiply by.
     * @return u*scalar.
     */
    def *(u: AnyVec2i) = u*value

    /** Multiplies this scalar by a vector.
     * @param u a vector to multiply by.
     * @return u*scalar.
     */
    def *(u: AnyVec3i) = u*value

    /** Multiplies this scalar by a vector.
     * @param u a vector to multiply by.
     * @return u*scalar.
     */
    def *(u: AnyVec4i) = u*value

    /** Divides this scalar by a vector.
     * @param u a vector to divide by.
     * @return a vector with components s/u.x and s/u.y.
     */
    def /(u: AnyVec2i) = u.divideByComponent(value)

    /** Divides this scalar by a vector.
     * @param u a vector to divide by.
     * @return a vector with components s/u.x, s/u.y, and s/u.z.
     */
    def /(u: AnyVec3i) = u.divideByComponent(value)

    /** Divides this scalar by a vector.
     * @param u a vector to divide by.
     * @return a vector with components s/u.x, s/u.y, s/u.z, and s/u.w.
     */
    def /(u: AnyVec4i) = u.divideByComponent(value)

    /** Add this scalar to each component of a vector.
     * @param u a vector to add to.
     * @return a vector with components s + u.x and s + u.y.
     */
    def +(u: AnyVec2i) = u + value

    /** Add this scalar to each component of a vector.
     * @param u a vector to add to.
     * @return a vector with components s + u.x, s + u.y, and s + u.z.
     */
    def +(u: AnyVec3i) = u + value

    /** Add this scalar to each component of a vector.
     * @param u a vector to add to.
     * @return a vector with components s + u.x, s + u.y, s + u.z, and s + u.w.
     */
    def +(u: AnyVec4i) = u + value

    def -(u: AnyVec2i) = { val t = -u; t += value; t }
    def -(u: AnyVec3i) = { val t = -u; t += value; t }
    def -(u: AnyVec4i) = { val t = -u; t += value; t }

    def %(u: AnyVec2i) = u.modByComponent(value)
    def %(u: AnyVec3i) = u.modByComponent(value)
    def %(u: AnyVec4i) = u.modByComponent(value)

    def &(u: AnyVec2i) = u & value
    def &(u: AnyVec3i) = u & value
    def &(u: AnyVec4i) = u & value

    def |(u: AnyVec2i) = u | value
    def |(u: AnyVec3i) = u | value
    def |(u: AnyVec4i) = u | value

    def ^(u: AnyVec2i) = u ^ value
    def ^(u: AnyVec3i) = u ^ value
    def ^(u: AnyVec4i) = u ^ value
}
