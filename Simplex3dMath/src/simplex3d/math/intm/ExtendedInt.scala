/*
 * Simplex3d, IntMath module
 * Copyright (C) 2009 Simplex3d Team
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
 * Glue code to make ints interact with vectors and matrices.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class ExtendedInt(val value: Int) {
    def *(u: AnyVec2i) = u*value
    def *(u: AnyVec3i) = u*value
    def *(u: AnyVec4i) = u*value

    def /(u: AnyVec2i) = u.divideByComponent(value)
    def /(u: AnyVec3i) = u.divideByComponent(value)
    def /(u: AnyVec4i) = u.divideByComponent(value)

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
