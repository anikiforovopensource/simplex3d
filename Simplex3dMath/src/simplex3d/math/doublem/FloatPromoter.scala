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
 * Glue code to promote float types to double types.
 *
 * @author Aleksey Nikiforov (lex)
 */
final class FloatPromoter(val value: Double) {
    // Vecf to Vecd promotion
    def *(u: Read2{ type T = Float }) =
        new Vec2d(u.dx*value, u.dy*value)

    def *(u: Read3{ type T = Float }) =
        new Vec3d(u.dx*value, u.dy*value, u.dz*value)

    def *(u: Read4{ type T = Float }) =
        new Vec4d(u.dx*value, u.dy*value, u.dz*value, u.dw*value)

    def /(u: Read2{ type T = Float }) =
        new Vec2d(value/u.dx, value/u.dy)

    def /(u: Read3{ type T = Float }) =
        new Vec3d(value/u.dx, value/u.dy, value/u.dz)

    def /(u: Read4{ type T = Float }) =
        new Vec4d(value/u.dx, value/u.dy, value/u.dz, value/u.dw)


    def +(u: Read2{ type T = Float }) =
        new Vec2d(value + u.dx, value + u.dy)

    def +(u: Read3{ type T = Float }) =
        new Vec3d(value + u.dx, value + u.dy, value + u.dz)

    def +(u: Read4{ type T = Float }) =
        new Vec4d(value + u.dx, value + u.dy, value + u.dz, value + u.dw)

    def -(u: Read2{ type T = Float }) =
        new Vec2d(value - u.dx, value - u.dy)

    def -(u: Read3{ type T = Float }) =
        new Vec3d(value - u.dx, value - u.dy, value - u.dz)

    def -(u: Read4{ type T = Float }) =
        new Vec4d(value - u.dx, value - u.dy, value - u.dz, value - u.dw)
}
