/*
 * Simplex3D, IntMath module
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

package simplex3d.math

import simplex3d.math.intm.IntMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object intm {

    //Implicints
    implicit def imIntToExtInt(s: Int) = new ExtendedInt(s)

    implicit def mutable2iToConst(u: Vec2i) = consti(u)
    implicit def constVec2iToSwizzled(u: ConstVec2i) = new ConstVec2iSwizzled(u)
    implicit def mutable3iToConst(u: Vec3i) = consti(u)
    implicit def constVec3iToSwizzled(u: ConstVec3i) = new ConstVec3iSwizzled(u)
    implicit def mutable4iToConst(u: Vec4i) = consti(u)
    implicit def constVec4iToSwizzled(u: ConstVec4i) = new ConstVec4iSwizzled(u)
}
