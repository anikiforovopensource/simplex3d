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

package simplex3d

import simplex3d.math.BaseMath._


/**
 * @author Aleksey Nikiforov (lex)
 */
package object math {

    // Implicits
    implicit def intToRead1(x: Int) = new IntVal(x)
    implicit def floatToRead1(x: Float) = new FloatVal(x)
    implicit def doubleToRead1(x: Double) = new DoubleVal(x)

    implicit def mutable2bToConst(u: Vec2b) = constb(u)
    implicit def constVec2bToSwizzled(u: ConstVec2b) = new ConstVec2bSwizzled(u)
    implicit def mutable3bToConst(u: Vec3b) = constb(u)
    implicit def constVec3bToSwizzled(u: ConstVec3b) = new ConstVec3bSwizzled(u)
    implicit def mutable4bToConst(u: Vec4b) = constb(u)
    implicit def constVec4bToSwizzled(u: ConstVec4b) = new ConstVec4bSwizzled(u)
}
