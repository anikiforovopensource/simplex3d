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
 * @author Aleksey Nikiforov (lex)
 */
private[math] class FloatVecFactory
extends VecFactory[Float, ConstVec2f, ConstVec3f, ConstVec4f]
{
    protected def make2(x: Float, y: Float) = ConstVec2f(x, y)
    protected def make3(x: Float,
                               y: Float,
                               z: Float) = ConstVec3f(x, y, z)
    protected def make4(x: Float,
                               y: Float,
                               z: Float,
                               w: Float) = ConstVec4f(x, y, z, w)
}
