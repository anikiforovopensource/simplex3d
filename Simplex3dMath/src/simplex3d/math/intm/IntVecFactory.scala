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

package simplex3d.math.intm

import simplex3d.math._


/**
 * @author Aleksey Nikiforov (lex)
 */
private[math] class IntVecFactory
extends VecFactory[Int, ConstVec2i, ConstVec3i, ConstVec4i]
{
    protected def make2(x: Int, y: Int) = new ConstVec2i(x, y)
    protected def make3(x: Int,
                               y: Int,
                               z: Int) = new ConstVec3i(x, y, z)
    protected def make4(x: Int,
                               y: Int,
                               z: Int,
                               w: Int) = new ConstVec4i(x, y, z, w)
}
