/*
 * Simplex3d, MathTest package
 * Copyright (C) 2009-2010 Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math

import simplex3d.math.floatm.renamed._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class Fun {
    /**
     * @param pixel
     *          pixel coordinates
     * @param time
     *          time, in seconds
     * @param dimensions
     *          width and height of the rendering surface
     * @return
     *          rgb color with floating point components from 0 to 1
     */
    def apply(pixel: AnyVec2, time: Float, dimensions: AnyVec2) :AnyVec3
}
