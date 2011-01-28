/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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

package visual.math.draw

import simplex3d.math.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
abstract class Function {

  /**
   * Width and height of the rendering surface
   */
  final val dimensions: ReadVec2 = Vec2(0)

  /**
   * @param pixel
   *          pixel coordinates
   * @param time
   *          time, in seconds
   * @return
   *          rgb color with floating point components from 0 to 1
   */
  def apply(pixel: ReadVec2, time: Double) :ReadVec3
}
