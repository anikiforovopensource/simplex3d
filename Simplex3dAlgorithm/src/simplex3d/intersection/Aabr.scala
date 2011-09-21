/*
 * Simplex3d, Intersection module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.intersection

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/** Axis aligned bounding box. 2d case Axis alingned bounding rectangle (Aabr).
 * 
 * @author Aleksey Nikiforov (lex)
 */ 
class Aabr(val min: Vec2, val max: Vec2)


object Aabr {
  
  /** Intersect dynamic bounding rectangle (dmin, dmax) with a static bounding rectangle (smin, smax).
   * The result is a vector by which the dynamic rectangle has to be displaced to resolve the collision.
   * 
   */
  def intersectAabr(dmin: inVec2, dmax: inVec2)(smin: inVec2, smax: inVec2) :Vec2 = {
    val tx = Util.axisAlignedComponentTest(dmin.x, dmax.x, smin.x, smax.x); if (tx == 0) return Vec2(0)
    val ty = Util.axisAlignedComponentTest(dmin.y, dmax.y, smin.y, smax.y); if (ty == 0) return Vec2(0)
    
    if (abs(tx) < abs(ty)) Vec2(tx, 0) else Vec2(0, ty)
  }
}
