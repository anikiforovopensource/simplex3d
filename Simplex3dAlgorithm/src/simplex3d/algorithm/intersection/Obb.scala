/*
 * Simplex3dAlgorithm - Intersection Module
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

package simplex3d.algorithm.intersection

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/** Oriented bounding box. 2d case is orineted bounding rectangle (obr).
 * 
 * @author Aleksey Nikiforov (lex)
 */ 
class Obb(val min: Vec3, val max: Vec3, val transformation: Mat3x4)


object Obb {
  /** Intersect dynamic Obb (dmin, dmax, dtransformation) with a static Aabb (smin, smax).
   * The result is a vector by which the dynamic box has to be displaced to resolve the collision.
   */
  def intersectAabb(dmin: inVec3, dmax: inVec3, dtransformation: inMat3x4)(smin: inVec3, smax: inVec3) :Vec3 = {
    intersectObb(dmin, dmax, dtransformation)(smin, smax, Mat3x4.Identity)
  }
  
  /** Intersect dynamic Obb (dmin, dmax, dtransformation) with a static Obb (smin, smax, stransformation).
   * The result is a vector by which the dynamic box has to be displaced to resolve the collision.
   */
  def intersectObb
    (dmin: inVec3, dmax: inVec3, dtransformation: inMat3x4)
    (smin: inVec3, smax: inVec3, stransformation: inMat3x4)
  :Vec3 = {
    Vec3(0)
  }
}
