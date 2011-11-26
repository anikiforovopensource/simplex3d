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


/** Axis aligned bounding box. 2d case Axis alingned bounding rectangle (Aabr).
 * 
 * @author Aleksey Nikiforov (lex)
 */ 
class Aabb(val min: Vec3, val max: Vec3)


object Aabb {
  
  /** Projects Aabb defined by bmin and bmax onto a space defined by transformation.
   */
  def projectAabb(bmin: inVec3, bmax: inVec3, transformation: inMat3x4)(resultMin: Vec3, resultMax :Vec3) {
  
    resultMin := Vec3(Double.PositiveInfinity)
    resultMax := Vec3(Double.NegativeInfinity)
        
    def process(point: inVec3) {
      val trainsformed = transformation.transformPoint(point)
      resultMin := min(resultMin, trainsformed)
      resultMax := max(resultMax, trainsformed)
    }
  
    process(bmin)
    process(Vec3(bmax.x, bmin.y, bmin.z))
    process(Vec3(bmin.x, bmin.y, bmax.z))
    process(Vec3(bmax.x, bmin.y, bmax.z))
    process(bmax)
    process(Vec3(bmin.x, bmax.y, bmax.z))
    process(Vec3(bmax.x, bmax.y, bmin.z))
    process(Vec3(bmin.x, bmax.y, bmin.z))
  }
  
  /** Intersect dynamic bounding box (dmin, dmax) with a static bounding box (smin, smax).
   * The result is a vector by which the dynamic box has to be displaced to resolve the collision.
   * 
   */
  def intersectAabb(dmin: inVec3, dmax: inVec3)(smin: inVec3, smax: inVec3) :Vec3 = {
    val tx = Util.axisAlignedComponentTest(dmin.x, dmax.x, smin.x, smax.x); if (tx == 0) return Vec3(0)
    val ty = Util.axisAlignedComponentTest(dmin.y, dmax.y, smin.y, smax.y); if (ty == 0) return Vec3(0)
    val tz = Util.axisAlignedComponentTest(dmin.z, dmax.z, smin.z, smax.z); if (tz == 0) return Vec3(0)
    
    val ax = abs(tx)
    val ay = abs(ty)
    val az = abs(tz)
    
    val res = min(ax, min(ay, az))
    
    if (res == ax) Vec3(tx, 0, 0)
    else if (res == ay) Vec3(0, ty, 0)
    else Vec3(0, 0, tz)
  }
}
