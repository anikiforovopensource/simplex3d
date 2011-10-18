/*
 * Simplex3dAlgorithm - Intersection Module
 * Copyright (C) 2010-2011, Aleksey Nikiforov
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
import Collision._


/** 
 * 
 * @author Aleksey Nikiforov (lex)
 */ 
class Plane(val normal: ConstVec3, val coefficient: Double) {

  def intersectAabb(min: inVec3, max: inVec3) :Int = {
    val pSelector = greaterThanEqual(normal, Vec3.Zero)
    val pVertex = mix(min, max, pSelector)

    if (dot(pVertex, normal) < -coefficient) Outside
    else {
      val nVertex = mix(min, max, not(pSelector))

      if (dot(nVertex, normal) < -coefficient) Intersecting
      else Inside
    }
  }

  //def intersectObb
}

object Plane {
  /** For a vector u, dot(equation.xyz, u) + equation.w = 0.
   */
  def apply(planeEquation: inVec4) = {
    val dir = planeEquation.xyz
    val len = length(dir);
    new Plane(dir/len, planeEquation.w/len)
  }
}
