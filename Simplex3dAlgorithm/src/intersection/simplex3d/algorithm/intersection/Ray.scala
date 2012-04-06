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

package simplex3d.algorithm.intersection

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


/**
 * Ray is defined as a directed line segment starting at the origin and ending
 * at (origin + direction).
 *
 * @param origin ray origin.
 * @param direction non-normalized direction.
 *
 * @author Aleksey Nikiforov (lex)
 */
class Ray private (val origin: inVec3, val direction: inVec3) {
  def point(s: Double) :Vec3 = origin + direction*s

  def intersectsPlane(normal: inVec3, coefficient: Double) :Boolean = {
    throw new UnsupportedOperationException()
  }

  def intersectsSphere(translation: inVec3, radius: Double) :Boolean = {
    throw new UnsupportedOperationException()
  }

  def intersectsAabb(min: inVec3, max: inVec3) :Boolean = {
    (Ray.intersectAabb(origin, direction)(min, max).length != 0)
  }

  def intersectsObb(min: inVec3, max: inVec3, transformation: inMat4x3) :Boolean = {
    (Ray.intersectObb(origin, direction)(min, max, transformation).length != 0)
  }

  def intersectPlane(normal: inVec3, coefficient: Double) :Array[Double] = {
    throw new UnsupportedOperationException()
  }

  def intersectSphere(translation: inVec3, radius: Double) :Array[Double] = {
    throw new UnsupportedOperationException()
  }

  def intersectAabb(min: inVec3, max: inVec3) :Array[Double] = {
    Ray.intersectAabb(origin, direction)(min, max)
  }

  def intersectObb(min: inVec3, max: inVec3, transformation: inMat4x3) :Array[Double] = {
    Ray.intersectObb(origin, direction)(min, max, transformation)
  }
}


object Ray {
  
  private val noIntersection = new Array[Double](0)
  
  
  def apply(origin: inVec3, direction: inVec3) = new Ray(origin, direction)

  
  def intersectAabb(origin: inVec3, direction: inVec3)(min: inVec3, max: inVec3) :Array[Double] = {

    // (origin + direction*ratio(0)) is an intersecion point closest to the origin,
    // (origin + direction*ratio(1)) is the second intersection point.
    val ratio = Array[Double](0, 1)

    def testComponent(origin: Double, direction: Double, min: Double, max: Double) :Boolean = {
      if (abs(direction) < 1e-14) return (min <= origin && origin <= max)

      val invDirection = 1/direction
      var rmin = (min - origin)*invDirection
      var rmax = (max - origin)*invDirection
      if (rmin > rmax) { val tmp = rmin; rmin = rmax; rmax = tmp }

      if (rmin > ratio(0)) ratio(0) = rmin
      if (rmax < ratio(1)) ratio(1) = rmax

      (ratio(0) <= ratio(1))
    }

    if (!testComponent(origin.x, direction.x, min.x, max.x)) return noIntersection
    if (!testComponent(origin.y, direction.y, min.y, max.y)) return noIntersection
    if (!testComponent(origin.z, direction.z, min.z, max.z)) return noIntersection

    ratio
  }
  
  def intersectObb(origin: inVec3, direction: inVec3)(min: inVec3, max: inVec3, transformation: inMat4x3)
  :Array[Double] = {
    val t = inverse(transformation)
    intersectAabb(t.transformPoint(origin), t.transformVector(direction))(min, max)
  }
}
