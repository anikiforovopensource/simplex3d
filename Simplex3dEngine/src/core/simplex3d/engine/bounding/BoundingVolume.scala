/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package bounding

import simplex3d.math.types._
import simplex3d.math.double._
import simplex3d.algorithm.intersection._
import simplex3d.engine.util._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._



trait BoundingVolume extends Accessible {
  type Read = BoundingVolume
  type Mutable = BoundingVolume
}


object BoundingVolume {
  // TODO improve culling performance by keeping track of "safe" planes.
  final def intersect[T <: Transformation](
    frustum: Frustum, volume: BoundingVolume, worldTransformation: TransformationBinding[T]
  ) :Int =
  {
    volume match {
      case bound: Aabb =>
        frustum.intersectAabb(bound.min, bound.max)
      case bound: Oabb =>
        if (worldTransformation.isDefined) frustum.intersectObb(
          bound.min, bound.max,
          worldTransformation.matrix
        )
        else frustum.intersectAabb(
          bound.min, bound.max
        )
      case bound: Obb =>
        if (worldTransformation.isDefined) frustum.intersectObb(
          bound.min, bound.max,
          bound.transformation concat worldTransformation.matrix
        )
        else frustum.intersectObb(
          bound.min, bound.max,
          bound.transformation
        )
    }
  }
}
