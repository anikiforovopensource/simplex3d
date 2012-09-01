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

import simplex3d.math.double._


/** Oriented Bounding Box is defined by local min and max and an additional volume transformation.
 * This box is be transformed by the volume transformation first and then by the world transformation.
 */
final class Obb(val min: Vec3 = Vec3(0), val max: Vec3 = Vec3(0), val transformation: Mat4x3 = Mat4x3(1))
extends BoundingVolume
{
  def readType = classOf[Obb]
  def mutableCopy() = new Obb(min.mutableCopy(), max.mutableCopy(), transformation.mutableCopy())
  
  def :=(r: BoundingVolume) {
    this := r.asInstanceOf[Obb]
  }
  
  def :=(r: Obb) {
    min := r.min
    max := r.max
    transformation := r.transformation
  }
  
  override def toString() :String = "Obb(" + min + ", " + max + ", " + transformation + ")"
}
