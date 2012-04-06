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


/** Object Aligned Bounding Box is a special case of Oriented Bounding Box
 * with the volume transformation locked to identity.
 * 
 * When the world transformation is not defined, this bounding box behaves like the
 * Axis Aligned Bonding Box. If world transformation is defined then it
 * will be applied to this bounding box.
 */
final class Oabb(minVector: inVec3 = Vec3.Zero, maxVector: inVec3 = Vec3.Zero) extends BoundingVolume {
  final class MutableSubtext {
    def min: Vec3 = _min
    def max: Vec3 = _max
  }
  private val mutableSubtext = new MutableSubtext
  
  def mutable = {
    dataChanges = true
    mutableSubtext
  }
  
  
  private val _min = minVector.mutableCopy()
  private val _max = maxVector.mutableCopy()
  
  def min: ReadVec3 = _min
  def max: ReadVec3 = _max
  
  def :=(r: Oabb) {
    mutable.min := r.min
    mutable.max := r.max
  }
  
  override def toString() :String = "Oabb(" + min + ", " + max + ")"
}
