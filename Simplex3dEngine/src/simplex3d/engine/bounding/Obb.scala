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


/** Oriented Bounding Box is defined by local min and max and an additional volume transformation.
 * This box is be transformed by the volume transformation first and then by the world transformation.
 */
final class Obb(cmin: inVec3 = Vec3.Zero, cmax: inVec3 = Vec3.Zero, ctransformation: inMat3x4 = Mat3x4.Identity)
extends BoundingVolume {
  final class MutableSubtext {
    def min: Vec3 = _min
    def max: Vec3 = _max
    def transformation: Mat3x4 = _transformation
  }
  private val mutableSubtext = new MutableSubtext
  
  def mutable = {
    dataChanges = true
    mutableSubtext
  }
  
  
  private val _min = cmin.mutableCopy()
  private val _max = cmax.mutableCopy()
  private val _transformation = ctransformation.mutableCopy()
  
  def min: ReadVec3 = _min
  def max: ReadVec3 = _max
  def transformation: ReadMat3x4 = _transformation
  
  def :=(r: Obb) {
    mutable.min := r.min
    mutable.max := r.max
    mutable.transformation := r.transformation
  }
  
  override def toString() :String = "Obb(" + min + ", " + max + ", " + transformation + ")"
}
