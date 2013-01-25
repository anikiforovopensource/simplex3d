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
package scene

import simplex3d.math._
import simplex3d.math.double._


trait AbstractCamera extends Spatial {
  val name: String
  
  def view: ReadMat4x3
  def projection: ReadMat4
  
  def viewProjection: ReadMat4
  def inverseViewProjection: ReadMat4
  
  // XXX untested
  def toWorldCoords(normalizedViewCoords: inVec3) :Vec3 = {
    val worldCoords = inverseViewProjection*Vec4(normalizedViewCoords, 1)
    worldCoords.xyz/worldCoords.w
  }

  // XXX untested
  def toNormalizedViewCoords(worldCoords: inVec3) :Vec3 = {
    val transformed = viewProjection*Vec4(worldCoords, 1)
    transformed.xyz/transformed.w
  }
  
  // XXX untested
  def toViewCoords(worldCoords: inVec3, viewDimensions: inVec2i) :Vec3 = {
    val n01 = (toNormalizedViewCoords(worldCoords) + 1)*0.5
    Vec3(viewDimensions*n01.xy, n01.z)
  }
  
  // XXX untested
  def toWorldCoords(viewCoords: inVec3, viewDimensions: inVec2i) :Vec3 = {
    val n01 = viewCoords/Vec3(viewDimensions, 1)
    toWorldCoords(n01*2 - 1)
  }
}


object IdentityCamera extends AbstractCamera {
  val name = "Identity Camera"

  def view: ReadMat4x3 = Mat4x3.Identity
  def projection: ReadMat4 = Mat4.Identity
  
  def viewProjection: ReadMat4 = Mat4.Identity
  def inverseViewProjection: ReadMat4 = Mat4.Identity
}
