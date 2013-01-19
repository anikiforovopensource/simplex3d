/*
 * Simplex3dEngine - Renderer Module
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

package simplex3d.engine.renderer
package lighting

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._


sealed abstract class ReadDirectionalLight {
  def direction: ReadVec3
  def intensity: ReadVec3
}
final class DirectionalLight extends ReadDirectionalLight {
  val direction = Vec3(1)
  val intensity = Vec3(1)
}


sealed abstract class ReadPointLight extends prototype.ReadStruct {
  type Read = ReadPointLight
  type Mutable = PointLight
  
  def position: ReadVec3
  def intensity: ReadVec3
  def linearAttenuation: ReadDoubleRef
  def quadraticAttenuation: ReadDoubleRef
}

final class PointLight extends ReadPointLight with prototype.Struct {
  protected def mkMutable() = new PointLight
  
  val position = Vec3(0)
  val intensity = Vec3(1)
  val linearAttenuation = new DoubleRef(0)
  val quadraticAttenuation = new DoubleRef(0)
  
  private[renderer] val ecPosition = Vec3(0)
  
  init(classOf[PointLight])
}

//XXX ConeLight