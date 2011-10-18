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

import scala.collection.mutable.ArrayBuffer


abstract class Scene(val name: Symbol) {
  val camera: AbstractCamera
  
  
  // preload some content within a soft bound given by timeSlice, return the overall completion 0-started, 1-done.
  def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double
 
  def updateControllers(time: TimeStamp) :Unit
  
  // combine attribs while culling
  def buildRenderArray(pass: Pass, time: TimeStamp, result: InplaceSortBuffer[AbstractMesh]) :Unit
  
  // discard unwanted meshes
  def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Unit

  def cleanup(context: RenderContext) :Unit
}
