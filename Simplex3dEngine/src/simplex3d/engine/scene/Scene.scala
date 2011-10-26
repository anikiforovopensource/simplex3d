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
import simplex3d.engine.graphics._


abstract class Scene[G <: GraphicsContext](val name: String) { self =>
  final class SceneSubtext {
    def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double = {
      self.preload(context, frameTimer, timeSlice)
    }
    def update(time: TimeStamp) {
      self.update(time)
    }
    def render(renderManager: RenderManager, time: TimeStamp) {
      self.render(renderManager, time)
    }
    def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) {
      self.manage(context, frameTimer, timeSlice)
    }
    def cleanup(context: RenderContext) {
      self.cleanup(context)
    }
  }
  private[engine] final val sceneSubtext = new SceneSubtext
  
  
  // preload some content within a soft bound given by timeSlice, return the overall completion 0-started, 1-done.
  protected def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double
 
  protected def update(time: TimeStamp) :Unit
  
  protected def render(renderManager: RenderManager, time: TimeStamp) :Unit
  
  // discard unwanted meshes
  protected def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Unit

  protected def cleanup(context: RenderContext) :Unit
}
