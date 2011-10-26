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


abstract class ManagedScene[G <: GraphicsContext](name: String) extends Scene[G](name) { self =>
  final class ManagedSceneSubtext {
    def camera = self.camera
    def techniqueManager = self.techniqueManager
    
    def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) {
      self.buildRenderArray(pass, time, result)
    }
  }
  private[engine] final val managedSceneSubtext = new ManagedSceneSubtext
  
  
  protected val camera: AbstractCamera
  protected val techniqueManager: TechniqueManager[G]
  
   /** Update and combine environmental effects while culling.
    */
  protected def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) :Unit
  
  protected def render(renderManager: RenderManager, time: TimeStamp) {
    techniqueManager.passManager.render(renderManager, time, this)
  }
}
