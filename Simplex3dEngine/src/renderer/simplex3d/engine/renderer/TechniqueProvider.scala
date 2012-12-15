/*
 * Simplex3dEngine - Renderer Module
 * Copyright (C) 2012, Aleksey Nikiforov
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
package renderer

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object TechniqueProvider {

  def assembleTechniqueManager[G <: GraphicsContext]()(implicit graphicsContext: G) :pluggable.TechniqueManager[G] = {
    val manager = new pluggable.TechniqueManager[G]
    
    def pushAll(shaders: Seq[ShaderPrototype]) {
      for (shader <- shaders) manager.push(shader)
    }
    
    pushAll(ShaderPack.mkShaders(Profile.Gl2)._2)
    pushAll(fog.ShaderPack.mkShaders(Profile.Gl2)._2)
    pushAll(texturing.ShaderPack.mkShaders(Profile.Gl2)._2)
    pushAll(lighting.ShaderPack.mkShaders(Profile.Gl2)._2)
    
    manager
  }
}
