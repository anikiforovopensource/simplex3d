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
package graphics

import simplex3d.math.types._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.scene._
import simplex3d.engine.util._
import simplex3d.engine.graphics.prototype._


abstract class FullscreenEffect(name: String) extends Scene[GraphicsContext](name) { effect =>
  
  protected val fragmentShader: String
  
  private val vertexShader = new Shader(Shader.Vertex,
    """
    attribute vec3 vertices;
    void main() {
      gl_Position = vec4(vertices, 1.0);
    }
    """
  )
  
  private val renderArray = new SortBuffer[AbstractMesh](1)
  
  
  private def mkMesh() = new BaseMesh(effect.name + " Canvas") { self =>
    
    geometry.vertices := Attributes.fromData(DataBuffer[Vec3, RFloat](
      Vec3(-1, -1, 0), Vec3(1, 1, 0), Vec3(-1, 1, 0),
      Vec3(-1, -1, 0), Vec3(1, -1, 0), Vec3(1, 1, 0)
    ))
    
    {
      import AccessScene._
      
      val (names, props) = FieldReflection.valueMap(
        effect,
        classOf[Property[_ <: Binding]], FieldReflection.BindingFilter,
        Nil
      )
      val shaderUniforms = Map((names zip props): _*).asInstanceOf[Map[String, Property[UncheckedBinding]]]
      val fs = new Shader(Shader.Fragment, fragmentShader, shaderUniforms)
      this.technique := new Technique(MinimalGraphicsContext, Set(vertexShader, fs))
    }
  }
  
  
  protected def render(renderManager: RenderManager, time: TimeStamp) {
    if (renderArray.isEmpty) renderArray += mkMesh()
    
    renderManager.render(time, IdentityCamera, renderArray)
  }
  
  protected def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double = 1.0
  protected def update(time: TimeStamp) {}
  protected def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) {}
  protected def cleanup(context: RenderContext) {}
}
