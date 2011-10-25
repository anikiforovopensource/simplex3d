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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._


final class FullscreenEffect(name: String, protected val shader: Shader)
extends Scene[GraphicsContext](name) { effect =>
  
  protected val camera = new AbstractCamera {
    val name = effect.name + " Camera"
  
    def view: ReadMat3x4 = Mat3x4.Identity
    def projection: ReadMat4 = Mat4.Identity
    
    def viewProjection: ReadMat4 = Mat4.Identity
    def inverseViewProjection: ReadMat4 = Mat4.Identity
  }
  
  private val vertexShader = new Shader(Shader.VertexShader,
    """
    attribute vec3 vertices;
    void main() {
      gl_Position = vec4(vertices, 1.0);
    }
    """,
    Map()
  )
  
  private val mesh = new AbstractMesh { self =>
    val name = effect.name + " Mesh"
    
    val geometry = DummyGraphicsContext.mkGeometry()
    val material = DummyGraphicsContext.mkMaterial()
    protected val worldEnvironment = DummyGraphicsContext.mkEnvironment()
    new EngineAccess { setWorldMatrixResolver(self, () => Mat3x4.Identity) }
    
    geometry.vertices.defineAs(Attributes(DataBuffer[Vec3, RFloat](
      Vec3(-1, -1, 0), Vec3(1, 1, 0), Vec3(-1, 1, 0),
      Vec3(-1, -1, 0), Vec3(1, -1, 0), Vec3(1, 1, 0)
    )))
    
    import SubtextAccess._
    this.technique.defineAs(new Technique(DummyGraphicsContext, List(vertexShader, shader)))
  }
  
  private val renderArray = new SortBuffer[AbstractMesh](1)
  renderArray += mesh
  
  
  protected def render(renderManager: RenderManager, time: TimeStamp) {
    renderManager.render(camera, renderArray)
  }
  
  protected def cleanup(context: RenderContext) {
    
  }
  
  
  protected def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double = 1.0
  protected def updateControllers(time: TimeStamp) {}
  protected def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) {}
  protected def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) {}
}
