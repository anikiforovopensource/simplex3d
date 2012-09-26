/*
 * Simplex3dEngine - Test Package
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

package simplex3d.test.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.renderer._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._


object FrustumTest extends default.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Frustum Test"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  

  val cube = new Mesh("Cube")
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 100)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    
    cube.geometry.indices := Attributes.fromData(indices)
    cube.geometry.vertices := Attributes.fromData(vertices)
    cube.geometry.normals := Attributes.fromData(normals)
    cube.geometry.texCoords := Attributes.fromData(texCoords)
    
    val objectTexture = Texture2d[Vec3](Vec2i(128))
    objectTexture.fillWith { p =>
      val borderWidth = 5
  
      if (
        p.x < borderWidth || p.x >= objectTexture.dimensions.x - borderWidth ||
        p.y < borderWidth || p.y >= objectTexture.dimensions.y - borderWidth
      ) {
        Vec3(0.1, 0.1, 1)
      }
      else {
        Vec3(0, 0.8, 0.8)
      }
    }
    
    cube.material.textureUnits.update += new TextureUnit(objectTexture)
    
    cube.transformation.update.scale := 20
    cube.transformation.update.rotation := Quat4 rotateX(radians(20)) rotateY(radians(-30)) 
    
    world.attach(cube)
  }
    
  def update(time: TimeStamp) {
    import simplex3d.algorithm.intersection._
    
    val frustum = Frustum(world.camera.viewProjection)
    val intersection = frustum.intersectObb(Vec3(-0.5), Vec3(0.5), cube.worldTransformation.matrix)
    if (intersection != Collision.Inside) println(time.total + ": " + Collision.toString(intersection))
  }

  override def reshape(position: inVec2i, dimensions: inVec2i) {
    val aspect = dimensions.x.toDouble/dimensions.y
    world.camera.projection := perspectiveProj(radians(60), aspect, 20, 100)
  }
}
