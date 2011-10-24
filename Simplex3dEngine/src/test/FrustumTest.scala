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

package test

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.shapes._
import simplex3d.engine._
import simplex3d.engine.renderer._
import simplex3d.engine.app._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._
import simplex3d.engine.impl._
import simplex3d.engine.default._


object FrustumTest extends BasicApp with lwjgl.App {
  val title = "Frustum Test"
  
  def main(args: Array[String]) {
    val settings = new Settings(
      fullScreen = false,
      verticalSync = true,
      capabilitiesLog = true,
      performanceLog = true,
      dimensions = Vec2i(800, 600)
    )
    
    launch(settings)
  }
  
  
  val texture = Texture2d(Vec2i(128), DataBuffer[Vec3, UByte](128*128));
  {
    val data = texture.write
    for (y <- 0 until texture.dimensions.y; x <- 0 until texture.dimensions.x) {
      data(x + y*texture.dimensions.x) = Vec3(0, 1, 1)
    }
  }
  
  val cube = new Mesh
  
  def init() {
    input.mouse.isGrabbed = true
    
    val (indices, vertices, normals, texCoords) = makeBox()
    
    cube.geometry.indices.defineAs(Attributes(indices))
    cube.geometry.vertices.defineAs(Attributes(vertices))
    cube.geometry.normals.defineAs(Attributes(normals))
    cube.geometry.texCoords.defineAs(Attributes(texCoords))
    
    cube.material.texture.mutable := texture
    cube.transformation.mutable.scale := 20
    cube.transformation.mutable.rotation := Quat4 rotateX(radians(20)) rotateY(radians(-30)) 
    
    world.attach(cube)
  }
  
  
  val camControls = new FirstPersonHandler(world.camera.transformation)
  world.camera.transformation.mutable.translation := Vec3(0, 0, 100)
  addInputListener(camControls)
  
  addInputListener(new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        if (KeyCode.Num_Enter == e.keyCode || KeyCode.K_Enter == e.keyCode) {
          input.mouse.isGrabbed = !input.mouse.isGrabbed
          camControls.isEnabled = input.mouse.isGrabbed
        }
      }
    }
  })
  
  
  def update(time: TimeStamp) {
    import simplex3d.algorithm.intersection._
    
    val frustum = Frustum(world.camera.viewProjection)
    val intersection = frustum.intersectObb(Vec3(-0.5), Vec3(0.5), cube.worldTransformation.matrix)
    if (intersection != Collision.Inside) println(time.total + ": " + Collision.toString(intersection))
  }

  
  def reshape(position: inVec2i, dimensions: inVec2i) {
    val aspect = dimensions.x.toDouble/dimensions.y
    world.camera.projection := perspectiveProj(radians(60), aspect, 20, 100)
  }
}
