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
import simplex3d.scenegraph._


object ObbTest extends default.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Obb Test"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  
  val movingCube = new Mesh("Moving Cube")
  val cube1 = new Mesh("Left Cube")
  val cube2 = new Mesh("Right Cube")
  val cubeScale = 10
  val translation = ConstVec3(20, 0, 0)
  
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 60)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(movingCube.transformation))
    
    
    val texture = Texture2d[Vec3](Vec2i(128))
    texture.fillWith { p =>
      val borderWidth = 5
  
      if (
        p.x < borderWidth || p.x >= texture.dimensions.x - borderWidth ||
        p.y < borderWidth || p.y >= texture.dimensions.y - borderWidth
      ) {
        Vec3(0.1, 0.1, 1)
      }
      else {
        Vec3(0, 0.8, 0.8)
      }
    }
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    val aindices = Attributes.fromData(indices)
    val avertices = Attributes.fromData(vertices)
    val anormals = Attributes.fromData(normals)
    val atexCoords = Attributes.fromData(texCoords)
    
    
    movingCube.geometry.indices := aindices
    movingCube.geometry.vertices := avertices
    movingCube.geometry.normals := anormals
    movingCube.geometry.texCoords := atexCoords
    
    movingCube.material.textureUnits.update += new TextureUnit(texture)
    
    movingCube.transformation.update.rotation := Quat4 rotateX(radians(20)) rotateY(radians(-30))
    movingCube.transformation.update.scale := cubeScale
    
    world.attach(movingCube)
    
    
    cube1.geometry.indices := aindices
    cube1.geometry.vertices := avertices
    cube1.geometry.normals := anormals
    cube1.geometry.texCoords := atexCoords
    
    cube1.material.textureUnits.update += new TextureUnit(texture)
    
    cube1.transformation.update.scale := cubeScale
    cube1.transformation.update.translation := -translation
    
    world.attach(cube1)
    
    
    cube2.geometry.indices := aindices
    cube2.geometry.vertices := avertices
    cube2.geometry.normals := anormals
    cube2.geometry.texCoords := atexCoords
    
    cube2.material.textureUnits.update += new TextureUnit(texture)
    
    cube2.transformation.update.rotation := Quat4 rotateX(radians(-15)) rotateZ(radians(30))
    cube2.transformation.update.scale := cubeScale
    cube2.transformation.update.translation := translation
    
    world.attach(cube2)
  }
    
  
  def update(time: TimeStamp) {
    //import simplex3d.algorithm.intersection._
    
    val min = Vec3(-0.5)
    val max = Vec3(0.5)

    val intersection1 = Obb.intersectAabb(min, max, movingCube.worldTransformation.matrix)(min*cubeScale - translation, max*cubeScale - translation)
    if (intersection1 != Vec3.Zero) println("Aabb " + time.total + ": " + intersection1)
    
    val intersection2 = Obb.intersectObb(min, max, movingCube.worldTransformation.matrix)(min, max, cube2.worldTransformation.matrix)
    if (intersection2 != Vec3.Zero) println("Obb " + time.total + ": " + intersection2)
  }
}


import simplex3d.algorithm.intersection._
object Obb {
  
  /** Intersect dynamic Obb (dmin, dmax, dtransformation) with a static Aabb (smin, smax).
   * The result is a vector by which the dynamic box has to be displaced to resolve the collision.
   */
  def intersectAabb(dmin: inVec3, dmax: inVec3, dtransformation: inMat4x3)(smin: inVec3, smax: inVec3) :Vec3 = {
    intersectObb(dmin, dmax, dtransformation)(smin, smax, Mat4x3.Identity)
  }
  
  /** Intersect dynamic Obb (dmin, dmax, dtransformation) with a static Obb (smin, smax, stransformation).
   * The result is a vector by which the dynamic box has to be displaced to resolve the collision.
   */
  def intersectObb
    (dmin: inVec3, dmax: inVec3, dtransformation: inMat4x3)
    (smin: inVec3, smax: inVec3, stransformation: inMat4x3)
  :Vec3 = {
    Vec3(0)
  }
}
