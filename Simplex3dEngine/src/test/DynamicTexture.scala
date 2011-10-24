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
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._
import simplex3d.engine.impl._
import simplex3d.engine.default._


object DynamicTexture extends BasicApp with lwjgl.App {
  val title = "Dynamic Texture"
  
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
  
  
  val noise = ClassicalGradientNoise
  
  val objectTexture = Texture2d(Vec2i(128), DataBuffer[Vec3, UByte](128*128))
  val subImgDims = ConstVec2i(64)
  val subImg = DataBuffer[Vec3, UByte](subImgDims.x*subImgDims.y)
  
  
  def init() {
    world.camera.transformation.mutable.translation := Vec3(0, 0, 100)
    
    val camControls = new FirstPersonHandler(world.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls))
    
    
    val (indices, vertices, normals, texCoords) = makeBox()
    
    val obj = new Mesh
    
    obj.geometry.faceCulling.undefine()
    
    obj.geometry.indices.defineAs(Attributes(indices))
    obj.geometry.vertices.defineAs(Attributes(vertices))
    obj.geometry.normals.defineAs(Attributes(normals))
    obj.geometry.texCoords.defineAs(Attributes(texCoords))
    
    obj.material.texture.mutable := objectTexture//resources.loadTexture2d[Vec3]("sample/texture.png").get
    obj.transformation.mutable.rotation := Quat4 rotateX(radians(20)) rotateY(radians(-30))
    obj.transformation.mutable.scale := 50
    
    world.attach(obj)
  }
  
  def update(time: TimeStamp) {
    
    def writeImg(img: Data[Vec3], dims: inVec2i)(function: (Int, Int) => ReadVec3) {
      var y = 0; while (y < dims.y) {
        var x = 0; while (x < dims.x) {
          
          val i = x + y*dims.x
          img(i) = function(x, y)
          
          x += 1
        }
        y += 1
      }
    }
    
    // Updating the texture: the changes will be synchronized with OpenGL automatically.
    writeImg(objectTexture.write, objectTexture.dimensions) { (x, y) =>
      val intencity = (noise(x*0.06, y*0.06, time.total*0.4) + 1)*0.5
      Vec3(0, intencity, intencity)
    }
    
    // An example on how to update sub-image.
    if (true) {
      writeImg(subImg, subImgDims) { (x, y) =>
        val intencity = (noise(x*0.12, y*0.12, time.total*0.8) + 1)*0.5
        Vec3(0, intencity, 0)
      }
      objectTexture.write.put2d(objectTexture.dimensions, Vec2i(32), subImg, subImgDims)
    }
  }
  
  def reshape(position: inVec2i, dimensions: inVec2i) {
    val aspect = dimensions.x.toDouble/dimensions.y
    world.camera.projection := perspectiveProj(radians(60), aspect, 10, 500)
  }
}
