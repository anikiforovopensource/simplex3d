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
import simplex3d.algorithm.mesh.Shapes._
import simplex3d.engine._
import simplex3d.engine.renderer._
import simplex3d.engine.app._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.graphics._
import simplex3d.engine.scenegraph._
import simplex3d.engine.impl._
import simplex3d.engine.default._


object InstancingTest extends BasicApp with lwjgl.App {
  val objCount = 3000
  val title = "Instancing Test: " + objCount + " objects."
  
  def main(args: Array[String]) {
    val settings = new Settings(
      fullScreen = false,
      verticalSync = false,
      capabilitiesLog = true,
      performanceLog = true,
      dimensions = Vec2i(800, 600)
    )
    
    launch(settings)
  }
  
  val objRotationSpeed = 1.0
  
  val seed = 2
  val nx = new ClassicalGradientNoise(0)
  val ny = new ClassicalGradientNoise(seed + 1)
  val nz = new ClassicalGradientNoise(seed + 2)
  val offsetScale = 0.05
  val curveSpeed = 0.1
  def curve(index: Int, uptime: Double): Vec3 = {
    val a = index*offsetScale + uptime*curveSpeed
    val b = 28.25896 //uptime*curveSpeed
    Vec3(nx(a, b), ny(a, b), nz(a, b))*100
  }
  
  
  var vVertices: Attributes[Vec3, RFloat] = null
  var boxVertices: DataArray[Vec3, RFloat] = null
  
  val originalTexture = Texture2d(Vec2i(128), DataBuffer[Vec3, UByte](128*128));
  {
    val dims = originalTexture.dimensions
    val data = originalTexture.write
    
    for (y <- 0 until dims.y; x <- 0 until dims.x) {
      data(x + y*dims.x) = Vec3(0, 1, 1)
    }
  }
  val dynamicTexture = Texture2d(Vec2i(64), DataBuffer[Vec3, UByte](64*64))
  
  def init() {
    input.mouse.isGrabbed = true
    
    if (false) {
      val (indices, vertices, normals, texCoords) = makeBox()
      
      val obj = new Mesh("Large Cube")
      
      obj.geometry.faceCulling.mutable := FaceCulling.Back
      
      obj.geometry.indices.defineAs(Attributes(indices))
      obj.geometry.vertices.defineAs(Attributes(vertices))
      obj.geometry.normals.defineAs(Attributes(normals))
      obj.geometry.texCoords.defineAs(Attributes(texCoords))
      
      obj.material.texture.mutable := originalTexture//resources.loadTexture2d[Vec3]("sample/texture.png").get
      obj.transformation.mutable.rotation := Quat4 rotateX(radians(20)) rotateY(radians(-30))
      obj.transformation.mutable.scale := 50
      
      world.attach(obj)
    }
    
    if (objCount != 0) {
      val random = new java.util.Random(3)
      def randomQuat() = normalize(Quat4(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble))
      def smallQuat() = normalize(Quat4(1, random.nextDouble, random.nextDouble, random.nextDouble))
      
      val (indices, vertices, normals, texCoords) = makeBox()
      
      val vIndices = Attributes(indices)
      
      
      // non-interleaved version
      //var vVertices: Attributes[Vec3, RFloat] = null
      var vNormals: Attributes[Vec3, RFloat] = null
      var vTexCoords: Attributes[Vec2, RFloat] = null
      
      vVertices = Attributes(vertices)
      vNormals = Attributes(normals)
      vTexCoords = Attributes(texCoords)
      
      // copy for vertex update effects
      boxVertices = vVertices.read.copyAsDataArray()
      
      
      val node = new InstancingNode("Instancing Node")
      node.instanceBoundingVolume.defineAs(new Oabb(Vec3(-0.5)*1.5, Vec3(0.5)*1.5))
      node.customBoundingVolume.defineAs(new Oabb(Vec3(Double.MinValue), Vec3(Double.MaxValue)))
      
      node.geometry.faceCulling.mutable := FaceCulling.Back
      
      node.geometry.indices.defineAs(vIndices)
      node.geometry.vertices.defineAs(vVertices)
      node.geometry.normals.defineAs(vNormals)
      node.geometry.texCoords.defineAs(vTexCoords)
      
      node.material.texture.mutable := dynamicTexture
      
      world.attach(node)
        
      for (i <- 0 until objCount) {
        val obj = node.appendInstance("Instance" + i)
        obj.transformation.mutable.rotation := randomQuat
        obj.transformation.mutable.translation := curve(i, 0)
        
        val rotation = smallQuat()
        
        // slerp and curve are very expensive calls
        val controller = obj.controller { time: TimeStamp =>
          val q = slerp(Quat4.Identity, rotation, time.interval*objRotationSpeed)
        
          val transformation = obj.transformation.mutable
          transformation.rotation.applyRotation(q)
          transformation.translation := curve(i, time.total)
        }
        controller.isEnabled = false
      }
    }
  }
  
  
  val camControls = new FirstPersonHandler(world.camera.transformation)
  world.camera.transformation.mutable.translation := Vec3(0, 0, 200)
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
    
    val noise = ClassicalGradientNoise
    def n(i: Int) = noise(time.total + i*8.234)*0.25
    val fuzzyMat = Mat3x4(1) + Mat3x4(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7), n(8), 0, 0, 0)
    
    if (vVertices != null) {
      val data = vVertices.write
      var i = 0; while (i < data.size) {
        data(i) = fuzzyMat.transformPoint(boxVertices(i)*2)
        i += 1
      }
    }
    
    val img = dynamicTexture.write
    val a = abs(fract(time.total*0.1) * 2 - 1)
    var y = 0; while (y < dynamicTexture.dimensions.y) {
      var x = 0; while (x < dynamicTexture.dimensions.x) {
        val i = x + y*dynamicTexture.dimensions.x
        img(i) = Vec3((noise(x*0.1, y*0.1, time.total*0.5) + 1)*0.5)
        
        x += 1
      }
      y += 1
    }
    
    val finalImg = originalTexture.write
    finalImg.put2d(originalTexture.dimensions, Vec2i(32), img, dynamicTexture.dimensions)
  }

  
  def reshape(position: inVec2i, dimensions: inVec2i) {
    val aspect = dimensions.x.toDouble/dimensions.y
    world.camera.projection := perspectiveProj(radians(60), aspect, 5, 1000)
  }
}
