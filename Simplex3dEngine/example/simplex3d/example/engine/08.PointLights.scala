package simplex3d.example.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.renderer._
import simplex3d.scenegraph._


object PointLights extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "PointLights"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 20, 30)
    world.camera.transformation.update.lookAt(Vec3(0), Vec3.UnitY, true)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    // Setup teapot
    val (indices, vertices, normals, texCoords) = assetManager.loadObj("simplex3d/example/asset/teapot.obj").get
    
    val mesh = new Mesh("Teapot")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals.get)
    
    mesh.geometry.texCoords := Attributes.fromData(texCoords.get)
    
    val noise = ClassicalGradientNoise
    val objectTexture = Texture2d[Vec3](Vec2i(256)).fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06) + 1)*0.5
      Vec3(intensity, intensity, intensity) + 0.2
    }
    
    mesh.material.textureUnits.update += new TextureUnit(objectTexture)
    
    val transformation = mesh.transformation.update
    transformation.translation := Vec3(0, -10, 0)
    transformation.rotation := Quat4 rotateY(radians(-180))
    transformation.scale := 2
    
    world.attach(mesh)
    
    
    // Setup point lights
    val pointLight1 = new PointLight()
    world.environment.lighting.update.pointLights += pointLight1
    
    val lightEntity = new Mesh("Point Light")
    lightEntity.geometry.vertices := Attributes[Vec3, RFloat](1)
    lightEntity.geometry.primitive.update.mode := VertexMode.PointSprites
    lightEntity.geometry.primitive.update.pointSize := 100
    lightEntity.geometry.primitive.update.pointSpriteSize := 1
    
    val dims = Vec2(128)
    val lightTexture = Texture2d[Vec4](Vec2i(dims))
    lightTexture.fillWith { p =>
      val outerRadius = dims.x *0.5
      val len = length(p - dims * 0.5)
      val blendPixels = 4
      
      if (len < outerRadius + blendPixels*0.5) {
        val factor = (outerRadius - len)/blendPixels
        mix(Vec4(1, 1, 1, 0), Vec4(1), clamp(factor, 0, 1))
      }
      else {
        Vec4.Zero
      }
    }
    
    lightEntity.material.textureUnits.update += new TextureUnit(lightTexture)
    
    world.attach(lightEntity)
    
    lightEntity.controller { time =>
      lightEntity.transformation.update.translation := Quat4.rotateY(time.total).rotateVector(Vec3(15, 10, 0))
      true
    }
    
    lightEntity.controller { time =>
      pointLight1.position := lightEntity.transformation.get.translation
      world.environment.lighting.update
      true
    }
  }
  
  def update(time: TimeStamp) {}
}
