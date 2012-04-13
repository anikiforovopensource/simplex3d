package simplex3d.example.engine

import scala.collection.mutable.ArrayBuffer
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
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._
import simplex3d.engine.default._


object DynamicTexture extends DefaultApp {
  val title = "Dynamic Texture"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  
  val noise = ClassicalGradientNoise
  val objectTexture = Texture2d[Vec3](Vec2i(128))
  val subTexture = Texture2d[Vec3](Vec2i(64))

  
  def init() {
    world.camera.transformation.mutable.translation := Vec3(0, 0, 100)
    
    val camControls = new FirstPersonHandler(world.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls)())
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices.defineAs(Attributes(indices))
    mesh.geometry.vertices.defineAs(Attributes(vertices))
    mesh.geometry.normals.defineAs(Attributes(normals))
    
    mesh.geometry.texCoords.defineAs(Attributes(texCoords))
    mesh.material.textures.mutable += objectTexture
    
    mesh.transformation.mutable.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.mutable.scale := 50
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {
    
    // Updating the texture: the changes will be synchronized with OpenGL automatically.
    objectTexture.fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06, time.total*0.4) + 1)*0.5
      Vec3(0, intensity, intensity)
    }
    
    // An example on how to update sub-image.
    subTexture.fillWith { p =>
      val intensity = (noise(p.x*0.12, p.y*0.12, time.total*0.8) + 1)*0.5
      Vec3(0, intensity, 0)
    }
    objectTexture.write.put2d(objectTexture.dimensions, Vec2i(32), subTexture.read, subTexture.dimensions)
  }
}
