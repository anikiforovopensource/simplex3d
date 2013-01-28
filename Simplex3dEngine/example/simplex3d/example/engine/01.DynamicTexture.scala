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


object DynamicTexture extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Dynamic Texture"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 100)
    world.camera.transformation.update.lookAt(Vec3(0), Vec3.UnitY, true)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals)
    
    mesh.geometry.texCoords := Attributes.fromData(texCoords)
    
    val objectTexture = Texture2d[Vec3](Vec2i(128))
    mesh.material.textureUnits.update += new TextureUnit(objectTexture)
    
    val transformation = mesh.transformation.update
    transformation.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    transformation.scale := 50
    
    val noise = ClassicalGradientNoise
    val subTexture = Texture2d[Vec3](Vec2i(64))
    
    mesh.controller { time =>
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
      
      true
    }
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {}
}
