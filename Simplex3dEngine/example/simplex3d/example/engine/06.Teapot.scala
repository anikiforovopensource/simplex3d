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
import simplex3d.engine.renderer._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._


object Teapot extends default.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Teapot"
  
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
    
    val start = System.nanoTime
    val (indices, vertices, normals, texCoords) = assetManager.loadObj("simplex3d/example/asset/teapot.obj").get
    val total = System.nanoTime - start
    println("Loading time: " + total*1e-9)
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals.get)
    
    mesh.geometry.texCoords := Attributes.fromData(texCoords.get)
    
    /*val objectTexture = Texture2d[Vec3](Vec2i(128)).fillWith { p =>
      val selector = equal(mod(p, Vec2(8)), Vec2.Zero)
      if (any(selector)) Vec3(selector, 0)
      else Vec3(0.8)
    }*/
    
    val noise = ClassicalGradientNoise
    val objectTexture = Texture2d[Vec3](Vec2i(256)).fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06) + 1)*0.5
      Vec3(0, intensity, intensity) + 0.2
    }
    
    mesh.material.textureUnits.update += new TextureUnit(
      objectTexture, Mat3x2.Identity
    )
    
    val transformation = mesh.transformation.update
    transformation.translation := Vec3(0, -10, 0)
    transformation.rotation := Quat4 rotateY(radians(-180))
    transformation.scale := 2
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {}
}
