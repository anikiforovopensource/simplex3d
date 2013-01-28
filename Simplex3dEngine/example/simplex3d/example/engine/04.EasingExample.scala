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


object EasingExample extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Easing Example"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  def BackAndForth(speed: Double)(a: ConstVec3, b: ConstVec3): (Vec3, TimeStamp) => Boolean = {
    val period = length(a - b)/speed
    
    (u, time) => {
      val factor = abs((mod(time.total, 2*period) - period)/period)
      u := mix(b, a, factor)
      true // Never terminates.
    }
  }
  
  def Rotate(speed: Double)(center: inVec2): (Mat3x2, TimeStamp) => Boolean = {
    var frameCount = 0
    val offset = -center
    
    (m, time) => {
      // Some feedback when animator is running.
      {
        frameCount += 1
        if (frameCount % 50 == 0) println("Running animator (animator is active only when the object is visible).")
      }
      
      m := Mat3x2 translate(offset) rotate(speed*time.total)
      true // Never terminates.
    }
  }
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 100)
    world.camera.transformation.update.lookAt(Vec3(0), Vec3.UnitY, true)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    val originalVertices = vertices.copyAsDataBuffer()
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals)
    mesh.geometry.texCoords := Attributes.fromData(texCoords)
    
    val noise = new TiledNoise(ClassicalGradientNoise, ConstVec4(128), 0.07)
    val objectTexture = Texture2d[Vec3](Vec2i(128)).fillWith { p =>
      val intensity = (noise(p.x, p.y, 2.324) + 1)*0.5
      Vec3(0, intensity, intensity)
    }
    mesh.material.textureUnits.update += new TextureUnit(objectTexture)
    
    mesh.transformation.update.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.update.scale := 40
    
    
    mesh.transformation.controller(_.translation)(BackAndForth(speed=22)(Vec3(-20, 0, 0), Vec3(20, 0, 0)))
    mesh.material.textureUnits.animator(_(0).transformation)(Rotate(speed=0.4)(center=Vec2(0.5)))
    
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {}
}
