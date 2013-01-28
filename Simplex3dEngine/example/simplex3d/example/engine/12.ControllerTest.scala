package simplex3d.example.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.graphics._
import simplex3d.renderer._
import simplex3d.scenegraph._


object ControllerTest extends simplex3d.vanilla.App {
  
  //override lazy val config = new Config(mainLoop = "simplex3d.engine.backend.lwjgl.ParallelLoop")
  
  def main(args: Array[String]) {
    launch()
  }
  
  val objectCount = 3000
  val title = "Controller Test: " + objectCount + " objects."
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = false,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  override lazy val sceneGraphSettings = new SceneGraphSettings(
    multithreadedControllers = true,
    multithreadedParsing = true
  )
  
  
  val seed = 2
  val nx = new ClassicalGradientNoise(0)
  val ny = new ClassicalGradientNoise(seed + 1)
  val nz = new ClassicalGradientNoise(seed + 2)
  val offsetScale = 0.05
  def curve(index: Int, uptime: Double): Vec3 = {
    val a = index*offsetScale + uptime
    val b = 28.25896 //uptime*curveSpeed
    Vec3(nx(a, b), ny(a, b), nz(a, b))*100
  }
  
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 200)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    val objectTexture = Texture2d[Vec3](Vec2i(128))
    objectTexture.fillWith { p =>
      val borderWidth = 20
  
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
  
    
    val (indexBuffer, vertexBuffer, normalBuffer, texCoordBuffer) = Shapes.makeBox()
    val indices = Attributes.fromData(indexBuffer)
    val vertices = Attributes.fromData(vertexBuffer)
    val normals = Attributes.fromData(normalBuffer)
    val texCoords = Attributes.fromData(texCoordBuffer)
    
    
    val scale = 1.7
    val node = new InstancingNode("Instancing Node")
    node.instanceBoundingVolume := new Oabb(Vec3(-0.5)*scale, Vec3(0.5)*scale)
    node.customBoundingVolume := new Oabb(Vec3(Double.MinValue), Vec3(Double.MaxValue))
    
    node.geometry.primitive.update.faceCulling := FaceCulling.Back
    
    node.geometry.indices := indices
    node.geometry.vertices := vertices
    node.geometry.normals := normals
    node.geometry.texCoords := texCoords
    
    node.material.textureUnits.update += new TextureUnit(objectTexture)
    
    world.attach(node)
    
    
    val random = new java.util.Random(3)
    def randomRotation() = normalize(Quat4(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble))
    def smallRandomRotation() = normalize(Quat4(1, random.nextDouble, random.nextDouble, random.nextDouble))
    
    for (i <- 0 until objectCount) {
      
      val instance = node.appendInstance("Instance" + i)
      
      instance.transformation.update.scale := scale
      instance.transformation.update.rotation := randomRotation()
      instance.transformation.update.translation := curve(i, 0)
      
      
      val instanceRotation = smallRandomRotation()
      
      val controller = instance.controller { time: TimeStamp =>
        val rotationIncrement = slerp(Quat4.Identity, instanceRotation, time.interval)
      
        val transformation = instance.transformation.update
        transformation.rotation.applyRotation(rotationIncrement)
        transformation.translation := curve(i, time.total*0.07)
        
        true
      }
      controller.isEnabled = true
    }
  }
    
  def update(time: TimeStamp) {}
}
