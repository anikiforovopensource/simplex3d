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


/** Test raw glUniform() and glDrawElemenets() performance.
 */
object StressTestObjects extends simplex3d.vanilla.App {
  
  //override lazy val config = new Config(mainLoop = "simplex3d.engine.backend.lwjgl.ParallelLoop")
  
  def main(args: Array[String]) {
    launch()
  }
  
  val objectCount = 3000
  val title = "Stress Test: " + objectCount + " objects."
  
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
    val (iVertices, iNormals, iTexCoords) = interleave(vertexBuffer, normalBuffer, texCoordBuffer)(vertexBuffer.size)
    
    val indices = Attributes.fromData(indexBuffer)
    var vertices: Attributes[Vec3, RFloat] = null
    var normals: Attributes[Vec3, RFloat] = null
    var texCoords: Attributes[Vec2, RFloat] = null
    
    new interleaved(Caching.Stream) {
      vertices = Attributes.fromData(iVertices)
      normals = Attributes.fromData(iNormals)
      texCoords = Attributes.fromData(iTexCoords)
    }.delayedInit()
    
    
    val scale = 1.7
    val random = new java.util.Random(3)
    def randomRotation() = normalize(Quat4(random.nextDouble, random.nextDouble, random.nextDouble, random.nextDouble))
    
    for (i <- 0 until objectCount) {
      
      val mesh = new Mesh("Cube" + i)
      mesh.customBoundingVolume := new Oabb(Vec3(-0.5)*scale, Vec3(0.5)*scale)
      
      mesh.geometry.primitive.update.faceCulling := FaceCulling.Back
      
      mesh.geometry.indices := indices
      mesh.geometry.vertices := vertices
      mesh.geometry.normals := normals
      mesh.geometry.texCoords := texCoords
      
      mesh.material.textureUnits.update += new TextureUnit(objectTexture)
      
      mesh.transformation.update.scale := scale
      mesh.transformation.update.rotation := randomRotation()
      mesh.transformation.update.translation := curve(i, 0)
      
      world.attach(mesh)
    }
  }
  
  private var start = 0L
  private var frameCount = 0
  def update(time: TimeStamp) {
    frameCount match {
      case 0 => start = System.nanoTime
      case 1 => println("First frame: " + (System.nanoTime - start)*1e-9)
      case _ =>
    }
    frameCount += 1
  }
}
