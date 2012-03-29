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


object DynamicAttributes extends BasicApp {
  val title = "Dynamic Attributes"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )

  
  val noise = ClassicalGradientNoise
  var vertices: DataBuffer[Vec3, RFloat] = _
  var mesh: Mesh[DT, DG] = _
  
  
  //val objectTexture = assetManager.loadTexture2d[Vec3]("sample/texture.png").get
  val objectTexture = Texture2d(Vec2i(128), DataBuffer[Vec3, UByte](128*128)); {
    val img = objectTexture.write
    
    var y = 0; while (y < objectTexture.dimensions.y) {
      var x = 0; while (x < objectTexture.dimensions.x) {
        
        val i = x + y*objectTexture.dimensions.x
        val intensity = (noise(x*0.06, y*0.06, 2.324) + 1)*0.5
        img(i) = Vec3(0, intensity, intensity)
        
        x += 1
      }
      y += 1
    }
  }
  
  
  def init() {
    world.camera.transformation.mutable.translation := Vec3(0, 0, 100)
    
    val camControls = new FirstPersonHandler(world.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls)())
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    this.vertices = vertices.copyAsDataBuffer()
    
    mesh = new Mesh("Cube")
    
    mesh.geometry.indices.defineAs(Attributes(indices))
    mesh.geometry.vertices.defineAs(Attributes(vertices))
    mesh.geometry.normals.defineAs(Attributes(normals))
    mesh.geometry.texCoords.defineAs(Attributes(texCoords))
    
    mesh.material.texture.mutable := objectTexture
    
    mesh.transformation.mutable.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.mutable.scale := 40
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {
    def n(i: Int) = noise(time.total*0.8 + i*8.234)*0.15
    def fuzzyMat = Mat3x4(1) + Mat3x4(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7), n(8), 0, 0, 0)
    
    // Updating vertex attributes: the changes will be synchronized with OpenGL automatically.
    val data = mesh.geometry.vertices.write
    var i = 0; while (i < data.size) {
      data(i) = fuzzyMat.transformPoint(vertices(i))
      i += 1
    }
  }
}
