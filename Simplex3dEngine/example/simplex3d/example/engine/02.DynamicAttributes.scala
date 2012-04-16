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


object DynamicAttributes extends DefaultApp {
  val title = "Dynamic Attributes"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    antiAliasingSamples = 4,
    resolution = Some(Vec2i(800, 600))
  )
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 100)
    
    val camControls = new FirstPersonHandler(world.camera.transformation)
    addInputListener(camControls)
    addInputListener(new MouseGrabber(true)(KeyCode.Num_Enter, KeyCode.K_Enter)(camControls)())
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    val originalVertices = vertices.copyAsDataBuffer()
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    mesh.geometry.vertices := Attributes.fromData(vertices)
    mesh.geometry.normals := Attributes.fromData(normals)
    mesh.geometry.texCoords := Attributes.fromData(texCoords)
    
    val noise = ClassicalGradientNoise
    //mesh.material.textures.update += assetManager.loadTexture2d[Vec3]("sample/texture.png").get
    mesh.material.textures.update += Texture2d[Vec3](Vec2i(128)).fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06, 2.324) + 1)*0.5
      Vec3(0, intensity, intensity)
    }
    
    mesh.transformation.update.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.update.scale := 40
    
    mesh.controller { time =>
      def n(i: Int) = noise(time.total*0.8 + i*8.234)*0.15
      def fuzzyMat = Mat4x3(1) + Mat4x3(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7), n(8), 0, 0, 0)
      
      // Updating vertex attributes: the changes will be synchronized with OpenGL automatically.
      val data = mesh.geometry.vertices.write
      var i = 0; while (i < data.size) {
        data(i) = fuzzyMat.transformPoint(originalVertices(i))
        i += 1
      }
    }
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {}
}
