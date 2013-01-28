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


object InterleavedAttributes extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Interleaved Attributes"
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = true,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
  )
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 100)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
    val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    val (iVertices, iNormals, iTexCoords) = interleave(vertices, normals, texCoords)(vertices.size)
    val originalVertices = vertices.copyAsDataBuffer()
    
    val mesh = new Mesh("Cube")
    
    mesh.geometry.indices := Attributes.fromData(indices)
    // This will be replaced by a macro once Scala 2.10 is out.
    new interleaved(Caching.Stream) {
      mesh.geometry.vertices := Attributes.fromData(iVertices)
      mesh.geometry.normals := Attributes.fromData(iNormals)
      mesh.geometry.texCoords := Attributes.fromData(iTexCoords)
    }.delayedInit()
    
    val noise = ClassicalGradientNoise
    val objectTexture = Texture2d[Vec3](Vec2i(128)).fillWith { p =>
      val intensity = (noise(p.x*0.06, p.y*0.06, 2.324) + 1)*0.5
      Vec3(0, intensity, intensity)
    }
    mesh.material.textureUnits.update += new TextureUnit(objectTexture)
    
    mesh.transformation.update.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
    mesh.transformation.update.scale := 40
    
    mesh.controller { time =>
      def n(i: Int) = noise(time.total*0.8 + i*8.234)*0.15
      def fuzzyMat = Mat4x3(1) + Mat4x3(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7), n(8), 0, 0, 0)
      
      // Interleaved and non-interleaved attributes are updated in the same fashion.
      val data = mesh.geometry.vertices.write
      var i = 0; while (i < data.size) {
        data(i) = fuzzyMat.transformPoint(vertices(i))
        i += 1
      }
      
      true
    }
    
    world.attach(mesh)
  }
  
  def update(time: TimeStamp) {}
}
