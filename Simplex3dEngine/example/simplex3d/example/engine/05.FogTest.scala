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
object FogTest extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val title = "Fog Test."
  
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
  

  def init() {
    world.camera.transformation.update.translation := Vec3(20, 5, 20)
    world.camera.transformation.update.lookAt(Vec3(20, 0, -100), Vec3.UnitY, true)
    
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
    
    
    val scale = 10
    
    for (i <- 0 until 100) {
      
      val mesh = new Mesh("Cube" + i)
      mesh.customBoundingVolume := new Oabb(Vec3(-0.5)*10, Vec3(0.5)*10)
      
      mesh.geometry.primitive.update.faceCulling := FaceCulling.Back
      
      mesh.geometry.indices := indices
      mesh.geometry.vertices := vertices
      mesh.geometry.normals := normals
      mesh.geometry.texCoords := texCoords
      
      mesh.material.textureUnits.update += new TextureUnit(objectTexture)
      
      mesh.transformation.update.scale := scale
      mesh.transformation.update.translation := Vec3(0, -5, -i*scale*2)

      world.attach(mesh)
    }
    
    
    world.environment.fog.update.color := Vec3(0.5, 0.3, 0.5)
    world.environment.fog.update.density := 0.007
  }
    
  def update(time: TimeStamp) {}
}
