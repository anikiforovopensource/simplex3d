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


object PointSpritesMesh extends simplex3d.vanilla.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val pointCount = 3000
  val title = "PointSprites: " + pointCount + " points."
  
  override lazy val settings = new Settings(
    fullscreen = false,
    verticalSync = false,
    logCapabilities = true,
    logPerformance = true,
    resolution = Some(Vec2i(800, 600))
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

  
  val mesh = new Mesh("PointSprites") {
    // Still a temp hack, but a more robust one.
    import org.lwjgl.opengl.GL11
    
    override def preRender() {
      GL11.glEnable(GL11.GL_BLEND)
      GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
    }
    
    override def postRender() {
      GL11.glDisable(GL11.GL_BLEND)
    }
  }
  
  val tempIndexData = DataBuffer[SInt, UShort](pointCount)
  val sortContext = new DataSort(pointCount)
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 200)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    {
      val objectTexture = Texture2d[Vec4](Vec2i(128))
      objectTexture.fillWith { p =>
        val innerRadius = 44
        val outerRadius = 64
    
        val len = length(p - Vec2(64))
        
        if (len < innerRadius - 2) {
          Vec4(0, 0.8, 0.8, 1)
        }
        else if (len < innerRadius + 2) {
          val factor = (innerRadius - len)*0.25
          mix(Vec4(0.1, 0.1, 1, 1), Vec4(0, 0.8, 0.8, 1), clamp(factor, 0, 1))
        }
        else if (len < outerRadius - 2) {
          Vec4(0.1, 0.1, 1, 1)
        }
        else if (len < outerRadius + 2) {
          val factor = (outerRadius - len)*0.25
          mix(Vec4(0.1, 0.1, 1, 0), Vec4(0.1, 0.1, 1, 1), clamp(factor, 0, 1))
        }
        else {
          Vec4.Zero
        }
      }
      
      
      val indices = Attributes[SInt, UShort](pointCount)
      val indexData = indices.write
      for (i <- 0 until indexData.size) indexData(i) = i
      
      mesh.geometry.primitive.update.mode := VertexMode.PointSprites
      // Setting large point size for PointSprites prevents culling when point center is offscreen.
      mesh.geometry.primitive.update.pointSize := 500
      mesh.geometry.primitive.update.pointSpriteSize := 1.9
      
      mesh.geometry.indices := indices
      mesh.geometry.vertices := Attributes[Vec3, RFloat](pointCount)
      mesh.customBoundingVolume := new Oabb(Vec3(-200), Vec3(200))
      mesh.geometry.primitive.update.faceCulling := FaceCulling.Back
      mesh.material.textureUnits.update += new TextureUnit(objectTexture)
      mesh.transformation.update
      
      var vertices = mesh.geometry.vertices.write//XXX replace attribute.read/wreite with attribute.get/update
      for (i <- 0 until pointCount) {
        vertices(i) = curve(i, 0)
      }
      
      world.attach(mesh)
    }
    
    {// An object to test blending.
      val (indices, vertices, normals, texCoords) = Shapes.makeBox()
    
      val mesh = new Mesh("Cube")
      
      mesh.geometry.indices := Attributes.fromData(indices)
      mesh.geometry.vertices := Attributes.fromData(vertices)
      mesh.geometry.normals := Attributes.fromData(normals)
      
      mesh.geometry.texCoords := Attributes.fromData(texCoords)
      
      val noise = new TiledNoise(ClassicalGradientNoise, ConstVec4(128), 0.01)
      val objectTexture = Texture2d[Vec3](Vec2i(128)).fillWith { p =>
        val intensity = (noise(p.x, p.y, 2.324) + 1)*0.5
        Vec3(0.8, 0, 1)*intensity
      }
      mesh.material.textureUnits.update += new TextureUnit(objectTexture)
      
      val transformation = mesh.transformation.update
      transformation.rotation := Quat4 rotateX(radians(25)) rotateY(radians(-30))
      transformation.scale := 50
      
      world.attach(mesh)
    }
  }

  
  def update(time: TimeStamp) {
    // Animate points
    val writableVertices = mesh.geometry.vertices.write
    for (i <- 0 until pointCount) {
      writableVertices(i) = curve(i, time.total*0.07)
    }
    
    //Sort point sprites
    val camPosition = world.camera.transformation.get.translation
    val meshPosition = mesh.transformation.get.translation
    val offset = meshPosition - camPosition
    val camDir = -world.camera.transformation.get.direction()
    
    val indices = mesh.geometry.indices.read
    val vertices = mesh.geometry.vertices.read
    

    // Test sorting with temp buffers
//    val tempIndexData = DataArray[SInt, UShort](pointCount)
//    val mapped = new Array[Float](pointCount)
//    val tempStore = new Array[Long](pointCount)
  
    //Experimental DataSeq sort
    var i = 0; while (i < pointCount) {
      val dir = vertices(indices(i)) + offset
      val ordering = -dot(camDir, dir)
      
      sortContext.map(i, ordering.toFloat)
      
      i += 1
    }
    
    sortContext.sort(pointCount)
    indices.reorder(sortContext, 1, tempIndexData)
    mesh.geometry.indices.write.put(tempIndexData)
  }
}
