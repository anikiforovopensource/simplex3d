package simplex3d.example.engine

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.algorithm.noise._
import simplex3d.algorithm.mesh._
import simplex3d.engine._
import simplex3d.engine.renderer._
import simplex3d.engine.bounding._
import simplex3d.engine.input._
import simplex3d.engine.input.handler._
import simplex3d.engine.scenegraph._
import simplex3d.engine.graphics._


object PointSpritesMesh extends default.App {
  
  def main(args: Array[String]) {
    launch()
  }
  
  val pointCount = 30000
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

  
  val mesh = new Mesh("PointSprites")
  val tempIndexData = DataBuffer[SInt, UShort](pointCount)
  val mapped = new Array[Float](pointCount)
  val tempStore = new Array[Long](pointCount)
  
  def init() {
    world.camera.transformation.update.translation := Vec3(0, 0, 200)
    
    addInputListener(new MouseGrabber(false)(KeyCode.Num_Enter, KeyCode.K_Enter))
    addInputListener(new FirstPersonHandler(world.camera.transformation))
    
    
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
    
    mesh.geometry.mode := PointSprites(1.9)
    mesh.geometry.indices := indices
    mesh.geometry.vertices := Attributes[Vec3, RFloat](pointCount)
    mesh.customBoundingVolume := new Oabb(Vec3(-200), Vec3(200))
    mesh.material.faceCulling.update := FaceCulling.Back
    mesh.material.textureUnits.update += new TextureUnit(objectTexture)
    mesh.transformation.update
    
    var vertices = mesh.geometry.vertices.write//XXX replace attribute.read/wreite with attribute.get/update
    for (i <- 0 until pointCount) {
      vertices(i) = curve(i, 0)
    }
    
    world.attach(mesh)
    
    
    // This is temp hack
    import org.lwjgl.opengl.{ GL11, GL12, GL14 }
    GL11.glEnable(GL11.GL_BLEND)
    GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
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
    
    val indices = mesh.geometry.indices.read
    val vertices = mesh.geometry.vertices.read
    

    // Test sorting with temp buffers
//    val tempIndexData = DataArray[SInt, UShort](pointCount)
//    val mapped = new Array[Float](pointCount)
//    val tempStore = new Array[Long](pointCount)
  
    //Experimental DataSeq sort
    var i = 0; while (i < pointCount) {
      val dir = vertices(indices(i)) + offset
      val ordering = -dot(dir, dir)
      mapped(i) = ordering.toFloat
      
      i += 1
    }
    
    sort(
      indices, 0, pointCount, 1,
      mapped,
      tempIndexData, 0,
      tempStore
    )
    
    mesh.geometry.indices.write.put(tempIndexData)
  }
  
  
  /** first and stride are in units of T
   * count is the number of strides
   * if target is a view, the result must be the 
   */
  def sort[T <: Accessor](
    src: ReadData[T], first: Int, count: Int, stride: Int,
    mapped: Array[Float],
    dest: Data[T], destFirst: Int,
    tempOrder: Array[Long]
  ) {
    import java.nio._
    
    require(first >= 0, "")
    require(count >= 0, "")
    require(stride >= 1, "")
    require(destFirst >= 0, "")
    
    require(mapped.length >= count, "")
    require(dest.size >= destFirst + count*stride, "")
    require(tempOrder.length >= count, "")
    
    require(!src.sharesStorageWith(dest), "")
    
    require(src.accessorManifest == dest.accessorManifest, "")
    require(src.formatManifest == dest.formatManifest, "")
    require(src.rawType == dest.rawType, "")
    require(src.offset == dest.offset, "")
    require(src.stride == dest.stride, "")
    
    
    if (!src.isInstanceOf[Contiguous[_, _]]) {
      reorderByteBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
    }
    else {
      src.primitives.rawType match {
        
        case RawType.SByte | RawType.UByte =>
          reorderByteBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
          
        case RawType.SShort | RawType.HFloat =>
          //reorderShortBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
          
        case RawType.UShort =>
          reorderCharBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
          
        case RawType.SInt | RawType.UInt =>
          //reorderIntBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
          
        case RawType.RFloat =>
          //reorderFloatBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
          
        case RawType.RDouble =>
          reorderDoubleBuffer(mapped, dest, destFirst, src, first, stride, count, tempOrder)
      }
    }
  }
  
  import java.nio._
  import java.util.Arrays
  
  
  def zipFloatByte(f: Float, d: Byte) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | (d & 0xFF)
  }
  
  def zipFloatBytes(f: Float, d0: Byte, d1: Byte, d2: Byte) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | ((d0 & 0xFF) << 16) | ((d1 & 0xFF) << 8) | (d2 & 0xFF)
  }
  
  def zipFloatShort(f: Float, d: Short) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | (d & 0xFFFF)
  }
  
  def zipFloatChar(f: Float, d: Char) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | d
  }
  
  def zipFloatInt(f: Float, d: Int) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | d
  }
  
  def zipFloatFloat(f: Float, d: Float) :Long = {
    val bits = java.lang.Float.floatToRawIntBits(f)
    val lead: Long =
      if (bits < 0) { -bits | 0x80000000 }
      else bits
    
    (lead << 32) | java.lang.Float.floatToRawIntBits(d)
  }
  
  def reorderByteBuffer[T <: Accessor](
    mapped: Array[Float],
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int,
    tempOrder: Array[Long]
  ) {
    val byteStride = src.byteStride
    val chunk = stride*byteStride
    
    val destBuffer = dest.bindingBufferSubData(destFirst, count*stride).asInstanceOf[ByteBuffer]
    val srcBuffer = src.bindingBuffer().asInstanceOf[ByteBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        {
          var i = 0; while (i < count) {
            tempOrder(i) = zipFloatByte(mapped(i), srcBuffer.get(srcFirst + i))
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
        
        {
          var i = 0; while (i < count) {
            destBuffer.put(tempOrder(i).toByte)
            i += 1
          }
        }
      }; single()
      
      case 3 => def tri() {
        {
          var i = 0; while (i < count) {
            val srcIndex = (srcFirst + i*stride)*byteStride
            tempOrder(i) = zipFloatBytes(
              mapped(i),
              srcBuffer.get(srcIndex),
              srcBuffer.get(srcIndex + 1),
              srcBuffer.get(srcIndex + 2)
            )
            
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
        
        {
          var i = 0; while (i < count) {
            val stored = tempOrder(i).toInt
            destBuffer.put((stored >> 16).toByte)
            destBuffer.put((stored >> 8).toByte)
            destBuffer.put(stored.toByte)
            
            i += 1
          }
        }
      }; tri()
    
      case _ => def multipart() {
        {
          var i = 0; while (i < count) {
            tempOrder(i) = zipFloatInt(mapped(i), i)
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
    
        {
          var i = 0; while (i < count) {
            val sortedIndex = tempOrder(i).toInt
            
            val srcIndex = (srcFirst + sortedIndex*stride)*byteStride
            srcBuffer.limit(srcIndex + chunk)
            srcBuffer.position(srcIndex)
            
            destBuffer.put(srcBuffer)
            
            i += 1
          }
        }
      }; multipart()
    }
  }
  
  def reorderCharBuffer[T <: Accessor](
    mapped: Array[Float],
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int,
    tempOrder: Array[Long]
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[CharBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[CharBuffer]
    
    
    chunk match {
      
      case 1 => def single() {
        {
          var i = 0; while (i < count) {
            tempOrder(i) = zipFloatChar(mapped(i), srcBuffer.get(srcFirst + i))
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
        
        {
          var i = 0; while (i < count) {
            destBuffer.put(tempOrder(i).toChar)
            i += 1
          }
        }
      }; single()
      
      case 3 => def tri() {
        {
          var i = 0; while (i < count) {
            tempOrder(i) = zipFloatInt(mapped(i), i)
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
        
        {
          var i = 0; while (i < count) {
            val sortedIndex = tempOrder(i).toInt
          
            val srcIndex = (srcFirst + sortedIndex*stride)*components
            destBuffer.put(srcBuffer.get(srcIndex))
            destBuffer.put(srcBuffer.get(srcIndex + 1))
            destBuffer.put(srcBuffer.get(srcIndex + 2))
            
            i += 1
          }
        }
      }; tri()

      case _ => def multipart() {
        {
          var i = 0; while (i < count) {
            tempOrder(i) = zipFloatInt(mapped(i), i)
            i += 1
          }
          Arrays.sort(tempOrder, 0, count)
        }
        
        {
          var i = 0; while (i < count) {
            val sortedIndex = tempOrder(i).toInt
              
            val srcIndex = (srcFirst + sortedIndex*stride)*components
            srcBuffer.limit(srcIndex + chunk)
            srcBuffer.position(srcIndex)
            
            destBuffer.put(srcBuffer)
            
            i += 1
          }
        }
      }; multipart()
    }
  }
  
  def reorderDoubleBuffer[T <: Accessor](
    mapped: Array[Float],
    dest: Data[T], destFirst: Int,
    src: ReadData[T], srcFirst: Int, stride: Int, count: Int,
    tempOrder: Array[Long]
  ) {
    val components = src.components
    val chunk = stride*components
    
    val destBuffer = dest.buffer().asInstanceOf[DoubleBuffer]
    destBuffer.limit(destFirst*components + count*chunk)
    val srcBuffer = src.readOnlyBuffer().asInstanceOf[DoubleBuffer]
    
    {
      var i = 0; while (i < count) {
        tempOrder(i) = zipFloatInt(mapped(i), i)
        i += 1
      }
      Arrays.sort(tempOrder, 0, count)
    }
    
    
    chunk match {
      
      case 1 => def single() {
        var i = 0; while (i < count) {
          val sortedIndex = tempOrder(i).toInt
          
          val srcIndex = srcFirst + sortedIndex*stride
          destBuffer.put(srcBuffer.get(srcIndex))
          
          i += 1
        }
      }; single()
      
      case 3 => def tri() {
        var i = 0; while (i < count) {
          val sortedIndex = tempOrder(i).toInt
          
          val srcIndex = (srcFirst + sortedIndex*stride)*components
          destBuffer.put(srcBuffer.get(srcIndex))
          destBuffer.put(srcBuffer.get(srcIndex + 1))
          destBuffer.put(srcBuffer.get(srcIndex + 2))
          
          i += 1
        }
      }; tri()
      
      case _ => def multipart() {
        var i = 0; while (i < count) {
          val sortedIndex = tempOrder(i).toInt
            
          val srcIndex = (srcFirst + sortedIndex*stride)*components
          srcBuffer.limit(srcIndex + chunk)
          srcBuffer.position(srcIndex)
          
          destBuffer.put(srcBuffer)
          
          i += 1
        }
      }; multipart()
    }
  }
}
