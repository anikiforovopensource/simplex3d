package example.simplex3d.tutorials

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object DataBindingAPI extends Application {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-data-binding-api-intro/

  {
    // Examples.
    val vertices = DataArray[Vec3, RFloat](100)
    val triangles = DataArray[Vec3i, UShort](50)
    val texture = DataArray[Vec4, UByte](128*128)
    val hdrTexture = DataArray[Vec3, HFloat](128*128)

    // Using direct Buffers.
    val vertices1 = DataBuffer[Vec3, RFloat](100)
    val triangles1 = DataBuffer[Vec3i, UShort](50)
    val texture1 = DataBuffer[Vec4, UByte](128*128)
    val hdrTexture1 = DataBuffer[Vec3, HFloat](128*128)

    // New DataArray.
    DataArray[Vec3, RFloat](100)
    DataArray[Vec3, RFloat](new Array[Float](3*100))
    DataArray[Vec3, RFloat](Vec3(0), Vec3(1), Vec3(100))

    // New DataBuffer.
    DataBuffer[Vec3, RFloat](100)
    val buff = java.nio.ByteBuffer.allocateDirect(4*3*100)
    buff.order(java.nio.ByteOrder.nativeOrder)
    DataBuffer[Vec3, RFloat](buff)
    DataBuffer[Vec3, RFloat](Vec3(0), Vec3(1), Vec3(100))

    // Common supertype.
    def foo(data: DataSeq[Vec3, RFloat]) {}
    foo(vertices)
    foo(vertices1)

    // Reading and writing.
    vertices(0) = Vec3(1, 2, 3)
    val vertex = vertices(0)

    // Mixing math operations with reading and writing.
    val position = Vec3(10, 0, 10)
    vertices(0) = vertices(0) + position
    vertices(1) += position
    var i = 0; while (i < vertices.size) { vertices(i) += position; i += 1 }

    // Data conversion
    val doubleVec = vertices(0)
    vertices(0) = Vec3(1/3.0)
    vertices(0) != Vec3(1/3.0) // lost precision

    // Read only
    val ro = vertices.asReadOnly()
    // ro(0) = Vec3(0) // will not compile
  }

}
