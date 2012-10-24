package simplex3d.example.data.advanced

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.data._
import simplex3d.data.double._


/**
 * @author Aleksey Nikiforov (lex)
 */
object InterleavedData extends App {

  // Complete tutorial at:
  // http://www.simplex3d.org/tutorials/tutorial-interleaved-data/

  {
    // Example.
    val size = 100
    val stride = 3 + 3 + 2
    val byteBuff = java.nio.ByteBuffer.allocateDirect(size*4*stride)
    byteBuff.order(java.nio.ByteOrder.nativeOrder)
    val vertices = DataView[Vec3, RFloat](byteBuff, 0, stride)
    val normals = DataView[Vec3, RFloat](byteBuff, 3, stride)
    val texCoords = DataView[Vec2, RFloat](byteBuff, 6, stride)

    // Empty interleave.
    val (vertices1, normals1, texCoords1) = interleave(
      DataFactory[Vec3, RFloat],
      DataFactory[Vec3, RFloat],
      DataFactory[Vec2, RFloat]
    )(size)

    // Interleaved copy of existing data.
    val (vertices2, normals2, texCoords2) = interleave(
      vertices,
      normals,
      texCoords
    )(size)

    // PrimitiveSeq layout.
    val prim = vertices.primitives
    vertices(0) == Vec3(prim(0), prim(1), prim(2))
    normals(0) == Vec3(prim(3), prim(4), prim(5))
    texCoords(0) == Vec2(prim(6), prim(7))

    // Addressing primitive sequences
    val index = 10
    val j = index*vertices.stride
    val position10 = Vec3(prim(j), prim(j + 1), prim(j + 2))
    val normal10 = Vec3(prim(j + 3), prim(j + 4), prim(j + 5))
    val texCoord10 = Vec2(prim(j + 6), prim(j + 7))

    // Dedicated factories for primitive sequences
    val prim1 = DataArray[SInt, SInt](100)
    val prim2 = DataBuffer[RDouble, RFloat](100)
  }

}
