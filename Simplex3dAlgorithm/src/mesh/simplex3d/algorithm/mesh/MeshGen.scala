/*
 * Simplex3dAlgorithm - Mesh Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.algorithm.mesh

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._


/**
 *
 * @author Aleksey Nikiforov (lex)
 */
abstract class MeshGen {
  def gridDimensions(meshDimensions: inVec2, gridWidth: Int) :Vec2i
  def indexSize(gridDimensions: inVec2i) :Int
  def dataSize(gridDimensions: inVec2i) :Int

  def genIndex(
    gridDimensions: inVec2i,
    indices: Index
  ) :Unit

  def genData(
    heightMap: (Double, Double) => Double,
    meshOffset: inVec2,
    meshDimensions: inVec2,
    gridDimensions: inVec2i,
    vertices: Data[Vec3],
    normals: Data[Vec3]
  ) :Unit

  
  /* To find the normal:
   *   - find the partial derivative with respect to x: df/dx = (heightMap(x + e, y) - height)/e
   *   - find the partial derivative with respect to y: df/dy = (heightMap(x, y + e) - height)/e
   *   - find the tangent vector in x direction: tx = Vec3(1, df/dx, 0)
   *   - find the tangent vector in y direction: ty = Vec3(0, df/dy, 1)
   *   - find the cross product between ty and tx: cp = cross(ty, tx)
   *   - normalize the cross product: normal = normalize(cp)
   */
  protected def genNormal(
    x: Double, y: Double, height: Double, sampleInterval: Double,
    heightMap: (Double, Double) => Double
  ) = {
    val e = 1e-2*sampleInterval
    val inve = 1/e
    normalize(Vec3(
      -(heightMap(x + e, y) - height)*inve,
      1,
      -(heightMap(x, y + e) - height)*inve
    ))
  }
}

object QuadMeshGen extends MeshGen {
  def gridDimensions(meshDimensions: inVec2, gridWidth: Int) =
    Vec2i(gridWidth, round(meshDimensions.y/meshDimensions.x*gridWidth).toInt)

  def indexSize(gridDimensions: inVec2i) = {
    gridDimensions.x*gridDimensions.y*2*3
  }
  def dataSize(gridDimensions: inVec2i) = {
    (gridDimensions.x + 1)*(gridDimensions.y + 1)
  }

  def genIndex(
    gridDimensions: inVec2i,
    indices: Index
  ) {
    val rowLength = gridDimensions.x + 1

    var y = 0; while (y < gridDimensions.y) {
      val offset = rowLength + y*rowLength

      var x = 0; while (x < gridDimensions.x) {
        val i = (x + y*gridDimensions.x)*6

        val a = offset - rowLength + x
        val b = offset + x

        indices(i) = a
        indices(i + 1) = b
        indices(i + 2) = b + 1

        indices(i + 3) = b + 1
        indices(i + 4) = a + 1
        indices(i + 5) = a

        x += 1
      }

      y += 1
    }
  }

  def genData(
    heightMap: (Double, Double) => Double,
    meshOffset: inVec2,
    meshDimensions: inVec2,
    gridDimensions: inVec2i,
    vertices: Data[Vec3],
    normals: Data[Vec3]
  ) {
    if (gridDimensions.x < 1 || gridDimensions.y < 1) return

    val sampleInterval = meshDimensions/gridDimensions

    var iy = 0; while (iy < gridDimensions.y + 1) {
      var ix = 0; while (ix < gridDimensions.x + 1) {

        val i = ix + iy*(gridDimensions.x + 1)

        val x = meshOffset.x + ix*sampleInterval.x
        val y = meshOffset.y + iy*sampleInterval.y

        val h = heightMap(x, y)
        vertices(i) = Vec3(x, h.toDouble, y)
        if (normals != null) normals(i) = genNormal(x, y, h, sampleInterval.x, heightMap)

        ix += 1
      }
      iy += 1
    }
  }
}

object TriMeshGen extends MeshGen {
  private final val hratio = 1.15470053837925152902// 2/sqrt(3)

  private def rowOffset(gridDimensions: inVec2i, y: Int) = {
    val evenRowLength = gridDimensions.x + 1
    val oddRowLength = gridDimensions.x + 2
    (y >> 1)*(evenRowLength + oddRowLength) + (y & 0x1)*evenRowLength
  }

  def gridDimensions(meshDimensions: inVec2, gridWidth: Int) = {
    Vec2i(
      gridWidth,
      round(hratio*meshDimensions.y/meshDimensions.x*gridWidth).toInt
    )
  }

  def indexSize(gridDimensions: inVec2i) = {
    val indicesPerRow = (gridDimensions.x*2 + 1)*3
    indicesPerRow*gridDimensions.y
  }
  def dataSize(gridDimensions: inVec2i) = {
    rowOffset(gridDimensions, gridDimensions.y + 1)
  }

  def genIndex(
    gridDimensions: inVec2i,
    indices: Index
  ) {
    val evenRowLength = gridDimensions.x + 1
    val oddRowLength = gridDimensions.x + 2
    val indicesPerRow = (gridDimensions.x*2 + 1)*3

    // even rows
    var y = 0; while (y < gridDimensions.y) {
      val offset = rowOffset(gridDimensions, y)

      var x = 0; while (x < gridDimensions.x) {
        val i = indicesPerRow*y + 6*x

        val a = offset + x
        val b = offset + evenRowLength + x

        indices(i) = a
        indices(i + 1) = b
        indices(i + 2) = b + 1

        indices(i + 3) = a
        indices(i + 4) = b + 1
        indices(i + 5) = a + 1

        x += 1
      }

      // last triangle.
      val i = indicesPerRow*(y + 1) - 3
      val a = offset + evenRowLength - 1
      indices(i) = a
      indices(i + 1) = a + oddRowLength - 1
      indices(i + 2) = a + oddRowLength

      y += 2
    }

    // odd rows.
    y = 1; while (y < gridDimensions.y) {
      val offset = rowOffset(gridDimensions, y)

      var x = 0; while (x < gridDimensions.x) {
        val i = indicesPerRow*y + 6*x

        val a = offset + x
        val b = offset + oddRowLength + x

        indices(i) = a
        indices(i + 1) = b
        indices(i + 2) = a + 1

        indices(i + 3) = a + 1
        indices(i + 4) = b
        indices(i + 5) = b + 1

        x += 1
      }

      // last triangle.
      val i = indicesPerRow*(y + 1) - 3
      val a = offset + oddRowLength - 1
      indices(i) = a - 1
      indices(i + 1) = a + evenRowLength
      indices(i + 2) = a

      y += 2
    }
  }

  def genData(
    heightMap: (Double, Double) => Double,
    meshOffset: inVec2,
    meshDimensions: inVec2,
    gridDimensions: inVec2i,
    vertices: Data[Vec3],
    normals: Data[Vec3]
  ) {
    if (gridDimensions.x < 1 || gridDimensions.y < 1) return

    val sampleInterval = meshDimensions/gridDimensions
    def genVertexData(index: Int, x: Double, y: Double) {
      val h = heightMap(x, y)
      vertices(index) = Vec3(x, h.toDouble, y)
      if (normals != null) normals(index) = genNormal(x, y, h, sampleInterval.x, heightMap)
    }

    // even rows.
    var iy = 0; while (iy < gridDimensions.y + 1) {
      val yoff = rowOffset(gridDimensions, iy)
      val y = meshOffset.y + iy*sampleInterval.y

      var ix = 0; while (ix < gridDimensions.x + 1) {
        val x = meshOffset.x + ix*sampleInterval.x
        val i = ix + yoff
        genVertexData(i, x, y)

        ix += 1
      }

      iy += 2
    }

    // odd rows.
    iy = 1; while (iy < gridDimensions.y + 1) {
      val yoff = rowOffset(gridDimensions, iy)
      val y = meshOffset.y + iy*sampleInterval.y

      // first point.
      {
        val x = meshOffset.x
        val i = yoff
        genVertexData(i, x, y)
      }

      var ix = 1; while (ix < gridDimensions.x + 1) {
        val x = meshOffset.x + (ix - 0.5)*sampleInterval.x
        val i = ix + yoff
        genVertexData(i, x, y)

        ix += 1
      }

      // last point.
      {
        val x = meshOffset.x + meshDimensions.x
        val i = gridDimensions.x + 1 + yoff
        genVertexData(i, x, y)
      }

      iy += 2
    }
  }
}

object Util {
  def buildNormals(
    indices: inIndex,
    vertices: inData[Vec3],
    normals: Data[Vec3]
  ) {
    var i = 0; while (i < normals.size) {
      normals(i) = Vec3.Zero
      i += 1
    }

    i = 0; while (i < indices.size - 2) {

      val i0 = indices(i)
      val i1 = indices(i + 1)
      val i2 = indices(i + 2)

      val v0 = vertices(i0)
      val v1 = vertices(i1)
      val v2 = vertices(i2)

      val normal = normalize(cross(v1 - v0, v2 - v0))

      normals(i0) += normal
      normals(i1) += normal
      normals(i2) += normal

      i += 3
    }

    i = 0; while (i < normals.size) {
      normals(i) = normalize(normals(i))

      i += 1
    }
  }

  def genNormalLines(
    vertices: inData[Vec3],
    normals: inData[Vec3],
    lines: Data[Vec3],
    lineLength: Double
  ) {
    var i = 0; while (i < vertices.size) {
      val v = vertices(i)
      lines(i*2) = v
      lines(i*2 + 1) = v + normals(i)*lineLength

      i += 1
    }
  }
}
