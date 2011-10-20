/*
 * Simplex3dAlgorithm - Shapes Module
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

package simplex3d.algorithm

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._


package object shapes {

  def makeQuad() = (
    DataBuffer[SInt, UByte](
      0, 1, 2, 2, 1, 3
    ),
    DataBuffer[Vec3, RFloat](
      Vec3(-0.5, -0.5, 0), Vec3(0.5, -0.5, 0), Vec3(-0.5, 0.5, 0), Vec3(0.5, 0.5, 0)
    ),
    DataBuffer[Vec3, RFloat](
      Vec3(0, 0, 1), Vec3(0, 0, 1), Vec3(0, 0, 1), Vec3(0, 0, 1) 
    )
  )
  
  def assembleBox() = {
    val (indices, vertices, normals) = makeQuad()
    
    val transformations = Seq(
      Mat3x4 translate(Vec3(0, 0, 0.5)), // front
      Mat3x4 rotateY(radians(180)) translate(Vec3(0, 0, -0.5)), // back
      Mat3x4 rotateY(radians(-90)) rotateX(radians(-90)) translate(Vec3(-0.5, 0, 0)), // left
      Mat3x4 rotateY(radians(90)) rotateX(radians(-90)) translate(Vec3(0.5, 0, 0)), // right
      Mat3x4 rotateX(radians(-90)) rotateY(radians(90)) translate(Vec3(0, 0.5, 0)), // top
      Mat3x4 rotateX(radians(90)) rotateY(radians(90)) translate(Vec3(0, -0.5, 0)) // bottom
    )
    
    (
      DataBuffer[SInt, UByte](
        (for (i <- 0 until 6) yield indices.map(_ + i*4)).flatten : _*
      ),
      DataBuffer[Vec3, RFloat](
        (for (t <- transformations) yield vertices.map(v => t.transformPoint(v))).flatten : _*
      ),
      DataBuffer[Vec3, RFloat](
        (for (t <- transformations) yield normals.map(n => t.transformVector(n))).flatten : _*
      )
    )
  }
  
  def makeBox() = (
    DataBuffer[SInt, UByte](
      0, 1, 2, 2, 1, 3, // front
      4, 5, 6, 6, 5, 7, // back
      8, 9, 10, 10, 9, 11, // left
      12, 13, 14, 14, 13, 15, // right
      16, 17, 18, 18, 17, 19, // top
      20, 21, 22, 22, 21, 23 // bottom
    ),
    DataBuffer[Vec3, RFloat](
      Vec3(-0.5, -0.5, 0.5), Vec3(0.5, -0.5, 0.5), Vec3(-0.5, 0.5, 0.5), Vec3(0.5, 0.5, 0.5),
      Vec3(0.5, -0.5, -0.5), Vec3(-0.5, -0.5, -0.5), Vec3(0.5, 0.5, -0.5), Vec3(-0.5, 0.5, -0.5),
      Vec3(-0.5, -0.5, 0.5), Vec3(-0.5, 0.5, 0.5), Vec3(-0.5, -0.5, -0.5), Vec3(-0.5, 0.5, -0.5),
      Vec3(0.5, 0.5, 0.5), Vec3(0.5, -0.5, 0.5), Vec3(0.5, 0.5, -0.5), Vec3(0.5, -0.5, -0.5),
      Vec3(0.5, 0.5, 0.5), Vec3(0.5, 0.5, -0.5), Vec3(-0.5, 0.5, 0.5), Vec3(-0.5, 0.5, -0.5),
      Vec3(-0.5, -0.5, 0.5), Vec3(-0.5, -0.5, -0.5), Vec3(0.5, -0.5, 0.5), Vec3(0.5, -0.5, -0.5)
    ),
    DataBuffer[Vec3, RFloat](
      Vec3(0, 0, 1), Vec3(0, 0, 1), Vec3(0, 0, 1), Vec3(0, 0, 1),
      Vec3(0, 0, -1), Vec3(0, 0, -1), Vec3(0, 0, -1), Vec3(0, 0, -1),
      Vec3(-1, 0, 0), Vec3(-1, 0, 0), Vec3(-1, 0, 0), Vec3(-1, 0, 0),
      Vec3(1, 0, 0), Vec3(1, 0, 0), Vec3(1, 0, 0), Vec3(1, 0, 0),
      Vec3(0, 1, 0), Vec3(0, 1, 0), Vec3(0, 1, 0), Vec3(0, 1, 0),
      Vec3(0, -1, 0), Vec3(0, -1, 0), Vec3(0, -1, 0), Vec3(0, -1, 0)
    ),
    DataBuffer[Vec2, RFloat](
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1),
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1),
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1),
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1),
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1),
      Vec2(0, 0), Vec2(1, 0), Vec2(0, 1), Vec2(1, 1)
    )
  )
}
