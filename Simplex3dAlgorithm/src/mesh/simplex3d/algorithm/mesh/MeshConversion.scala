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


object MeshConversion {

  def linesFromTriangles(indices: inData[SInt], vertices: inData[Vec3], resultIndices: Data[SInt]) = {
    val triangles = indices.size/3
    if (resultIndices.size < triangles*6) throw new IllegalArgumentException
    
    var t = 0; while (t < triangles) {

      val i = t*3
      val t0 = indices(i + 0)
      val t1 = indices(i + 1)
      val t2 = indices(i + 2)
      
      val j = t*6
      resultIndices(j + 0) = t0; resultIndices(j + 1) = t1
      resultIndices(j + 2) = t1; resultIndices(j + 3) = t2
      resultIndices(j + 4) = t2; resultIndices(j + 5) = t0
      
      t += 1
    }
  }
}
