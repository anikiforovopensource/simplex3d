/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dEngine.
 *
 * Simplex3dEngine is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dEngine is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.engine
package graphics

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.util._


sealed abstract class ReadPredefinedUniforms {
  val se_viewDimensions: ReadVec2i
  val se_timeTotal: ReadDoubleRef
  val se_timeInterval: ReadDoubleRef
  
  val se_projectionMatrix: ReadMat4
  val se_modelViewMatrix: ReadMat4
  val se_modelViewProjectionMatrix: ReadMat4
  val se_normalMatrix: ReadMat3
  
  val se_pointSize: ReadDoubleRef
}

final class PredefinedUniforms extends ReadPredefinedUniforms {
  val se_viewDimensions = Vec2i(0)
  val se_timeTotal = new DoubleRef(0)
  val se_timeInterval = new DoubleRef(0)
  
  val se_projectionMatrix = Mat4(1)
  val se_modelViewMatrix = Mat4(1)
  val se_modelViewProjectionMatrix = Mat4(1)
  val se_normalMatrix = Mat3(1)
  
  val se_pointSize = new DoubleRef(0)
}

object PredefinedUniforms {
  final val Names = new ReadArray(Array[String](
    "se_viewDimensions",
    "se_timeTotal",
    "se_timeInterval",
    
    "se_projectionMatrix",
    "se_modelViewMatrix",
    "se_modelViewProjectionMatrix",
    "se_normalMatrix",
    
    "se_pointSize"
  ))
}
