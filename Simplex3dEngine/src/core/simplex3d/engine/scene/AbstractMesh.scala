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
package scene

import simplex3d.math.double._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


trait AbstractMesh extends Spatial with EngineInfo { self =>
  
  final class MeshSubtext {
    import AccessChanges._
    
    def worldMatrix = self.worldMatrix
    
    val technique = SharedRef[Technique](StructuralChangeListener.Ignore)
    val elementRange = Optional(ElementRange.Default)(StructuralChangeListener.Ignore)
    
    def hasStructuralChanges :Boolean = {
      geometry.hasStructuralChanges ||
      material.hasStructuralChanges ||
      worldEnvironment.hasStructuralChanges
    }
    
    def resolveElementRange(result: ElementRange) {
      if (!elementRange.isDefined) {
        if (geometry.indices.isDefined) {
          result.first := 0
          result.count := geometry.indices.get.src.size
        }
        else if (geometry.vertices.isDefined) {
          result.first := 0
          result.count := geometry.vertices.get.src.size
        }
        else {
          result.first := 0
          result.count := 0
        }
      }
      else {
        result := elementRange.get
      }
    }
  }
  private[engine] final val meshSubtext = new MeshSubtext

  val name: String
  final def elementRange = meshSubtext.elementRange
  def geometry: Geometry
  def material: Material
  def worldEnvironment: Environment
  protected def worldMatrix: ReadMat4x3
  
  final def vertexCount :Int = {
    if (elementRange.isDefined) elementRange.get.count
    else if (geometry.indices.isDefined) geometry.indices.get.src.size
    else if (geometry.vertices.isDefined) geometry.vertices.get.src.size
    else 0
  }
}
