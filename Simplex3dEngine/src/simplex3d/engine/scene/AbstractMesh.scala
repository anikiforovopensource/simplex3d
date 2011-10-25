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

import simplex3d.engine.graphics._


trait AbstractMesh extends Spatial with EngineInfo { self =>
  
  final class MeshSubtext {
    val technique = SharedProperty[Technique](StructuralChangeListener.Ignore)
    val elementRange = ValueProperty(ElementRange.Default, StructuralChangeListener.Ignore)
    
    def worldEnvironment = self.worldEnvironment
    
    def hasStructuralChanges :Boolean = {
      geometry.hasStructuralChanges ||
      material.hasStructuralChanges ||
      worldEnvironment.hasStructuralChanges
    }
    
    def resolveElementRange(result: ElementRange) {
      if (!elementRange.isDefined) {
        if (geometry.indices.isDefined) {
          result.first := 0
          result.count := geometry.indices.defined.src.size
        }
        else {
          result.first := 0
          result.count := geometry.vertices.defined.src.size
        }
      }
      else {
        result := elementRange.defined
      }
    }
  }
  private[engine] final val meshSubtext = new MeshSubtext

  val name: String
  final def elementRange = meshSubtext.elementRange
  def geometry: Geometry
  def material: Material
  protected def worldEnvironment: Environment
  
  final def vertexCount :Int = {
    if (elementRange.isDefined) elementRange.defined.count
    else if (geometry.indices.isDefined) geometry.indices.defined.src.size
    else if (geometry.vertices.isDefined) geometry.vertices.defined.src.size
    else 0
  }
}
