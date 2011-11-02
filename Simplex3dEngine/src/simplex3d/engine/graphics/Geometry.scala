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
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._


abstract class Geometry extends StructuralChangeListener {
  
  def attributeNames: ReadArray[String]
  def attributes: ReadArray[UncheckedAttributes]

  
  final def hasShapeChanges(elementRange: OptionalProperty[ElementRange] = null) :Boolean = {
    if (elementRange != null && elementRange.hasDataChanges) {
      true
    }
    else if (elementRange != null && elementRange.isDefined) {
      val indexChanges =
        if (indices.hasRefChanges) true
        else if (indices.isDefined) indices.defined.sharedState.hasDataChanges // TODO check for data changes in range
        else false
        
      if (indexChanges) true
      else {
        if (vertices.hasRefChanges) true
        else if (vertices.isDefined) vertices.defined.sharedState.hasDataChanges // TODO check for data changes in range if !indices.isDefined
        else false
      }
    }
    else {
      val indexChanges =
        if (indices.hasRefChanges) true
        else if (indices.isDefined) indices.defined.sharedState.hasDataChanges
        else false
        
      if (indexChanges) true
      else {
        if (vertices.hasRefChanges) true
        else if (vertices.isDefined) vertices.defined.sharedState.hasDataChanges
        else false
      }
    }
  }

  
  final val indices = SharedAttributes[SInt, Unsigned](StructuralChangeListener.Ignore)
  final val vertices = SharedAttributes[Vec3, RFloat](this)
  final val normals = SharedAttributes[Vec3, RFloat](this)
  
  final val mode = SharedRef[VertexMode](StructuralChangeListener.Ignore)
  final val faceCulling = DefinedProperty[EnumRef[FaceCulling.type]](new EnumRef(FaceCulling.Disabled))
  
  
  final def copyNonattributes(geometry: Geometry) {
    mode.set(geometry.mode)
    faceCulling.mutable := geometry.faceCulling.mutable
  }
}
