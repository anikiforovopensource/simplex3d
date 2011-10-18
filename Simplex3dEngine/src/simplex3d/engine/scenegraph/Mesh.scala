/*
 * Simplex3dEngine - SceneGraph Module
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
package scenegraph

import scala.collection.mutable.ArrayBuffer
import simplex3d.intersection._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._



final class Mesh(
  meshParent: Entity,
  final val geometry: Geometry,
  final val material: Material
)(implicit transformationContext: TransformationContext)
extends Bounded with AbstractMesh {
  
  import PropertyAccess._; import ListenerAccess._
  
  def this()(implicit transformationContext: TransformationContext, graphicsContext: GraphicsContext) {
    this(null, graphicsContext.mkGeometry(), graphicsContext.mkMaterial())
  }
  
  protected def worldEnvironment = parent.worldEnvironment
  
  _parent = meshParent
  override def parent = super.parent
  
  
  private[scenegraph] def updateAutoBound() :Boolean = {
    var updated = false
    
    if (customBoundingVolume.hasRefChanges) {
      autoBoundingVolume == null
      customBoundingVolume.clearRefChanges()
      updated = true
    }
    
    if (!customBoundingVolume.isDefined) {
      if (autoBoundingVolume == null) {
        autoBoundingVolume = new Oabb
      }

      if (autoBoundingVolume.hasChanged || geometry.hasShapeChanges(elementRange)) {
        autoBoundingVolume match {
          case bound: Oabb =>
            Bounded.rebuildAabb(elementRange.defined, geometry)(bound.mutable.min, bound.mutable.max)
        }
        updated = true
      }
    }
    
    uncheckedWorldTransformation.clearChanges()
    
    if (resolveBoundingVolume.hasChanged || uncheckedWorldTransformation.hasChanged) {
      resolveBoundingVolume.clearChanges()
      updated = true
    }
    
    updated
  }
}
