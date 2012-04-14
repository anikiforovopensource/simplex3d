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
import simplex3d.algorithm.intersection._
import simplex3d.engine.util._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._



final class Mesh[T <: TransformationContext, G <: GraphicsContext] private[scenegraph] (
  name: String,
  meshParent: Entity[T, G],
  final val geometry: G#Geometry, // Caution: geometry and material must never be shared among displayable meshes!
  final val material: G#Material  // Caution: geometry and material must never be shared among displayable meshes!
)(implicit transformationCtx: T, graphicsCtx: G)
extends Bounded[T, G](name) with AbstractMesh {
  
  import AccessChanges._
  
  
  def this(name: String)(implicit transformationContext: T, graphicsContext: G) {
    this(name, null, graphicsContext.mkGeometry(), graphicsContext.mkMaterial())
  }
  
  def worldEnvironment = parent.worldEnvironment
  
  _parent = meshParent
  override def parent = super.parent
  
  
  private[scenegraph] override def onParentChange(
    controllerContext: ControllerContext, managed: ArrayBuffer[Spatial[T]]
  ) {
    super.onParentChange(controllerContext, managed)
  }
  
  private[scenegraph] override def update(version: Long) :Boolean = {
    if (updateVersion == version) return false
    
    propagateWorldTransformation()
    updateVersion = version
    
    
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

      if (autoBoundingVolume.hasDataChanges || geometry.hasShapeChanges(elementRange)) {
        autoBoundingVolume match {
          case bound: Oabb =>
            val range = if (elementRange.isDefined) elementRange.get else null
            Bounded.rebuildAabb(range, geometry)(bound.mutable.min, bound.mutable.max)
        }
        updated = true
      }
    }
    
    if (resolveBoundingVolume.hasDataChanges || uncheckedWorldTransformation.hasDataChanges) {
      resolveBoundingVolume.clearDataChanges()
      updated = true
    }
    
    
    elementRange.clearDataChanges()
    geometry.indices.clearRefChanges()
    uncheckedWorldTransformation.clearDataChanges()
    updated
  }
  
  private[scenegraph] override def updateCull(
    version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[AbstractMesh]
  ) {
    if (enableCulling) update(version)
    else updateWorldTransformation(version)
    
    val res =
      if (enableCulling) BoundingVolume.intersect(view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
      else Collision.Inside

    if (res == Collision.Outside) return
    
    def process() {
      if (animators != null && shouldRunAnimators) {
        runUpdaters(animators, time)
        shouldRunAnimators = false
      }
      
      renderArray += this
    }; process()
  }
}
