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
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm._
import simplex3d.algorithm.intersection.Collision
import simplex3d.engine.util._
import simplex3d.engine.bounding._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.graphics._


abstract class Bounded[T <: TransformationContext, G <: GraphicsContext] private[scenegraph] (name: String)(
  implicit transformationContext: T, graphicsContext: G
) extends SceneElement[T, G](name) {
  
  import ClearChangesAccess._
  
  
  /** Can be initialized to a custom-fit bounding volume, otherwise the SceneGraph will automatically calculate one.
   *  If specified by the user then the Scene Graph will not update the bounding when vertex data is modified.
   *  In this case the user is responsible for updating the bounding volume.
   *  
   *  If the vertex geometry is not accessible (stored only in the GPU memory) then the bounding
   *  volume must be provided by the user.
   */
  final val customBoundingVolume = SharedRef[BoundingVolume](StructuralChangeListener.Ignore)
  private[scenegraph] final var autoBoundingVolume: BoundingVolume = null
  
  private[scenegraph] final var shouldRunAnimators = false
  private[scenegraph] final var animators: ArrayBuffer[Updater] = null
  
  
  private[scenegraph] final def resolveBoundingVolume: BoundingVolume = {
    if (customBoundingVolume.isDefined) customBoundingVolume.defined else autoBoundingVolume
  }
  
  final def boundingVolume = {
    if (customBoundingVolume.isDefined) customBoundingVolume.defined
    else {
      update(updateVersion - 1)
      autoBoundingVolume
    }
  }
  
  
  /** Animators are executed only for visible objects and only once per frame.
   */
  protected def animator(function: TimeStamp => Unit) :Updater = {
    val updater = new Updater(function)
    addAnimator(updater)
    updater
  }
  
  protected def addAnimator(animator: Updater) {
    if (animators == null) animators = new ArrayBuffer[Updater](4)
    animators += animator
  }
  
  protected def removeAnimator(animator: Updater) {
    if (animators != null) animators -= animator
  }
  
  
  private[scenegraph] def updateCull(
    version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[AbstractMesh]
  )
}


object Bounded {
  
  /** All the children must have updated transformations and bounds.
   */
  def rebuildAabb(entity: Entity[_, _])(resultMin: Vec3, resultMax: Vec3) {
    val size = entity.children.size
    if (size == 0) {
      resultMin := Vec3.Zero
      resultMax := Vec3.Zero
      return
    }
    
    resultMin := Vec3(Double.MaxValue)
    resultMax := Vec3(Double.MinValue)
    
    val pmin = Vec3(0)
    val pmax = Vec3(0)
    
    def process(bounded: Bounded[_, _], worldTransformation: ReadTransformation[_]) {
      bounded.resolveBoundingVolume match {
        case b: Aabb =>
          resultMin := min(resultMin, b.min)
          resultMax := max(resultMax, b.max)
        case b: Oabb =>
          if (worldTransformation.isSet) {
            intersection.Aabb.projectAabb(b.min, b.max, worldTransformation.matrix)(pmin, pmax)
            resultMin := min(resultMin, pmin)
            resultMax := max(resultMax, pmax)
          }
          else {
            resultMin := min(resultMin, b.min)
            resultMax := max(resultMax, b.max)
          }
        case b: Obb =>
          if (worldTransformation.isSet) {
            intersection.Aabb.projectAabb(b.min, b.max, b.transformation concat worldTransformation.matrix)(pmin, pmax)
          }
          else {
            intersection.Aabb.projectAabb(b.min, b.max, b.transformation)(pmin, pmax)
          }
          resultMin := min(resultMin, pmin)
          resultMax := max(resultMax, pmax)
      }
    }
    
    var i = 0; while (i < size) { val current = entity.children(i)
      
      current match {
        case bounded: Bounded[_, _] => process(bounded, bounded.uncheckedWorldTransformation)
        case _ => // Ignore.
      }
    
      i+= 1
    }
  }
  
  
  def rebuildAabb(elementRange: ReadElementRange, geometry: Geometry)(resultMin: Vec3, resultMax: Vec3) {
    if (!geometry.vertices.isDefined || !geometry.vertices.defined.isAccessible) return
    
    val vertices = geometry.vertices.defined.read
    if (vertices.size == 0) {
      resultMin := Vec3.Zero
      resultMax := Vec3.Zero
      return
    }
    
    resultMin := Vec3(Double.MaxValue)
    resultMax := Vec3(Double.MinValue)
    
    val badIndex = (geometry.indices.isDefined && !geometry.indices.defined.isAccessible)
    var first = 0
    var count = vertices.size
    if (elementRange != null && !badIndex) {
      first = elementRange.first
      count = elementRange.count
    }
    
    if (geometry.indices.isDefined && geometry.indices.defined.isAccessible) {
      def rebuildWithIndex() {
        val indices = geometry.indices.defined.read
        
        var i = first; while (i < count) {
          val vertex = vertices(indices(i))
          
          resultMin := min(resultMin, vertex)
          resultMax := max(resultMax, vertex)
          
          i += 1
        }
      }; rebuildWithIndex()
    }
    else {
      def rebuildVertices() {
        var i = first; while (i < count) {
          val vertex = vertices(i)
          
          resultMin := min(resultMin, vertex)
          resultMax := max(resultMax, vertex)
          
          i += 1
        }; rebuildVertices()
      }
    }
  }
}