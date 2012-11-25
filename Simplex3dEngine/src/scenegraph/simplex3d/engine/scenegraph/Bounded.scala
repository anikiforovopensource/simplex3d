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
) extends SceneElement[T, G](name) with ControllerContext {
  
  import AccessChanges._
  
  
  /** Can be initialized to a custom-fit bounding volume, otherwise the SceneGraph will automatically calculate one.
   *  If specified by the user then the Scene Graph will not update the bounding when vertex data is modified.
   *  In this case the user is responsible for updating the bounding volume.
   *  
   *  If the vertex geometry is not accessible (stored only in the GPU memory) then the bounding
   *  volume must be provided by the user.
   */
  final val customBoundingVolume = Reassignable.optional[BoundingVolume]//XXX rename to boundingVolume
  private[scenegraph] final val autoBoundingVolume = Reassignable.optional[BoundingVolume]
  
  private[scenegraph] final var animators: ArrayBuffer[Updater] = null
  
  
  private[scenegraph] final def resolveBoundingVolume(): Reassignable[BoundingVolume] = {
    if (customBoundingVolume.isDefined) customBoundingVolume else autoBoundingVolume
  }
  
  /** Only valid for meshes that were accepted for rendering (in the renderArray).
   * XXX hide this method as debugging only.
   */
  final def boundingVolume: BoundingVolume = resolveBoundingVolume().get
  
  
  /** Animators are executed only for visible objects and only once per frame.
   */
  def animator(function: TimeStamp => Boolean) :Updater = {
    val updater = new UpdaterFunction(function)
    addAnimator(updater)
    updater
  }
  
  def addAnimator(animator: Updater) {
    if (animators == null) animators = new ArrayBuffer[Updater](4)
    animators += animator
  }
  
  private def checkNoAnimators() {
    if (animators.isEmpty) animators = null
  }
  
  def removeAnimator(animator: Updater) {
    if (animators != null) {
      animators -= animator
      checkNoAnimators()
    }
  }
  
  private[scenegraph] final def runAnimators(time: TimeStamp) {
    assert(animators != null)
    
    var removePending: ArrayBuffer[Updater] = null
    
    {
      val size = animators.size
      var i = 0; while (i < size) {
        val animator = animators(i)
        val keep = animator.apply(time)
        if (!keep) {
          if (removePending == null) removePending = new ArrayBuffer[Updater](4)
          removePending += animator
        }
        
        i += 1
      }
    }
    
    if (removePending != null) {
      animators --= removePending
      checkNoAnimators()
    }
  }
  
  /**
   * @return true if parent bounding volume must be adjusted, false otherwise.
   */
  private[scenegraph] def updateBoundingVolume(allowMultithreading: Boolean) :Boolean
  
  private[scenegraph] def cull(
    update: Boolean, enableCulling: Boolean,
    allowMultithreading: Boolean, currentDepth: Int,
    cullContext: CullContext[T, G]
  ) {
    if (update) {
      if (enableCulling) updateBoundingVolume(allowMultithreading)
      else updateWorldTransformation()
    }
    
    val res =
      if (enableCulling) BoundingVolume.intersect(
        cullContext.view.frustum, resolveBoundingVolume.get, uncheckedWorldTransformation
      )
      else Collision.Inside

    if (res == Collision.Outside) return
    
    
    if (update && animators != null) runAnimators(cullContext.time)
    
    cullContext.renderArray += this
  }
}


object Bounded {
  
  /** All the children must have updated transformations and bounds.
   */
  def rebuildAabb(node: AbstractNode[_, _])(resultMin: Vec3, resultMax: Vec3) {
    val size = node.children.size
    if (size == 0) {
      resultMin := Vec3.Zero
      resultMax := Vec3.Zero
      return
    }
    
    resultMin := Vec3(Double.MaxValue)
    resultMax := Vec3(Double.MinValue)
    
    val pmin = Vec3(0)
    val pmax = Vec3(0)
    
    def process[T <: Transformation](bounded: Bounded[_, _], worldTransformation: TransformationBinding[T]) {
      bounded.resolveBoundingVolume().get match {
        case b: Aabb =>
          resultMin := min(resultMin, b.min)
          resultMax := max(resultMax, b.max)
        case b: Oabb =>
          if (worldTransformation.isDefined) {
            intersection.Aabb.projectAabb(b.min, b.max, worldTransformation.matrix)(pmin, pmax)
            resultMin := min(resultMin, pmin)
            resultMax := max(resultMax, pmax)
          }
          else {
            resultMin := min(resultMin, b.min)
            resultMax := max(resultMax, b.max)
          }
        case b: Obb =>
          if (worldTransformation.isDefined) {
            intersection.Aabb.projectAabb(b.min, b.max, b.transformation concat worldTransformation.matrix)(pmin, pmax)
          }
          else {
            intersection.Aabb.projectAabb(b.min, b.max, b.transformation)(pmin, pmax)
          }
          resultMin := min(resultMin, pmin)
          resultMax := max(resultMax, pmax)
      }
    }
    
    var i = 0; while (i < size) { val current = node.children(i)
      
      current match {
        case bounded: Bounded[_, _] => process(bounded, bounded.uncheckedWorldTransformation)
        case _ => // Ignore.
      }
    
      i+= 1
    }
  }
  
  
  def rebuildAabb(elementRange: ReadElementRange, geometry: Geometry)(resultMin: Vec3, resultMax: Vec3) {
    resultMin := Vec3(Double.MaxValue)
    resultMax := Vec3(Double.MinValue)
    
    if (!geometry.vertices.isDefined || !geometry.vertices.get.isAccessible) return
    
    val pointSpriteOffset = geometry.primitive.get.mode.toConst match {
      case VertexMode.PointSprites => geometry.primitive.get.pointSpriteSize*0.5
      case _ => 0.0
    }
    
    val vertices = geometry.vertices.get.read
    if (vertices.size == 0) {
      resultMin := Vec3.Zero
      resultMax := Vec3.Zero
      return
    }
    
    
    if (geometry.indices.isDefined && geometry.indices.get.isAccessible) {
      def rebuildWithIndex() {
        val indices = geometry.indices.get.read
        
        var first = 0
        var count = vertices.size
        if (elementRange != null) {
          first = elementRange.first
          count = elementRange.count
        }
        
        var i = first; while (i < count) {
          val vertex = vertices(indices(i))
          
          resultMin := min(resultMin, vertex - pointSpriteOffset)
          resultMax := max(resultMax, vertex + pointSpriteOffset)
          
          i += 1
        }
      }; rebuildWithIndex()
    }
    else {
      def rebuildAll() {
        val first = 0
        val count = vertices.size
        
        var i = first; while (i < count) {
          val vertex = vertices(i)
          
          resultMin := min(resultMin, vertex - pointSpriteOffset)
          resultMax := max(resultMax, vertex + pointSpriteOffset)
          
          i += 1
        }
      }; rebuildAll()
    }
  }
}
