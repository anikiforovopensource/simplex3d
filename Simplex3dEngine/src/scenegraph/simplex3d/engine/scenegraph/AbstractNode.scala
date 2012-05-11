/*
 * Simplex3dEngine - SceneGraph Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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
import simplex3d.algorithm.intersection.{ Frustum, Collision }
import simplex3d.engine.util._
import simplex3d.engine.bounding._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


abstract class AbstractNode[T <: TransformationContext, G <: GraphicsContext] private[scenegraph](name: String)(
  implicit transformationCtx: T, graphicsCtx: G
) extends Bounded[T, G](name) {
  
  import AccessChanges._
  
  private[scenegraph] final var parentWorldEnvironment: G#Environment = _
  private[scenegraph] def worldEnvironment = parentWorldEnvironment
  
  private[this] final val _children = ArrayBuffer[SceneElement[T, G]]()
  private[this] final val readChildren = new ReadSeq(_children)
  protected[scenegraph] def children: ReadSeq[SceneElement[T, G]] = readChildren
  
  private[scenegraph] def appendAnyChild(element: SceneElement[T, G]) {
    append(element)
    
    val managed = new ArrayBuffer[Spatial[T]](4)
    element.onParentChange(this, managed)
    if (controllerContext != null) controllerContext.register(managed)
  }
  
  private[scenegraph] def removeChild(element: SceneElement[_, _]) :Boolean = {
    val elem = element.asInstanceOf[SceneElement[T, G]]
    val removed = remove(elem)
    if (removed) { onRemove(elem) }
    removed
  }
  
  private[scenegraph] def removeNestedChild(element: SceneElement[_, _]) :Boolean = {
    val elem = element.asInstanceOf[SceneElement[T, G]]
    val removed = removeNested(elem)
    if (removed) { onRemove(elem) }
    removed
  }
  
  private[this] def onRemove(element: SceneElement[T, G]) {
    val managed = new ArrayBuffer[Spatial[T]](4)
    element.onParentChange(null, managed)
    if (controllerContext != null) controllerContext.unregister(managed)
  }
  
  private[scenegraph] final override def onParentChange(
    parent: AbstractNode[T, G], managed: ArrayBuffer[Spatial[T]]
  ) {
    super.onParentChange(parent, managed)
    
    if (parent != null) parentWorldEnvironment = parent.worldEnvironment
    else parentWorldEnvironment = null.asInstanceOf[G#Environment]
    
    val size = _children.size
    var i = 0; while (i < size) { val current = _children(i)
      current.onParentChange(parent, managed)
      
      i += 1
    }
  }
  
  private[this] final def append(element: SceneElement[T, G]) {
    if (element._parent != null) element._parent.removeChild(element)

    _children += element
    element._parent = this
  }

  private[this] final def remove(element: SceneElement[T, G]) :Boolean = {
    var removed = false

    val oldSize = _children.size
    _children -= element
    removed = (_children.size != oldSize)

    if (removed) element._parent = null
    removed
  }

  private[this] final def removeNested(element: SceneElement[T, G]) :Boolean = {
    var removed = removeChild(element)

    val size = _children.size
    var i = 0; while (!removed && i < size) { val current = _children(i)
      current match {
        case node: AbstractNode[_, _] => removed = node.removeNestedChild(element)
        case _ => // do nothing.
      }
    
      i += 1
    }

    removed
  }
  
  private[scenegraph] override def updateBoundingVolume(allowMultithreading: Boolean) :Boolean = {
    propagateWorldTransformation()
    
    var updateParentVolume = false
    
    if (customBoundingVolume.hasRefChanges) {
      autoBoundingVolume == null
      customBoundingVolume.clearRefChanges()
      updateParentVolume = true
    }
    
    
    if (!customBoundingVolume.isDefined) {
      val atomicUpdateBounding = new java.util.concurrent.atomic.AtomicBoolean(false)
      var updateBounding = false
      
      def processChild(child: SceneElement[T, G]) {
        val childBoundingChanged = child match {
          case bounded: Bounded[_, _] => bounded.updateBoundingVolume(false)
          case _ => child.updateWorldTransformation()
        }
        atomicUpdateBounding.compareAndSet(false, childBoundingChanged || atomicUpdateBounding.get)
      }
      
      if (allowMultithreading) {
        (0 until children.size).par.foreach(i => processChild(children(i)))
        updateBounding = atomicUpdateBounding.get
      }
      else {
        def processChildren() {
          val size = children.size; var i = 0; while (i < size) {
            processChild(children(i))
            
            i += 1
          }
        }; processChildren()
        updateBounding = atomicUpdateBounding.get
      }
      
      
      if (resolveBoundingVolume == null) {
        autoBoundingVolume = new Aabb
        updateBounding = true
      }
      
      if (updateBounding || uncheckedWorldTransformation.hasDataChanges) {
        val bound = autoBoundingVolume.asInstanceOf[Aabb]
        Bounded.rebuildAabb(this)(bound.update.min, bound.update.max)
        updateParentVolume = true
      }
    }
    
    
    if (resolveBoundingVolume.hasDataChanges) {
      resolveBoundingVolume.clearDataChanges()
      updateParentVolume = true
    }
    
    uncheckedWorldTransformation.clearDataChanges()
    updateParentVolume
  }
  
  private[scenegraph] override def cull(
    update: Boolean,
    enableCulling: Boolean,
    allowMultithreading: Boolean, currentDepth: Int,
    cullContext: CullContext[T, G]
  ) {
    if (update) {
      if (enableCulling) updateBoundingVolume(allowMultithreading)
      else updateWorldTransformation()
    }
    
    val updateChildren = update || customBoundingVolume.isDefined
    
    
    val frustumTest = 
      if (!enableCulling) Collision.Inside
      else BoundingVolume.intersect(cullContext.view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
    
    if (frustumTest != Collision.Outside) {
      if (animators != null && shouldRunAnimators) {
        runUpdaters(animators, cullContext.time)
        shouldRunAnimators = false
      }
    }
    
    val cullChildren = 
      if (frustumTest == Collision.Outside) return
      else if (frustumTest == Collision.Inside) false
      else true
    
    
    val batchChildren =
      if (allowMultithreading && cullContext.batchArray != null) {
        if (currentDepth >= cullContext.batchDepthThreshold || this.children.size >= cullContext.batchChildrenThreshold) {
          true
        }
        else false
      } else false

    
    nodeCull(updateChildren, cullChildren, batchChildren, allowMultithreading, currentDepth, cullContext)
  }
  
  
  private[scenegraph] def nodeCull(
    updateChildren: Boolean, cullChildren:Boolean, batchChildren: Boolean,
    allowMultithreading: Boolean, currentDepth: Int,
    cullContext: CullContext[T, G]
  ) {
    val children = this.children
    val size = children.size; var i = 0; while (i < size) { val current = children(i)
      
      current match {
        
        case bounded: Bounded[_, _] =>
          if (batchChildren) cullContext.batchArray += bounded
          else bounded.cull(updateChildren, cullChildren, allowMultithreading, currentDepth + 1, cullContext)
        
        case _ =>
          current.updateWorldTransformation()
      }
      
      i += 1
    }
  }
}
