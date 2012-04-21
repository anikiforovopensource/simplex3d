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
    
    element.updateVersion = 0
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
  
  private[scenegraph] override def update(version: Long) :Boolean = {
    nodeUpdate(version)(false, 0)
  }
  
  private[scenegraph] final def nodeUpdate
    (version: Long)
    (allowMultithreading: Boolean, minChildren: Int)
  :Boolean =
  {
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
      val atomicUpdateBounding = new java.util.concurrent.atomic.AtomicBoolean(false)
      var updateBounding = false
      
      def processChild(child: SceneElement[T, G])(allowMultithreading: Boolean, minChildren: Int) {
        val childBoundingChanged = child match {
          case node: AbstractNode[_, _] => node.nodeUpdate(version)(allowMultithreading, minChildren)
          case _ => child.update(version)
        }
        atomicUpdateBounding.compareAndSet(false, childBoundingChanged || atomicUpdateBounding.get)
      }
      
      if (allowMultithreading && children.size >= minChildren) {
        (0 until children.size).par.foreach(i => processChild(children(i))(false, 0))
        updateBounding = atomicUpdateBounding.get
      }
      else {
        def processChildren() {
          val size = children.size; var i = 0; while (i < size) {
            processChild(children(i))(allowMultithreading, minChildren)
            
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
        updated = true
      }
    }
    
    
    if (resolveBoundingVolume.hasDataChanges) {
      resolveBoundingVolume.clearDataChanges()
      updated = true
    }
    
    uncheckedWorldTransformation.clearDataChanges()
    updated
  }
  
  private[scenegraph] override def updateCull(
    version: Long, enableCulling: Boolean, time: TimeStamp, view: View, renderArray: SortBuffer[AbstractMesh]
  ) {
    nodeUpdateCull(
      version, true, time, view, renderArray
    )(false, 0, null, 0, 0)
  }
  
  /**
   * @returns true when completely culled, false when partially culled or not culled at all.
   */
  private[scenegraph] def nodeUpdateCull(
    version: Long, enableCulling: Boolean,
    time: TimeStamp, view: View,
    renderArray: SortBuffer[AbstractMesh]
  )(
    allowMultithreading: Boolean, minChildren: Int,
    batchArray: SortBuffer[SceneElement[T, G]], maxDepth: Int, currentDepth: Int
  ) {
    if (enableCulling) nodeUpdate(version)(allowMultithreading, minChildren)
    else updateWorldTransformation(version)
    
    
    val frustumTest = 
      if (!enableCulling) Collision.Inside
      else BoundingVolume.intersect(view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
    
    if (frustumTest != Collision.Outside) {
      if (animators != null && shouldRunAnimators) {
        runUpdaters(animators, time)
        shouldRunAnimators = false
      }
    }
    
    
    val cullChildren = 
      if (frustumTest == Collision.Outside) return
      else if (frustumTest == Collision.Inside) false
      else true
    
    var  batchChildren = false
    if (allowMultithreading && batchArray != null) {
      if (currentDepth >= maxDepth) {
        batchChildren = true
      }
      else if (this.children.size >= minChildren) {
        batchChildren = true
      }
    }
    
    var hasLeafs = false
    val children = this.children
    val size = children.size; var i = 0; while (i < size) { val current = children(i)
      
      current match {
        case envNode: EnvrionmentNode[_, _] =>
          
          def propagateEnvironment() {
            val parentEnv = worldEnvironment
            val childEnv = envNode.environment
            val resultEnv = envNode.worldEnvironment
    
            val parentProps = parentEnv.properties
            val childProps = childEnv.properties
            val resultProps = resultEnv.properties
            
            val size = parentProps.length; var i = 0; while (i < size) {
              
              val parentProp = parentProps(i)
              val childProp = childProps(i)
              val resultProp = resultProps(i)
              
              if (parentProp.hasDataChanges || childProp.hasDataChanges) {
                if (parentProp.isDefined) {
                  if (childProp.isDefined) {
                    childProp.get.propagate(parentProp.get, resultProp.update)
                  }
                  else {
                    resultProp.update := parentProp.get
                  }
                }
                else {
                  if (childProp.isDefined) {
                    resultProp.update := childProp.get
                  }
                  else {
                    resultProp.undefine()
                  }
                }

                childProp.clearDataChanges()
              }
              
              i += 1
            }
          }; if (envNode.combineEnvironment) propagateEnvironment()

          
          if (batchChildren) batchArray += envNode
          else envNode.nodeUpdateCull(
            version, cullChildren, time, view, renderArray
          )(allowMultithreading, minChildren, batchArray, maxDepth, currentDepth + 1)
        
        case node: AbstractNode[_, _] =>
          if (batchChildren) batchArray += node
          else node.nodeUpdateCull(
            version, cullChildren, time, view, renderArray
          )(allowMultithreading, minChildren, batchArray, maxDepth, currentDepth + 1)
        
        case mesh: Mesh[_, _] =>
          hasLeafs = true
          if (batchChildren && cullChildren) batchArray += mesh
          else mesh.updateCull(version, cullChildren, time, view, renderArray)
          
        case bounded: Bounded[_, _] =>
          hasLeafs = true
          if (batchChildren && cullChildren) batchArray += bounded
          else bounded.updateCull(version, cullChildren, time, view, renderArray)
        
        case _ =>
          hasLeafs = true
          current.update(version)
      }
      
      i += 1
    }
    
    
    def postPropagation() {
      val props = worldEnvironment.properties
      val size = props.length; var i = 0; while (i < size) { val prop = props(i)
        if (prop.hasDataChanges) {
          if (hasLeafs && prop.isDefined && prop.get.hasBindingChanges) {
            worldEnvironment.signalStructuralChanges()
            prop.get.clearBindingChanges()
          }
          prop.clearDataChanges()
        }
        
        i += 1
      }
    }; if (this.isInstanceOf[EnvrionmentNode[_, _]]) postPropagation()
  }
}
