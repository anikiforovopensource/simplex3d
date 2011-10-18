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
import simplex3d.intersection.{ Frustum, Collision }
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


abstract class Entity(
  implicit transformationContext: TransformationContext, graphicsContext: GraphicsContext
) extends Bounded {
  
  import PropertyAccess._; import ListenerAccess._
  
  private[this] final val env = graphicsContext.mkEnvironment()
  protected def environment = env
  protected def inheritEnvironment = true
  private[scenegraph] final val worldEnvironment = graphicsContext.mkEnvironment()
  
  private[this] final val _children = ArrayBuffer[SceneElement]()
  private[this] final val readChildren = new ReadSeq(_children)
  protected[scenegraph] def children: ReadSeq[SceneElement] = readChildren

  
  protected def appendChild(element: SceneElement) {
    append(element)
    
    val managed = new ArrayBuffer[Spatial](4)
    element.manageControllerContext(this.controllerContext, managed)
    this.controllerContext.register(managed)
  }
  
  protected def removeChild(element: SceneElement) :Boolean = {
    val removed = remove(element)
    if (removed) { onRemove(element) }
    removed
  }
  
  protected def removeNestedChild(element: SceneElement) :Boolean = {
    val removed = removeNested(element)
    if (removed) { onRemove(element) }
    removed
  }
  
  private[this] def onRemove(element: SceneElement) {
    val managed = new ArrayBuffer[Spatial](4)
    element.manageControllerContext(null, managed)
    this.controllerContext.unregister(managed)
    
    element.worldTransformationVersion = 0
    element match { case b: Bounded => b.boundingVolumeVersion = 0; case _ => }
  }
  
  private[scenegraph] final override def manageControllerContext(
    controllerContext: ControllerContext, managed: ArrayBuffer[Spatial]
  ) {
    super.manageControllerContext(controllerContext, managed)
    
    val size = _children.size
    var i = 0; while (i < size) { val current = _children(i)
      
      current.manageControllerContext(controllerContext, managed)
      
      i += 1
    }
  }
  
  private[this] final def append(element: SceneElement) {
    if (element._parent != null) element._parent.removeChild(element)

    _children += element
    element._parent = this
  }

  private[this] final def remove(element: SceneElement) :Boolean = {
    var removed = false

    val oldSize = _children.size
    _children -= element
    removed = (_children.size != oldSize)

    if (removed) element._parent = null
    removed
  }

  private[this] final def removeNested(element: SceneElement) :Boolean = {
    var removed = removeChild(element)

    val size = _children.size
    var i = 0; while (!removed && i < size) { val current = _children(i)
      current match {
        case entity: Entity => removed = entity.removeNestedChild(element)
        case _ => // do nothing.
      }
    
      i += 1
    }

    removed
  }
  
    
  private[scenegraph] def updateAutoBound() :Boolean = {
    var updated = false
    
    if (customBoundingVolume.hasRefChanges) {
      autoBoundingVolume == null
      customBoundingVolume.clearRefChanges()
      updated = true
    }
    
    if (!customBoundingVolume.isDefined) {
      var updateBounding = false
      val worldTransformation = uncheckedWorldTransformation
      
      
      val size = children.size
      var i = 0; while (i < size) { val child = children(i)
        
        val childBoundingChanged = child match {
          case bounded: Bounded => bounded.updateAutoBound()
          case _ => false
        }
        
        updateBounding = childBoundingChanged || updateBounding
        
        i += 1
      }
      
      if (resolveBoundingVolume == null) {
        autoBoundingVolume = new Aabb
        updateBounding = true
      }
      
      if (updateBounding || worldTransformation.hasChanged) {
        val bound = autoBoundingVolume.asInstanceOf[Aabb]
        Bounded.rebuildAabb(this)(bound.mutable.min, bound.mutable.max)
        updated = true
      }
      
      uncheckedWorldTransformation.clearChanges()
    }
    
    if (resolveBoundingVolume.hasChanged) {
      resolveBoundingVolume.clearChanges()
      updated = true
    }
    
    updated
  }
  
  private[scenegraph] override def cull(
    version: Long, time: TimeStamp, view: View, renderArray: ArrayBuffer[SceneElement]
  ) {
    conditionalCull(true, version, time, view, renderArray)
  }
  
  private[scenegraph] def conditionalCull(
    cullSelf: Boolean, version: Long,
    time: TimeStamp, view: View,
    renderArray: ArrayBuffer[SceneElement]
  ) {
    if (worldTransformationVersion != version) {
      updateWorldTransformation()
      worldTransformationVersion = version
    }
    if (cullSelf && boundingVolumeVersion != version) {
      updateAutoBound()
      boundingVolumeVersion = version
    }
    
    
    val frustumTest = 
      if (!cullSelf) Collision.Inside
      else BoundingVolume.intersect(view.frustum, resolveBoundingVolume, uncheckedWorldTransformation)
    
    val cullChildren = 
      if (frustumTest == Collision.Outside) return
      else if (frustumTest == Collision.Inside) false
      else true
    
    
    def process[E <: EnvironmentalEffect[E]]() {
      if (animators != null && shouldRunAnimators) {
        runUpdaters(animators, time)
        shouldRunAnimators = false
      }
      
      
      // Propagate environment.
      val children = this.children
      val size = children.size
      var i = 0; while (i < size) { val current = children(i)
        
        current match {
          case entity: Entity =>
            
            def propagateEnvironment() {
              val resultEnv = entity.worldEnvironment
      
              val parentProps = this.worldEnvironment.properties.asInstanceOf[ReadArray[EnvironmentalProperty[E]]]
              val childProps = entity.environment.properties.asInstanceOf[ReadArray[EnvironmentalProperty[E]]]
              val resultProps = resultEnv.properties.asInstanceOf[ReadArray[EnvironmentalProperty[E]]]
              
              val size = parentProps.length
              var i = 0; while (i < size) {
                
                val parentProp = parentProps(i)
                val childProp = childProps(i)
                val resultProp = resultProps(i)
                
                if (parentProp.hasChanged || childProp.hasChanged) {
                  def propagateEnvironmentalProperty() {
                    
                    if (parentProp.isDefined) {
                      if (childProp.isDefined) {
                        val structChanges = childProp.defined.propagate(parentProp.defined, resultProp.mutable)
                        if (structChanges) resultEnv.signalStructuralChanges()
                      }
                      else {
                        resultProp.mutable := parentProp.defined
                      }
                    }
                    else {
                      if (childProp.isDefined) {
                        resultProp.mutable := childProp.defined
                      }
                      else {
                        resultProp.undefine()
                      }
                    }
                    
                  }; propagateEnvironmentalProperty()
                  childProp.clearChanges()
                }
                
                i += 1
              }
            }; if (entity.inheritEnvironment) propagateEnvironment()
            
            entity.conditionalCull(cullChildren, version, time, view, renderArray)
          
          case mesh: Mesh =>
            mesh.cull(version, time, view, renderArray)
            
          case bounded: Bounded =>
            bounded.cull(version, time, view, renderArray)
          
          case _ =>
            // do nothing.
        }
        
        i += 1
      }
      
    }; process()
  }
}
