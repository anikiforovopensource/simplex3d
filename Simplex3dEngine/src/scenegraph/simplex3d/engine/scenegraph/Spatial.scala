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
import simplex3d.engine.util._
import simplex3d.engine.scene._
import simplex3d.engine.graphics._
import simplex3d.engine.transformation._


abstract class Spatial[T <: TransformationContext] private[scenegraph] (final val name: String)(
  implicit final val transformationContext: T
) extends scene.Spatial { self =>
  
  protected type Graphics <: graphics.GraphicsContext
  
  import AccessChanges._
  
  
  private[scenegraph] final var _parent: AbstractNode[T, Graphics] = _
  protected def parent: AbstractNode[T, Graphics] = _parent
  
  private[scenegraph] final var controllerManager: ControllerManager = null
  private[scenegraph] final var controllers: ArrayBuffer[Updater] = null
  
  
  final val transformation = TransformationBinding[T#Transformation](transformationContext.factory)
  transformation.register {
    this match {
      case c: ControllerContext => c
      case _ =>
        new ControllerContext() {
          def addController(updater: Updater) { self.addController(updater) }
          def removeController(updater: Updater) { self.removeController(updater) }
          
          def addAnimator(updater: Updater) { addController(updater) }
          def removeAnimator(updater: Updater) { removeController(updater) }
        }
    }
  }
  
  private[scenegraph] final val uncheckedWorldTransformation = {
    TransformationBinding[T#Transformation](transformationContext.factory)
  }
  
  protected def worldMatrix = uncheckedWorldTransformation.matrix
  
  
  final def worldTransformation: TransformationBinding[T#Transformation] = {
    def update(node: AbstractNode[T, _]) :Boolean = {
      val parentUpdated = if (node.parent != null) update(node.parent) else false
      
      if (parentUpdated || node.transformation.hasDataChanges) {
        node.propagateWorldTransformation()
        true
      }
      else false
    }
    
    if (parent != null) update(parent)
    propagateWorldTransformation()
    
    uncheckedWorldTransformation
  }
  
  private[scenegraph] final def propagateWorldTransformation() {
    val parentTransformation = if (parent == null) null else parent.uncheckedWorldTransformation
    Transformation.propagateChanges(parentTransformation, transformation, uncheckedWorldTransformation)
    transformation.clearDataChanges()
  }
  
  private[scenegraph] final def updateWorldTransformation() :Boolean = {
    propagateWorldTransformation()
    val changed = uncheckedWorldTransformation.hasDataChanges
    uncheckedWorldTransformation.clearDataChanges()
    changed
  }
  
  
  private[scenegraph] def onSpatialParentChange(
    parent: AbstractNode[T, _], managed: ArrayBuffer[Spatial[T]]
  ) {
    transformation.signalDataChanges()
    
    if (parent != null) controllerManager = parent.controllerManager
    else controllerManager = null
    
    if (controllers != null) managed += this
  }
  
  /** Controllers are executed for all objects attached to a scene-graph when it is updated.
   */
  def controller(function: TimeStamp => Boolean) :Updater = {
    val updater = new UpdaterFunction(function)
    addController(updater)
    updater
  }
  
  def addController(controller: Updater) {
    if (controllers == null) {
      controllers = new ArrayBuffer[Updater](4)
      if (controllerManager != null) controllerManager.register(ArrayBuffer(this))
    }
    controllers += controller
  }
  
  def removeController(controller: Updater) {
    if (controllers != null) {
      controllers -= controller
      
      if (controllers.isEmpty) {
        controllers = null
        if (controllerManager != null) controllerManager.unregister(ArrayBuffer(this))
      }
    }
  }
  
  
  private[scenegraph] final def runControllers(time: TimeStamp) {
    assert(controllers != null)
    
    val size = controllers.size
    var i = 0; while (i < size) {
      val controller = controllers(i)
      val keep = controller.apply(time)
      if (!keep) removeController(controller)
      
      i += 1
    }
  }
}
