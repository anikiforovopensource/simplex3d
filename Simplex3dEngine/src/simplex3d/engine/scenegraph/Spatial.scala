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
import simplex3d.engine.scene._
import simplex3d.engine.transformation._


abstract class Spatial private[scenegraph] (
  implicit transformationContext: TransformationContext
) extends scene.Spatial {
  
  private[scenegraph] final var _parent: Entity = _
  protected def parent: Entity = _parent
  
  private[scenegraph] final var controllerContext: ControllerContext = null
  private[scenegraph] final var controllers: ArrayBuffer[Updater] = null
  
  private[scenegraph] final var worldTransformationVersion: Long = 0
  private[scenegraph] final val uncheckedWorldTransformation: Transformation[_] = transformationContext.factory()
  final val transformation: Transformation[_] = transformationContext.factory()
  
  WorldMatrixAccess.setWorldMatrixResolver(this, () => uncheckedWorldTransformation.matrix)
  
  
  final def worldTransformation: ReadTransformation[_] = {
    def update(entity: Entity) :Boolean = {
      val parentUpdated = if (entity.parent != null) update(entity.parent) else false
      
      if (parentUpdated || entity.transformation.hasChanged) {
        entity.updateWorldTransformation()
        true
      }
      else false
    }
    
    if (parent != null) update(parent)
    updateWorldTransformation()
    
    uncheckedWorldTransformation
  }
  
  private[scenegraph] def updateWorldTransformation() {
    def update[T <: ReadTransformation[T]]() {
      val parentTransformation =
        if (parent == null ) null.asInstanceOf[T]
        else parent.uncheckedWorldTransformation.asInstanceOf[T]
      
      val wordTransformation = uncheckedWorldTransformation.asInstanceOf[T#Mutable]
      val childTransformation = transformation.asInstanceOf[T]
      childTransformation.propagateChanges(parentTransformation, wordTransformation)
    }; update()
  }
  
  
  private[scenegraph] def manageControllerContext(
    controllerContext: ControllerContext, managed: ArrayBuffer[Spatial]
  ) {
    this.controllerContext = controllerContext
    if (controllers != null) managed += this
  }
  
  /** Controllers are executed for all objects attached to a scene-graph when it is updated.
   */
  def controller(function: TimeStamp => Unit) :Updater = {
    val updater = new Updater(function)
    addController(updater)
    updater
  }
  
  def addController(controller: Updater) {
    if (controllers == null) {
      controllers = new ArrayBuffer[Updater](4)
      if (controllerContext != null) controllerContext.register(List(this))
    }
    controllers += controller
  }
  
  def removeController(controller: Updater) {
    if (controllers != null) {
      controllers -= controller
      
      if (controllers.isEmpty) {
        controllers = null
        if (controllerContext != null) controllerContext.unregister(List(this))
      }
    }
  }
  
  
  private[scenegraph] final def runUpdaters(updaters: ArrayBuffer[Updater], time: TimeStamp) {
    val size = updaters.size
    var i = 0; while (i < size) {
      updaters(i).apply(time)
      
      i += 1
    }
  }
}
