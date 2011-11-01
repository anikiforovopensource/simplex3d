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


abstract class Spatial[T <: TransformationContext] private[scenegraph] (final val name: String)(
  implicit transformationContext: T
) extends scene.Spatial { self =>
  
  import ClearChangesAccess._
  
  
  private[scenegraph] final var _parent: Entity[T, _] = _
  protected def parent: Entity[T, _] = _parent
  
  private[scenegraph] final var controllerContext: ControllerContext = null
  private[scenegraph] final var controllers: ArrayBuffer[Updater] = null
  
  final val transformation: T#Transformation = transformationContext.factory()
  private[scenegraph] final var updateVersion: Long = 0
  private[scenegraph] final val uncheckedWorldTransformation: T#Transformation =
    transformationContext.factory()
  
  {
    new EngineAccess {
      setWorldMatrixResolver(self, () => uncheckedWorldTransformation.matrix)
    }
  }
  
  
  final def worldTransformation: T#Transformation#Read = {
    def update(entity: Entity[T, _]) :Boolean = {
      val parentUpdated = if (entity.parent != null) update(entity.parent) else false
      
      if (parentUpdated || entity.transformation.hasDataChanges) {
        entity.propagateWorldTransformation()
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
  
    transformation.asInstanceOf[UncheckedTransformation].propagateChanges(
      parentTransformation.asInstanceOf[UncheckedTransformation],
      uncheckedWorldTransformation.asInstanceOf[UncheckedTransformation]
    )
      
    transformation.clearDataChanges()
  }
  
  private[scenegraph] final def updateWorldTransformation(version: Long) :Boolean = {
    if (updateVersion != version) {
      propagateWorldTransformation()
      updateVersion = version
    }
    val changed = uncheckedWorldTransformation.hasDataChanges
    uncheckedWorldTransformation.clearDataChanges()
    changed
  }
  
  private[scenegraph] def update(version: Long) :Boolean = {
    updateWorldTransformation(version)
  }
  
  
  private[scenegraph] def manageControllerContext(
    controllerContext: ControllerContext, managed: ArrayBuffer[Spatial[T]]
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
