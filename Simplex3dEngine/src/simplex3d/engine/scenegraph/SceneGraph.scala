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

import scala.collection.mutable._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.algorithm.intersection.{ Frustum, Collision }
import simplex3d.engine.common._
import simplex3d.engine.scene._
import simplex3d.engine.transformation._
import simplex3d.engine.bounding._
import simplex3d.engine.graphics._


class SceneGraph[T <: TransformationContext, G <: GraphicsContext](
  name: String,
  settings: SceneGraphSettings,
  final val camera: Camera[T, G],
  final val techniqueManager: TechniqueManager[G]
)(implicit transformationContext: T)
extends ManagedScene[G](name) {
  
  import SceneAccess._; import ClearChangesAccess._
  
  
  private[this] var version: Long = 0
  private[this] val controllerContext = new ControllerContext(settings.multithreadedControllers)
  
  protected val _root: Node[T, G] = new Node("Root")(transformationContext, techniqueManager.graphicsContext)
  protected def root = _root
  root.controllerContext = controllerContext
  root.customBoundingVolume.defineAs(new Aabb(Vec3(Double.MinValue), Vec3(Double.MaxValue)))
  root.appendChild(camera)
  
  
  def attach(elem: SceneElement[T, G]) {
    root.appendChild(elem)
  }
  def detach(elem: SceneElement[T, G]) :Boolean = {
    root.removeNestedChild(elem)
  }
  
  
  protected def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double = {
    1.0
  }
  
  protected def update(time: TimeStamp) {
    version += 1
    controllerContext.update(time)
  }
  
  
  private val batchArray = new SortBuffer[SceneElement[T, G]](100)
  private val concurrentArray = new ConcurrentSortBuffer[AbstractMesh](100)
  
  protected def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) {
    val camera = if (pass.camera.isDefined) pass.camera.get else this.camera
    
    val frustum = Frustum(camera.viewProjection)
    val view = new View(pass.frameBuffer.dimensions, camera, frustum) //XXX proper dimensions
    
    val allowMultithreading = settings.multithreadedParsing
    if (allowMultithreading) batchArray.clear()
    
    root.entityUpdateCull(
      version, true, time, view, result
    )(
      allowMultithreading, settings.multithreadedParsing_NodesWithChildren,
      batchArray, settings.multithreadedParsing_FromDepth, 0
    )
    
    if (allowMultithreading) {
      concurrentArray.clear()
      
      (0 until batchArray.size).par.foreach { i =>
        batchArray(i) match {
          case b: Bounded[_, _] => b.updateCull(version, true, time, view, concurrentArray)
          case _ => // do nothing.
        }
      }
      result ++= concurrentArray
    }
    
    // Resolve techniques.
    val size = result.size
    var i = 0; while (i < size) { val mesh = result(i).asInstanceOf[Mesh[T, G]]
      
      if (mesh.hasStructuralChanges) {
        val technique = techniqueManager.resolveTechnique(mesh.name, mesh.geometry, mesh.material, mesh.worldEnvironment)
        mesh.technique.defineAs(technique)
        
        mesh.geometry.clearStructuralChanges()
        mesh.material.clearStructuralChanges()
      }
      
      i += 1
    }
    
    i = 0; while (i < size) { val mesh = result(i)
      mesh.worldEnvironment.clearStructuralChanges()
      
      i += 1
    }
  }
  
  protected def manage(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) {
    
  }
  
  protected def cleanup(context: RenderContext) {
    
  }
}
