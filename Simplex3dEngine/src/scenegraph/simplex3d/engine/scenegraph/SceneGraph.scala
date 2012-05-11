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
import simplex3d.engine.util._
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
  
  import AccessScene._; import AccessChanges._
  
  
  private[this] val controllerContext = new ControllerContext(settings.multithreadedControllers)
  
  protected val _root = new EnvrionmentNode("Root")(transformationContext, techniqueManager.graphicsContext)
  protected def root: EnvrionmentNode[T, G] = _root
  root.controllerContext = controllerContext
  root.customBoundingVolume := new Aabb(Vec3(Double.MinValue), Vec3(Double.MaxValue))
  root.appendChild(camera)
  
  
  def attach(elem: SceneElement[T, G]) {
    root.appendChild(elem)
  }
  def detach(elem: SceneElement[T, G]) :Boolean = {
    root.removeNestedChild(elem)
  }
  
  def environment = root.worldEnvironment
  
  
  protected def preload(context: RenderContext, frameTimer: FrameTimer, timeSlice: Double) :Double = {
    // TODO Implement two preloading modes: preload everything, and preload extended visual range.
    // TODO Add a loading screen with an image and a progress bar, integrate it with the Scenegraph.
    1.0
  }
  
  protected def update(time: TimeStamp) {
    controllerContext.update(time)
  }
  
  
  private val batchArray = new SortBuffer[SceneElement[T, G]](100)
  private val concurrentArray = new ConcurrentSortBuffer[SceneElement[T, G]](100)
  
  protected def buildRenderArray(pass: Pass, time: TimeStamp, result: SortBuffer[AbstractMesh]) {
    val camera = if (pass.camera.isDefined) pass.camera.get else this.camera
    
    val frustum = Frustum(camera.viewProjection)
    val view = new View(pass.frameBuffer.dimensions, camera, frustum) //XXX proper dimensions
    
    val allowMultithreading = settings.multithreadedParsing
    if (allowMultithreading) batchArray.clear()
    
    val cullContext = new CullContext(
      result.asInstanceOf[SortBuffer[SceneElement[T, G]]],
      time, view,
      settings.multithreadedParsing_NodesWithChildren, settings.multithreadedParsing_FromDepth,
      batchArray
    )
    root.cull(true, true, allowMultithreading, 0, cullContext)
    
    if (allowMultithreading) {
      concurrentArray.clear()
      
      val cullContext = new CullContext(
        concurrentArray,
        time, view,
        settings.multithreadedParsing_NodesWithChildren, settings.multithreadedParsing_FromDepth,
        null
      )
      
      (0 until batchArray.size).par.foreach { i =>
        batchArray(i) match {
          case b: Bounded[_, _] => b.cull(true, true, false, 0, cullContext)
          case _ => // do nothing.
        }
      }
      
      result ++= concurrentArray.asInstanceOf[SortBuffer[AbstractMesh]]
    }
    
    // Resolve techniques.
    val size = result.size
    var i = 0; while (i < size) { val mesh = result(i).asInstanceOf[Mesh[T, G]]
      
      if (mesh.hasStructuralChanges) {
        val technique = techniqueManager.resolveTechnique(
          mesh.name, mesh.geometry, mesh.material, mesh.worldEnvironment)
          
        if (technique != null) mesh.technique := technique else mesh.technique.undefine()
        
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
    // TODO add gradual texture and VBO loading for off-screen objects.
  }
  
  protected def cleanup(context: RenderContext) {
    //XXX implement
  }
}
