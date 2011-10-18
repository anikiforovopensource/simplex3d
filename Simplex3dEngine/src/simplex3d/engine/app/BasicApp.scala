/*
 * Simplex3dEngine - Core Module
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
package app

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._
import simplex3d.engine.scenegraph._
import simplex3d.engine.input._
import simplex3d.engine.resource._
import simplex3d.engine.default._


trait BasicApp extends App {
  
  // XXX must be independent of renderer or moved somewhere else
  protected val techniqueManager = new renderer.TechniqueManager(default.GraphicsContext)
  
  final class OpenSceneGraph(name: Symbol) extends SceneGraph(name, new Camera, techniqueManager) {
    override def root = super.root
    override def attach(elem: scenegraph.SceneElement) { super.attach(elem) }
    override def detach(elem: scenegraph.SceneElement) :Boolean = super.detach(elem)
  }
  val world = new OpenSceneGraph('World)
  
  
  val exitListener = new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        if (KeyCode.K_Escape == e.keyCode) dispose()
      }
    }
  }
  addInputListener(exitListener)
  
  
  val resources = new ResourceManager {
    loaders += new ClasspathLoader
  }
  
  
  private val renderArray = new InplaceSortBuffer[AbstractMesh]()
  
  // XXX this codes doesnt belong in App
  def render(context: RenderContext, time: TimeStamp) {
    context.clearFrameBuffer()
    
    renderArray.clear()
    world.buildRenderArray(null, time, renderArray)
    renderManager.sortRenderArray(null, renderArray)
    renderManager.render(context, world.camera, renderArray)
  }
  
  def manage(context: RenderContext) {
    world.manage(context, frameTimer, 0.01) //XXX make this value relate to the refresh rate
  }
}
