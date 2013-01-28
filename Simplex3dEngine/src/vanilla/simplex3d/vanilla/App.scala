/*
 * Simplex3dEngine - Core Module
 * Copyright (C) 2012, Aleksey Nikiforov
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

package simplex3d.vanilla

import simplex3d.engine.transformation._
import simplex3d.engine.graphics._
import simplex3d.engine.input._
import simplex3d.scenegraph._
import simplex3d.renderer._


// Cannot be a trait, due to AccessControlException caused by method invocation routed via trait's implementation.
abstract class App extends BaseApp {
  
  type Transformation = ComponentTransformation3dContext
  type Graphics = simplex3d.renderer.GraphicsContext
  
  implicit final val TransformationContext = new Transformation
  implicit final val GraphicsContext = new Graphics
  
  
  protected val world = new SceneGraph(
    "World",
    sceneGraphSettings,
    new Camera("Main Camera"),
    TechniqueProvider.assembleTechniqueManager()
  )
  
  
  addInputListener(new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        if (KeyCode.K_Escape == e.keyCode) dispose()
      }
    }
  })
}
