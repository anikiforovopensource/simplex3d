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

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.input._
import simplex3d.engine.graphics._
import simplex3d.engine.resource._


trait FullscreenShaderApp extends App {
  
  protected val shader: Shader
  
  protected def viewDimensions: ReadVec2i = viewDims.toConst()
  
  
  addInputListener(new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        if (KeyCode.K_Escape == e.keyCode) dispose()
      }
    }
  })
  
  
  import SubtextAccess._
  
  
  private var effect: FullscreenEffect = _
  private val viewDims = Vec2i(0)
  
  protected final def init() {
    effect = new FullscreenEffect("Fullscreen Shader", shader)
  }
  protected def render(time: TimeStamp) {
    effect.render(renderManager, time)
  }
  protected def reshape(position: inVec2i, dimensions: inVec2i) {
    viewDims := dimensions
  }
  
  
  protected def preUpdate(time: TimeStamp) {}
  protected def manage() {}
}
