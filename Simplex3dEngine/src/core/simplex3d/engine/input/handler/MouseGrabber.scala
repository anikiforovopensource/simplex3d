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
package input
package handler

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine.input._
import simplex3d.engine.transformation._


class MouseGrabber(startWithGrabbedMouse: Boolean)(keys: Int*)
  (mouseGrabbedHandler: InputListener*)(mouseFreeHandler: InputListener*)
extends InputListener {
  
  private var initialized = false
  mouseGrabbedHandler.foreach(_.isEnabled = false)
  mouseFreeHandler.foreach(_.isEnabled = false)
  
  override def update(input: Input, time: TimeStamp) {
    if (!initialized) {
      input.mouse.isGrabbed = startWithGrabbedMouse
      initialized = true
      
      mouseGrabbedHandler.foreach(_.isEnabled = input.mouse.isGrabbed)
      mouseFreeHandler.foreach(_.isEnabled = !input.mouse.isGrabbed)
    }
  }
  
  override val keyboardListener = new KeyboardListener {
    override def keyTyped(input: Input, e: KeyEvent) {
      if (keys.contains(e.keyCode)) {
        input.mouse.isGrabbed = !input.mouse.isGrabbed
        mouseGrabbedHandler.foreach(_.isEnabled = input.mouse.isGrabbed)
        mouseFreeHandler.foreach(_.isEnabled = !input.mouse.isGrabbed)
      }
    }
  }
}
