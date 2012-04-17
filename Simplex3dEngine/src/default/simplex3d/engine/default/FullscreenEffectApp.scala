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
package default

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.engine.graphics._
import simplex3d.engine.scene._
import simplex3d.engine.scenegraph._
import simplex3d.engine.input._
import simplex3d.engine.asset._
import simplex3d.engine.renderer._
import simplex3d.engine.transformation._


trait FullscreenEffectApp extends simplex3d.engine.FullscreenEffectApp {
  
  addInputListener(new InputListener {
    override val keyboardListener = new KeyboardListener {
      override def keyTyped(input: Input, e: KeyEvent) {
        println(this, "ESC")
        if (KeyCode.K_Escape == e.keyCode) dispose()
      }
    }
  })
  
  lazy val config = new Config
  lazy val settings = new Settings
}
