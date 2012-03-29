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

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.engine.input._
import simplex3d.engine.graphics._
import simplex3d.engine.asset._


trait Launcher {
  val driver: String
  
  /** Depending on the implementation, this method will return an appropriate UI element wrapping
   * the rendering surface. If the application is launched in a native window, this method
   * will return null.
   *  
   * @returns a UI element wrapping the rendering surface, or null when launched in a native window. 
   */
  def launch(title: String, settings: Settings) :Object
  
  def detectGraphicsCapabilities() :GraphicsCapabilities
}
