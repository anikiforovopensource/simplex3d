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


final class ControllerContext {
  private[this] val controlled = ArrayBuffer[Spatial[_]]()
  
  def register(elements: Seq[Spatial[_]]) {
    controlled ++= elements
  }
  
  def unregister(elements: Seq[Spatial[_]]) {
    controlled --= elements
  }
  
  def update(time: TimeStamp) {
    // XXX must be set via settings
    (0 until controlled.size).par.foreach { i =>
      val c = controlled(i)
      c match {
        case b: Bounded[_] => b.shouldRunAnimators = true
        case _ => // do nothing.
      }
      c.runUpdaters(c.controllers, time)
    }
  }
}
