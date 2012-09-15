/*
 * Simplex3dEngine - SceneGraph Module
 * Copyright (C) 2011-2012, Aleksey Nikiforov
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

import java.util.Collections
import java.util.HashSet
import java.util.ArrayList
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions


final class ControllerManager(val enableMultithreading: Boolean) {
  private[this] val set = new HashSet[Spatial[_]]()
  private[this] val syncSet = Collections.synchronizedSet(set)
  private[this] val list = new ArrayList[Spatial[_]]()
  
  
  def register(elements: ArrayBuffer[Spatial[_]]) {
    syncSet.addAll(JavaConversions.bufferAsJavaList(elements))
  }
  
  def unregister(elements: ArrayBuffer[Spatial[_]]) {
    syncSet.removeAll(JavaConversions.bufferAsJavaList(elements))
  }
  
  def update(time: TimeStamp) {
    list.clear()
    list.addAll(set)// Copying to list allows to add/remove controllers from existing controllers.
    
    def processSpatial(spatial: Spatial[_]) {
      spatial.runUpdaters(spatial.controllers, time)
    }
    
    if (enableMultithreading) {
      (0 until list.size).par.foreach(i => processSpatial(list.get(i)))
    } else {
      def processControlled() {
        val size = list.size; var i = 0; while (i < size) {
          processSpatial(list.get(i))
          i += 1
        }
      }; processControlled()
    }
  }
}
