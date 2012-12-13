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

package simplex3d.engine
package util

import simplex3d.math._
import simplex3d.engine.scene._


// Where does animated go? Is this property needed?
trait Updatable[T] {
  def update: T
  
  def controller[A](path: T => A)(function: (A, TimeStamp) => Boolean)
}

private[engine] class PropertyUpdater[T, A] private
  (context: ControllerContext, updatable: Updatable[T])
  (getter: T => A, function: (A, TimeStamp) => Boolean)
extends Updater
{
  def apply(time: TimeStamp) = {
    function(getter(updatable.update), time)
  }
}

private[engine] object PropertyUpdater {
  def register[T, A]
    (context: ControllerContext, isController: Boolean, updatable: Updatable[T])
    (getter: T => A, function: (A, TimeStamp) => Boolean)
  {
    val updater = new PropertyUpdater(context, updatable)(getter, function)
    
    if (isController) context.addController(updater)
    else context.addAnimator(updater)
  }
}
