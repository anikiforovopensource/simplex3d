/*
 * Simplex3dEngine - SceneGraph Module
 * Copyright (C) 2011 - 2012, Aleksey Nikiforov
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
package scene


abstract class Updater {
  final var isEnabled = true
  
  /**
   * @return true to continue running the updater next frame, false to terminate.
   */
  def apply(time: TimeStamp) :Boolean
}


class UpdaterFunction(function: TimeStamp => Boolean) extends Updater {
  def apply(time: TimeStamp) = { if (isEnabled) function(time) else true }
}
