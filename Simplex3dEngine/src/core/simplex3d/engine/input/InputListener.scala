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


/** The events are propagated by the input system as follows:
 * 1. Update methods are called on all the listeners.
 * 2. Keyboard events are propagated to all the keyboard listeners.
 * 3. Mouse events are propagated to all the mouse listeners.
 * 
 * InputListener users can rely on this order of events.
 * Input systems that do not follow this order are to be considered buggy.
 */
trait InputListener {
  var isEnabled = true
  
  def update(input: Input, time: TimeStamp) {}
  
  val keyboardListener: KeyboardListener = new KeyboardListener {}
  val mouseListener: MouseListener = new MouseListener {}
}
