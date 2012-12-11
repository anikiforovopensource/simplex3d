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

import simplex3d.math._
import simplex3d.math.double._


case class MouseButtonEvent(val buttonCode: Int, val location: Option[ConstVec2i], val time: TimeStamp)
extends InputEvent
{
  override def toString() :String = {
    "MouseButtonEvent(button = " + buttonCode + ", location = " + location + ", " + time + ")"
  }
}

case class MouseMotionEvent(val amount: ConstVec2, val location: Option[ConstVec2i], val time: TimeStamp)
extends InputEvent
{
  override def toString() :String = {
    "MouseMotionEvent(amount = " + amount + ", location = " + location + ", " + time + ")"
  }
}

case class MouseWheelEvent(val amount: Double, val location: Option[ConstVec2i], val time: TimeStamp)
extends InputEvent
{
  override def toString() :String = {
    "MouseWheelEvent(amount = " + amount + ", location = " + location + ", " + time + ")"
  }
}
