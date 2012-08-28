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
package graphics

import simplex3d.math.types._
import simplex3d.math._


sealed abstract class ReadElementRange extends Protected {
  type Read = ReadElementRange
  type Mutable = ElementRange
  final def readType = classOf[ReadElementRange]
  
  def first: ReadIntRef
  def count: ReadIntRef
  
  final def mutableCopy() = {
    val copy = new ElementRange
    copy := this
    copy
  }
}

final class ElementRange(firstElement: Int = 0, elementCount: Int = 0)
extends ReadElementRange with Accessible {
  val first = new IntRef(firstElement)
  val count = new IntRef(elementCount)
  
  def :=(e: ReadElementRange) {
    first := e.first
    count := e.count
  }
}

object ElementRange {
  val Default: ReadElementRange = new ElementRange
}
