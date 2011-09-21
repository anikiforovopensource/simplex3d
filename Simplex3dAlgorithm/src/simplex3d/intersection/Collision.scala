/*
 * Simplex3d, Intersection module
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dAlgorithm.
 *
 * Simplex3dAlgorithm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dAlgorithm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package simplex3d.intersection

import scala.annotation._


/** Collision values.
 * 
 * @author Aleksey Nikiforov (lex)
 */ 
object Collision {
  final val Outside = 0
  final val Intersecting = 1
  final val Inside = 2
  
  def toString(collision: Int) :String = {
    (collision: @switch) match {
      case 0 => "Outside"
      case 1 => "Intersecting"
      case 2 => "Inside"
    }
  }
}
