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

package simplex3d.engine.util


private[engine] object ClassUtil {
  
  /** Class.getSimpleName() will throw an exception when running interpreted code. This method is a workaround.
   */
  def simpleName(c: Class[_]) :String = {
    val fullName = c.getName
    val index = fullName.lastIndexOf('$')
    if (index != -1) {
      if (index == fullName.length - 1) {
        val anotherIndex = fullName.lastIndexOf('$', index)
        
        if (anotherIndex != -1) fullName.substring(anotherIndex + 1, index)
        else fullName.substring(0, index)
      }
      else fullName.substring(index + 1)
    }
    else c.getSimpleName
  }
}
