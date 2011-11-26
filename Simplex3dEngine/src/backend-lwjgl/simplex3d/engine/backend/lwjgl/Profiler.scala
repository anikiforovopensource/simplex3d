/*
 * Simplex3dEngine - LWJGL Module
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

package simplex3d.engine.backend.lwjgl


final class Profiler { // XXX quick testing tool
  private var _last1 = 0L
  private var _last2 = 0L
  private var start = 0L
  
  def begin() {
    start = System.currentTimeMillis()
  }
  def end() {
    _last2 = _last1
    _last1 = System.currentTimeMillis() - start
  }
  
  def last1 = _last1
  def last2 = _last2
  
  override def toString() :String = "Timing(last = " + last1 + ", second last = " + last2 + ")"
}
