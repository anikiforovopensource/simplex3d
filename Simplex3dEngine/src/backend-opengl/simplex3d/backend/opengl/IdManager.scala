/*
 * Simplex3dEngine - GL Module
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

package simplex3d.backend.opengl

import java.nio.IntBuffer
import simplex3d.data._


class IdManager(val idCacheSize: Int)(genIds: IntBuffer => Unit, deleteIds: IntBuffer => Unit) {
  private val ids = DataBuffer[SInt, SInt](idCacheSize)
  private var i = ids.size
  
  def nextId() = {
    if (i >= ids.size) {
      genIds(ids.buffer)
      i = 0
    }
    val id = ids(i)
    i += 1
    id
  }
  
  
  private val discard = DataBuffer[SInt, SInt](idCacheSize)
  private var j = 0
  
  def release(id: Int) {
    discard(j) = id
    j += 1
    
    if (j >= discard.size) {
      deleteIds(discard.buffer)
      j = 0
    }
  }
  
  def releasePending() {
    val buff = discard.buffer
    if (j > 0) {
      buff.limit(j)
      deleteIds(buff)
    }
  }
}
