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

package simplex3d.engine.util

import simplex3d.engine.graphics._


class StructuralChangeListener { self =>
  final class StructuralSubtext {
    def hasStructuralChanges: Boolean = changes
    def clearStructuralChanges() { changes = false }
  }
  private[engine] final val structuralSubtext = new StructuralSubtext
  
  
  private[engine] final var changes = true // Initialize as changed.
  
  private[engine] final def signalStructuralChanges() {
    changes = true
  }
}

object StructuralChangeListener {
  final val Ignore = new StructuralChangeListener//XXX remove this
  
  def register(listener: StructuralChangeListener, properties: ReadArray[Property[_]]) {
    val s = properties.size; var i = 0; while (i < s) {
      properties(i).register(listener)
      i += 1
    }
  }
}
