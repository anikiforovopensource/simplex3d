/*
 * Simplex3dEngine - Renderer Module
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
package graphics.pluggable

import scala.collection.mutable.HashMap
import simplex3d.engine.util._


final class StructSignature(
  val level: Int,
  val erasure: Class[_],
  val glslType: String,
  val entries: ReadArray[(String, String)],// (glslType, name) pairs
  val containsSamplers: Boolean
) {
  def isNested = (level != 0)
}


object StructSignature {
  
  def organizeDependencies(signatures: Seq[StructSignature]) :ReadArray[StructSignature] = {
    val map = new HashMap[String, StructSignature]
    
    for (signature <- signatures) {
      val existing = map.get(signature.glslType)
      if (existing.isDefined) {
        val existingClass = existing.get.erasure
        if (signature.erasure != existingClass) throw new AssertionError(
          "Both structs '" +  signature.erasure.getName + "' and '" +
          existingClass.getName + "' map to the same glsl type."
        )
        else if (existing.get.level < signature.level) {
          map.put(signature.glslType, signature)
        }
      }
      else {
        map.put(signature.glslType, signature)
      }
    }
    
    new ReadArray(map.values.toArray.sortBy(-_.level))
  }
}
