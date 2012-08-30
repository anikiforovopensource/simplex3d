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

import java.util.logging._
import scala.collection._
import simplex3d.engine.util._


final class Technique private (
  val graphicsContext: GraphicsContext,
  val shaders: Set[Shader],
  val programUniforms: immutable.Map[String, Property[UncheckedBinding]]
) extends EngineInfoRef {
  
  def this(graphicsContext: GraphicsContext, shaders: Set[Shader]) {
    this(graphicsContext, shaders, {
      
      // Extract uniforms.
      val uniforms = scala.collection.mutable.Map[String, Property[UncheckedBinding]]()
      for (shader <- shaders; (name, prop) <- shader.uniforms) {
        val prev = uniforms.put(name, prop)
        
        if (prev.isDefined) Logger.getLogger(classOf[Technique].getName).log(
          Level.SEVERE,
          "Program uniform '" + name + "' is defined in multiple shaders. Only one value will be used."
        )
        
        if (graphicsContext.namespace.contains(name)) {
          Logger.getLogger(classOf[Technique].getName).log(
            Level.SEVERE,
            "Program uniform '" + name + "' is overriden by graphics context."
          )
        }
      }
      
      immutable.Map[String, Property[UncheckedBinding]]() ++ uniforms
    })
  }
}
