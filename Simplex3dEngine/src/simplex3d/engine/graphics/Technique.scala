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


final class Technique private (
  val graphicsContext: GraphicsContext,
  val shaders: Set[Shader],
  args: (
    ReadArray[String],
    ReadArray[DefinedProperty[_ <: TechniqueBinding]]
  )
) extends EngineInfo {
  
  val uniformNames: ReadArray[String] = args._1
  val uniforms: ReadArray[DefinedProperty[_ <: TechniqueBinding]] = args._2
  

  def this(graphicsContext: GraphicsContext, shaders: Set[Shader]) {
    this(graphicsContext, shaders, {
      
      val allUniforms = shaders.toList.flatMap(_.uniforms)
      val allUniformNames = allUniforms.map(_._1)
      val uniqueUniformNames = allUniformNames.distinct
      
      if (uniqueUniformNames.size != allUniformNames.size) {
        var duplicates = (allUniformNames.filterNot(e => uniqueUniformNames.contains(e))).distinct
        Logger.getLogger(classOf[Technique].getName).log(
          Level.SEVERE,
          "Duplicate uniform definitions: " + duplicates.mkString(", ") + "."
        )
      }
      
      val intersection = uniqueUniformNames.toSet.intersect(graphicsContext.namespace)
      if (intersection.size > 0) {
        Logger.getLogger(classOf[Technique].getName).log(
          Level.SEVERE,
          "Namespace collision between shaders and graphics context: " + intersection.mkString(", ") + "."
        )
      }
      
      val (uniformNames, unifromProps) = allUniforms.unzip
      (new ReadArray(uniformNames.toArray), new ReadArray(unifromProps.toArray))
    })
  }
}
