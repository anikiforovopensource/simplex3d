/*
 * Simplex3dEngine - Test Package
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

package test

import scala.collection.mutable.ArrayBuffer
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.engine._
import simplex3d.engine.input._
import simplex3d.engine.graphics._


// XXX rework FullscreenEffect to allow extend, then rework the app and the example.
// XXX fix uniforms not updating in shader.
object FullscreenShader extends FullscreenShaderApp with impl.lwjgl.App {
  val title = "Fullscreen Shader"
  
  def main(args: Array[String]) {
    val settings = new Settings(
      fullScreen = false,
      verticalSync = true,
      capabilitiesLog = true,
      performanceLog = true,
      dimensions = Vec2i(800, 600)
    )
    
    launch(settings)
  }
  
  
  val shaderSrc = """
    uniform ivec2 viewDimensions;
    uniform float uptime;
    
    void main() {
      const float borderWidth = 2;
    
      vec3 color = vec3(0, 1, 0) + uptime;
      if (gl_FragCoord.x < borderWidth || gl_FragCoord.x >= viewDimensions.x - borderWidth) color = vec3(0, 0, 1);
      if (gl_FragCoord.y < borderWidth || gl_FragCoord.y >= viewDimensions.y - borderWidth) color = vec3(0, 0, 1);
    
      gl_FragColor = vec4(color, 1.0);
    }
  """

  val viewDims = ShaderProperty[ReadVec2i](Vec2i(0))
  val uptime = ShaderProperty[ReadDoubleRef](0)
  
  val shader = new Shader(Shader.FragmentShader, shaderSrc, Map[String, ShaderProperty[_ <: TechniqueBinding]](
    "viewDimensions" -> viewDims,
    "uptime" -> uptime
  ))
  
  override def update(time: TimeStamp) {
    viewDims.mutable := viewDimensions
    uptime.mutable := time.total
  }
}
