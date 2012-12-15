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
package renderer.fog

import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object ShaderPack extends pluggable.ShaderPack[Unit] {

  object Universal {
    def vertexBasedFogFragment(config: Unit) = new FragmentShader {
      uniform {
        declare[Fog]("fog")
      }
      
      in("fogCtx") {
        declare[DoubleRef]("factor")
      }
      
      function("vec4 applyFog(in vec4 baseColor)"){"""
        return vec4(mix(fog.color, baseColor.rgb, fogCtx.factor), baseColor.a);
      """}
    }
    
    def vertexBasedFogVertex(config: Unit) = new VertexShader {
      use("vec3 ecPosition()")
      
      uniform {
        declare[Fog]("fog")
      }
      
      out("fogCtx") {
        declare[DoubleRef]("factor")
      }
      
      main("computeFog")(){"""
        vec3 ecPos = ecPosition();
        fogCtx.factor = clamp(exp(-fog.density*fog.density*dot(ecPos, ecPos)), 0.0, 1.0);
      """}
    }
  }
  
  
  def mkShaders(profile: Profile.type#Value): (Unit, Seq[ShaderPrototype]) = {
    if (profile != Profile.Gl2) throw new RuntimeException("Profile '" + profile.toString + "' is not supported.")
    
    val shaders = ArrayBuilder.make[ShaderPrototype]
    val config = Unit
    
    shaders += Universal.vertexBasedFogFragment(config).toPrototype(profile)
    shaders += Universal.vertexBasedFogVertex(config).toPrototype(profile)
    
    (config, shaders.result())
  }
}
