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
package renderer.lighting

import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object ShaderPack extends pluggable.ShaderPack[Unit] {

  object Universal {
    def lightingFragment(config: Unit) = new FragmentShader {
      uniform {
        declare[BindingList[PointLight]]("lighting")
      }
      
      in("lightingCtx") {
        declare[Vec3]("normal")
        declare[Vec3]("ecPosition")
      }
      
      function("vec3 lightIntensity()"){"""
        vec3 intensity = vec3(0.0);
        
        for (int i = 0; i < se_sizeOf_lighting; i++) {
      
          vec3 lightDir = lighting[i].ecPosition - lightingCtx.ecPosition;
          float dist = length(lightDir);
        
          float attenuation = 1.0 / (1.0 +
            lighting[i].linearAttenuation * dist +
            lighting[i].quadraticAttenuation * dist*dist
          );
        
          lightDir = lightDir/dist;
          float diffuseFactor = max(0.0, dot(lightingCtx.normal, lightDir));
        
          intensity += lighting[i].intensity * diffuseFactor * attenuation;
        }
        
        return intensity;
      """}
    }
    
    def lightingVertex(config: Unit) = new VertexShader {
      uniform {
        declare[Mat4]("se_modelViewMatrix")
        declare[Mat3]("se_normalMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
        declare[Vec3]("normals")
      }
      
      out("lightingCtx") {
        declare[Vec3]("ecPosition")
        declare[Vec3]("normal")
      }
      
      main("propagateLightingValues")(){"""
        lightingCtx.ecPosition = (se_modelViewMatrix*vec4(vertices, 1.0)).xyz;
        lightingCtx.normal = normalize(se_normalMatrix*normals);
      """}
    }
  }
  
  
  def mkShaders(profile: Profile.type#Value): (Unit, Seq[ShaderPrototype]) = {
    if (profile != Profile.Gl2) throw new RuntimeException("Profile '" + profile.toString + "' is not supported.")
    
    val shaders = ArrayBuilder.make[ShaderPrototype]
    val config = Unit
    
    shaders += Universal.lightingFragment(config).toPrototype(profile)
    shaders += Universal.lightingVertex(config).toPrototype(profile)
    
    (/*config*/Unit, shaders.result())
  }
}
