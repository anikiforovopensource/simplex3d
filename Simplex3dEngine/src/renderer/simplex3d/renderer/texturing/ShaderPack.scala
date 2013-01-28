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

package simplex3d.renderer
package texturing

import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object ShaderPack extends pluggable.ShaderPack[Unit] {
  
  object Universal {
    def texturing(config: Unit) = new FragmentShader {
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      in("texturingCtx") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      function("vec4 texturingColor()"){"""
        vec4 color = vec4(1.0);
        for (int i = 0; i < se_sizeOf_textureUnits; i++) {
          color *= texture2D(texture[i], texturingCtx.ecTexCoords[i]);
        }
        return color;
      """}
    }
    
    def texturingPointSprites(config: Unit) = new FragmentShader {
      
      condition("primitive.mode")(_ == VertexMode.PointSprites)
      
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      function("vec4 texturingColor()"){"""
        vec4 color = vec4(1.0);
        for (int i = 0; i < se_sizeOf_textureUnits; i++) {
          color *= texture2D(texture[i], gl_PointCoord);
        }
        if (color.a == 0.0) discard;
        return color;
      """}
    }
  }
  
  object Gl2 {
    def transformTexCoords(config: Unit) = new VertexShader {
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      attributes {
        declare[Vec2]("texCoords")
      }
      
      out("texturingCtx") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      main("transformTexCoords")(){"""
        for (int i = 0; i < se_sizeOf_textureUnits; i++) {
          vec3 transformed = textureUnits[i].transformation*vec3(texCoords, 1);
          texturingCtx.ecTexCoords[i] = transformed.xy;
        }
      """}
    }
  }
  
  
  def mkShaders(profile: Profile.type#Value): (Unit, Seq[ShaderPrototype]) = {
    if (profile != Profile.Gl2) throw new RuntimeException("Profile '" + profile.toString + "' is not supported.")
    
    val shaders = ArrayBuilder.make[ShaderPrototype]
    val config = Unit
    
    shaders += Universal.texturing(config).toPrototype(profile)
    shaders += Universal.texturingPointSprites(config).toPrototype(profile)
    shaders += Gl2.transformTexCoords(config).toPrototype(profile)
    
    (/*config*/Unit, shaders.result())
  }
}
