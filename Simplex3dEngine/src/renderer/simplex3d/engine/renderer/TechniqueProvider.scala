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
package renderer

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object TechniqueProvider {
  def assembleTechniqueManager[G <: GraphicsContext]()(implicit graphicsContext: G) :pluggable.TechniqueManager[G] = {
    val manager = new pluggable.TechniqueManager[G]
    
    manager.register(new FragmentShader {
      uniform {
        declare[BindingList[TextureBinding[Texture2d[_]]]]("textures")
      }
      
      in("rasterisation") {
        declare[Vec4]("gl_FragCoord")
      }
      in("textureCoords") {
        declare[Vec2]("ecTexCoords")
      }
      
      src {"""
        void resolveColor() {
          vec4 color = vec4(1);
          for (int i = 0; i < se_sizeOf_textures; i++) {
            color *= texture2D(textures[i], textureCoords.ecTexCoords);
          }
          gl_FragColor = color;
        }
      """}
      
      entryPoint("resolveColor")
    })
    
    manager.register(new FragmentShader {
      //version("120")
      useSquareMatrices
      
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      in("rasterisation") {
        declare[Vec4]("gl_FragCoord")
      }
      in("textureUnitCoords") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      src {"""
        void resolveColor() {
          vec4 color = vec4(1);
          for (int i = 0; i < se_sizeOf_textureUnits; i++) {
            color *= texture2D(textureUnits[i].texture, textureUnitCoords.ecTexCoords[i]);
          }
          gl_FragColor = color;
        }
      """}
      
      entryPoint("resolveColor")
    })
    
    
    manager.register(new VertexShader {
      attributes {
        declare[Vec2]("texCoords")
      }
      
      out("textureCoords") {
        declare[Vec2]("ecTexCoords")
      }
      
      src {"""
        void passTexCoords() {
          textureCoords.ecTexCoords = texCoords;
        }
      """}
      
      entryPoint("passTexCoords")
    })
    
    manager.register(new VertexShader {
      uniform {
        declare[Mat4]("se_modelViewProjectionMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
      }
      
      out("rasterisation") {
        declare[Vec4]("gl_Position")
      }
      
      src {"""
        void transformVertices() {
          rasterisation.gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
        }
      """}
      
      entryPoint("transformVertices")
    })
    
    manager.register(new VertexShader {
      //version("120")
      useSquareMatrices
      
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      attributes {
        declare[Vec2]("texCoords")
      }
      
      out("textureUnitCoords") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      src {"""
        void passTexCoords() {
          for (int i = 0; i < se_sizeOf_textureUnits; i++) {
            textureUnitCoords.ecTexCoords[i] = (textureUnits[i].transformation*vec3(texCoords, 1)).xy;
          }
        }
      """}
      
      entryPoint("passTexCoords")
    })
    
    manager
  }
}
