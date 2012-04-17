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
      use("vec4 baseColor()")
      use("vec4 texturingColor()")
      use("vec4 lightIntensity()")
      
      in("transformationCtx") {
        declare[Vec4]("gl_FragCoord")
      }
      
      src {"""
        void resolveColor() {
          gl_FragColor = texturingColor() * lightIntensity();
        }
      """}
      
      entryPoint("resolveColor")
    })
    
    
    manager.register(new FragmentShader {
      src {"""
        vec4 baseColor() {
          return vec4(1.0);
        }
      """}
      
      export("vec4 baseColor()")
    })
    manager.register(new FragmentShader {
      src {"""
        vec4 lightIntensity() {
          return vec4(1.0);
        }
      """}
      
      export("vec4 lightIntensity()")
    })
    
    manager.register(new FragmentShader {
      uniform {
        declare[Vec4]("color")
      }
      
      src {"""
        vec4 baseColor() {
          return color;
        }
      """}
      
      export("vec4 baseColor()")
    })
    
    manager.register(new FragmentShader {
      forceSquareMatrices
      
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      in("texturingCtx") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      src {"""
        vec4 texturingColor() {
          vec4 color = vec4(1.0);
          for (int i = 0; i < se_sizeOf_textureUnits; i++) {
            color *= texture2D(textureUnits[i].texture, texturingCtx.ecTexCoords[i]);
          }
          return color;
        }
      """}
      
      export("vec4 texturingColor()")
    })
    
    
    manager.register(new VertexShader {
      uniform {
        declare[Mat4]("se_modelViewProjectionMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
      }
      
      out("transformationCtx") {
        declare[Vec4]("gl_Position")
      }
      
      src {"""
        void transformVertices() {
          gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
        }
      """}
      
      entryPoint("transformVertices")
    })
      
    manager.register(new VertexShader {
      forceSquareMatrices
      
      uniform {
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      attributes {
        declare[Vec2]("texCoords")
      }
      
      out("texturingCtx") {
        declare[BindingList[Vec2]]("ecTexCoords").size("se_sizeOf_textureUnits")
      }
      
      src {"""
        void propagateTexturingValues() {
          for (int i = 0; i < se_sizeOf_textureUnits; i++) {
            vec3 transformed = textureUnits[i].transformation*vec3(texCoords, 1);
            texturingCtx.ecTexCoords[i] = transformed.xy;
          }
        }
      """}
      
      entryPoint("propagateTexturingValues")
    })
    
    manager
  }
}
