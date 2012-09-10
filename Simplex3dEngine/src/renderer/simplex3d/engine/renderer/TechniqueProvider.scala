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
      entryPoint("resolveColor")
      
      use("vec3 lightEmission()")
      use("vec3 lightIntensity()")
      use("vec4 texturingColor()")
      
      in("transformationCtx") {
        declare[Vec4]("gl_FragCoord")
      }
      
      src {"""
        void resolveColor() {
          gl_FragColor = texturingColor() * vec4(lightEmission() + lightIntensity(), 1.0);
          if (gl_FragColor.a == 0.0) discard;
        }
      """}
    })
    
    
    manager.register(new FragmentShader {
      export("vec3 lightEmission()")
      
      src {"""
        vec3 lightEmission() {
          return vec3(0.0);
        }
      """}
    })
    manager.register(new FragmentShader {
      export("vec4 texturingColor()")
      
      src {"""
        vec4 texturingColor() {
          return vec4(1.0);
        }
      """}
    })
    manager.register(new FragmentShader {
      export("vec3 lightIntensity()")
      
      src {"""
        vec3 lightIntensity() {
          return vec3(1.0);
        }
      """}
    })
    
    manager.register(new FragmentShader {
      export("vec3 lightEmission()")
      
      uniform {
        declare[Vec3]("emission")
      }
      
      src {"""
        vec3 lightEmission() {
          return emission;
        }
      """}
    })
    
    manager.register(new FragmentShader {
      export("vec4 texturingColor()")
      
      forceSquareMatrices = true
      
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
            color *= texture2D(textureUnits[i].texture.sampler, texturingCtx.ecTexCoords[i]);
          }
          return color;
        }
      """}
    })
    
    // PointSprites texturing, must be added after default texturing.
    manager.register(new FragmentShader {
      export("vec4 texturingColor()")
      
      forceSquareMatrices = true
      
      uniform {
        declare[DoubleRef]("se_pointSize") // Will restrict shader to PointSprites.
        declare[BindingList[TextureUnit]]("textureUnits")
      }
      
      src {"""
        vec4 texturingColor() {
          vec4 color = vec4(1.0);
          for (int i = 0; i < se_sizeOf_textureUnits; i++) {
            color *= texture2D(textureUnits[i].texture.sampler, gl_PointCoord);
          }
          return color;
        }
      """}
    })
    
    
    manager.register(new VertexShader {
      entryPoint("transformVertices")
      
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
    })

    // PointSprites transformation, must be added after default transformation.
    manager.register(new VertexShader {
      entryPoint("transformVertices")
      
      uniform {
        declare[DoubleRef]("se_pointSize") // Will restrict shader to PointSprites.
        declare[Mat4]("se_modelViewProjectionMatrix")
        declare[Mat4]("se_projectionMatrix")
        declare[Vec2i]("se_viewDimensions")
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
          
          // Universal for all projection matrices.
          gl_PointSize = se_pointSize*0.5*float(se_viewDimensions.y)*se_projectionMatrix[1][1]/gl_Position.w;
        }
      """}
    })
      
    manager.register(new VertexShader {
      entryPoint("propagateTexturingValues")
      
      forceSquareMatrices = true
      
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
    })
    
    manager
  }
}
