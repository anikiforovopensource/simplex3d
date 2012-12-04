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
    
    manager.push(new FragmentShader {
      use("vec3 lightEmission()")
      use("vec3 lightIntensity()")
      use("vec4 texturingColor()")
      use("vec4 applyFog(in vec4 baseColor)")
      
      in("transformationCtx") {
        declare[Vec4]("gl_FragCoord")
      }
      
      main("resolveColor")(){"""
        vec4 lighting = vec4(lightEmission() + lightIntensity(), 1.0);
        vec4 baseColor = texturingColor() * lighting;
        gl_FragColor = applyFog(baseColor);
      """}
    })
    
    
    manager.push(new FragmentShader {
      function("vec3 lightEmission()"){"""
        return vec3(0.0);
      """}
    })
    manager.push(new FragmentShader {
      function("vec4 texturingColor()"){"""
        return vec4(1.0);
      """}
    })
    manager.push(new FragmentShader {
      function("vec3 lightIntensity()"){"""
        return vec3(1.0);
      """}
    })
    manager.push(new FragmentShader {
      function("vec4 applyFog(in vec4 baseColor)"){"""
        return baseColor;
      """}
    })
    
    
    manager.push(new FragmentShader {
      uniform {
        declare[Vec3]("emission")
      }
      
      function("vec3 lightEmission()"){"""
        return emission;
      """}
    })
    
    manager.push(new FragmentShader {
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
    })
    
    // PointSprites texturing, must be added after default texturing.
    manager.push(new FragmentShader {
      
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
    })
    
    manager.push(new FragmentShader {
      uniform {
        declare[Fog]("fog")
      }
      
      in("fogCtx") {
        declare[DoubleRef]("factor")
      }
      
      function("vec4 applyFog(in vec4 baseColor)"){"""
        return vec4(mix(fog.color, baseColor.rgb, fogCtx.factor), baseColor.a);
      """}
    })
    
    manager.push(new VertexShader {
      uniform {
        declare[Mat4]("se_modelViewProjectionMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
      }
      
      out("transformationCtx") {
        declare[Vec4]("gl_Position")
      }
      
      main("transformVertices")(){"""
        gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
      """}
    })

    // PointSprites transformation, must be added after default transformation.
    manager.push(new VertexShader {
      
      condition("primitive.mode")(_ == VertexMode.PointSprites)
      
      uniform {
        declare[DoubleRef]("se_pointSpriteSize") // Only available to PointSprites.
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
      
      main("transformVertices")(){"""
        gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
        
        // Universal for all projection matrices.
        gl_PointSize = se_pointSpriteSize*0.5*float(se_viewDimensions.y)*se_projectionMatrix[1][1]/gl_Position.w;
      """}
    })
      
    manager.push(new VertexShader {
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
    })
    
    manager.push(new VertexShader {
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
    })
    
    manager.push(new VertexShader {
      uniform {
        declare[Mat4]("se_modelViewMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
      }
      
      function("vec3 ecPosition()"){"""
        return vec3(se_modelViewMatrix*vec4(vertices, 1.0));
      """}
    })
    
    manager
  }
}
