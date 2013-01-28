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

import scala.collection.mutable.ArrayBuilder
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.engine._
import simplex3d.engine.graphics._
import simplex3d.engine.graphics.pluggable._


object ShaderPack extends pluggable.ShaderPack[Unit] {

  object Universal {
    def root(config: Unit) = new FragmentShader {
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
    }
    
    // XXX config as a structural type.
    def stubFog(config: Unit) = new FragmentShader {
      function("vec4 applyFog(in vec4 baseColor)"){"""
        return baseColor;
      """}
    }
    
    def stubTexturing(config: Unit) = new FragmentShader {
      function("vec4 texturingColor()"){"""
        return vec4(1.0);
      """}
    }
    
    def stubEmission(config: Unit) = new FragmentShader {
      function("vec3 lightEmission()"){"""
        return vec3(0.0);
      """}
    }
    
    def stubLighting(config: Unit) = new FragmentShader {
      function("vec3 lightIntensity()"){"""
        return vec3(1.0);
      """}
    }
    
    def basicEmission(config: Unit) = new FragmentShader {
      uniform {
        declare[Vec3]("emission")
      }
      
      function("vec3 lightEmission()"){"""
        return emission;
      """}
    }
    
    def transformation(config: Unit) = new VertexShader {
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
    }
    
    def transformationPointSprite(config: Unit) = new VertexShader {
      
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
    }
  }
  
  object Gl2 {
    def ecPosition(config: Unit) = new VertexShader {
      uniform {
        declare[Mat4x3]("se_modelViewMatrix")
      }
      
      attributes {
        declare[Vec3]("vertices")
      }
      
      function("vec3 ecPosition()"){"""
        return vec3(se_modelViewMatrix*vec4(vertices, 1.0));
      """}
    }
  }
  
  
  def mkShaders(profile: Profile.type#Value) :(Unit, Seq[ShaderPrototype]) = {
    if (profile != Profile.Gl2) throw new RuntimeException("Profile '" + profile.toString + "' is not supported.")
    
    val shaders = ArrayBuilder.make[ShaderPrototype]
    val config = Unit
    
    shaders += Universal.root(config).toPrototype(profile)
    shaders += Universal.stubFog(config).toPrototype(profile)
    shaders += Universal.stubTexturing(config).toPrototype(profile)
    shaders += Universal.stubEmission(config).toPrototype(profile)
    shaders += Universal.stubLighting(config).toPrototype(profile)
    shaders += Universal.basicEmission(config).toPrototype(profile)
    shaders += Universal.transformation(config).toPrototype(profile)
    shaders += Universal.transformationPointSprite(config).toPrototype(profile)
    shaders += Gl2.ecPosition(config).toPrototype(profile)
    
    (/*config*/Unit, shaders.result())
  }
}
