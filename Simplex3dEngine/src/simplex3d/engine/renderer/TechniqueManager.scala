/*
 * Simplex3dEngine - Renderer Module
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
package renderer

import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  val passManager = new PassManager[G]

  
  private val cached = {
    val vertexShader = """
uniform mat4 se_modelViewMatrix;
uniform mat4 se_modelViewProjectionMatrix;
uniform mat3 se_normalMatrix;
attribute vec3 vertices;

//uniform float fogDensity;

attribute vec3 normals;
attribute vec2 texCoords;

varying vec3 ecPosition;
varying vec3 ecNormal;
varying vec2 ecTexCoords;
//varying float fogFactor;

void main() {
  gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
  ecPosition = vec3(se_modelViewMatrix*vec4(vertices, 1.0));
  
  //fogFactor = clamp(exp(-fogDensity*fogDensity*dot(ecPosition, ecPosition)), 0.0, 1.0);

  ecNormal = normalize(se_normalMatrix*normals);
  ecTexCoords = texCoords.xy;
}
"""
      
    val fragmentShader = """
//uniform vec3 ecLightDir;

//uniform vec3 color;
uniform sampler2D texture;

//uniform vec3 fogColor;

varying vec3 ecPosition;
varying vec3 ecNormal;
varying vec2 ecTexCoords;
//varying float fogFactor;

void main() {
  vec3 ecLightDir = vec3(0.25, 0.5, -1); //XXX

  vec3 normal = normalize(ecNormal);
  if (!gl_FrontFacing) normal = -normal;
  float intensity = max(0.0, dot(-ecLightDir, normal));
  vec3 lighting = vec3(intensity) + 0.2;

  vec3 matColor = texture2D(texture, ecTexCoords).rgb*lighting;//*color
  gl_FragColor = vec4(matColor, 1);//vec4(mix(fogColor, matColor, fogFactor), 1.0);
}
"""
    
    val shaders = List[Shader](
      new Shader(Shader.VertexShader, vertexShader),
      new Shader(Shader.FragmentShader, fragmentShader)
    )
    new Technique(graphicsContext, shaders)
  }
  
  
  def resolveTechnique(
    geometry: graphics.Geometry,
    material: graphics.Material,
    worldEnvironment: graphics.Environment)
  :Technique = {
    cached
  }
}
