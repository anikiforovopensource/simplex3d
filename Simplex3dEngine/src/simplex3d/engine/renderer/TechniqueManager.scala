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

import scala.collection.mutable.HashMap
import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  val passManager = new PassManager[G]
  
  
  private val cache = new java.util.HashMap[Set[Shader], Technique]
  private def cachedTechnique(shaders: Set[Shader]) :Technique = {
    var technique = cache.get(shaders)
    if (technique == null) {
      technique = new Technique(graphicsContext, shaders)
      cache.put(shaders, technique)
    }
    technique
  }
  
  
  private val branchQueues = new HashMap[String, List[Branch]]
  
  private val geometryNames = graphicsContext.mkGeometry().attributeNames
  private val materialNames = graphicsContext.mkMaterial().uniformNames
  private val environmentNames = graphicsContext.mkEnvironment().propertyNames
  
  
  private[this] def find(names: ReadArray[String], name: String) :Int = {
    var i = 0; while (i < names.length) {
      if (names(i) == name) return i
      
      i += 1
    }
    -1
  }
  
  def register(branch: Branch) {
    var requiredGeometry = List[Int]()
    var requiredMaterial = List[Int]()
    var requiredEnvironment = List[Int]()
    
    for (prop <- branch.requiredProperties) {
      var id = find(geometryNames, prop)
      if (id >= 0) requiredGeometry ::= id
      else {
        id = find(materialNames, prop)
        if (id >= 0) requiredMaterial ::= id
        else {
          id = find(environmentNames, prop)
          if (id >= 0) requiredEnvironment ::= id
          else throw new RuntimeException() //XXX log and return
        }
      }
    }
    
    branch.requiredGeometry = requiredGeometry.sorted.toArray
    branch.requiredMaterial = requiredMaterial.sorted.toArray
    branch.requiredEnvironment = requiredEnvironment.sorted.toArray
    
    val branchQueue = branchQueues.get(branch.name).getOrElse(Nil)
    branchQueues.put(branch.name, branch :: branchQueue)
  }
  
  
  def resolveTechnique(
    meshName: String,
    geometry: G#Geometry, material: G#Material, worldEnvironment: G#Environment
  )
  :Technique = {
    
    def allDefined(required: Array[Int], properties: ReadArray[Property[_]]) :Boolean = {
      var passed = true
      var i = 0; while (passed && i < required.length) {
        if (!properties(required(i)).isDefined) passed = false

        i += 1
      }
      passed
    }
    
    def passesRequirements(branch: Branch) :Boolean = {
      var passed = true
      
      val requiredGeometry = branch.requiredGeometry
      var i = 0; while (passed && i < requiredGeometry.length) {
        if (!geometry.attributes(requiredGeometry(i)).isDefined) {
          passed = false
        }

        i += 1
      }
      
      if (passed) passed = allDefined(branch.requiredMaterial, material.uniforms)
      if (passed) passed = allDefined(branch.requiredEnvironment, worldEnvironment.properties)
      
      passed
    }
    
    var result = Set[Shader]()
    
    def processQueue(queue: Option[List[Branch]]) :Boolean = {
      if (!queue.isDefined) return false //XXX log
      
      val passed = queue.get.find(branch => passesRequirements(branch))
      if (!passed.isDefined) return false // XXX log
      
      result ++= passed.get.shaders
      
      var resolved = true
      for (subBranch <- passed.get.subBrachnges) {
        resolved = resolved && processQueue(branchQueues.get(subBranch))
      }
      
      resolved
    }
    
    val resolved = processQueue(branchQueues.get("main"))
    if (resolved) cachedTechnique(result)
    else null
  }
  
  
  // Default setup
  {
    register(new Branch("main", subBrachnges = Set("resolveColor"), requiredProperties = Set.empty)(
      new Shader(Shader.VertexShader, """
          uniform mat4 se_modelViewProjectionMatrix;
          attribute vec3 vertices;
          
          void resolveColor();
          
          void main() {
            gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
            resolveColor();
          }
        """
      ),
      new Shader(Shader.FragmentShader, """
          vec4 resolveColor();
          
          void main() {
            gl_FragColor = resolveColor();
          }
        """
      )
    ))
    
    register(new Branch("resolveColor", subBrachnges = Set.empty, requiredProperties = Set.empty)(
      new Shader(Shader.VertexShader, """
          void resolveColor() {}
        """
      ),
      new Shader(Shader.FragmentShader, """
          vec4 resolveColor() {
            return vec4(1);
          }
        """
      )
    ))
    
    
    register(new Branch("resolveColor", subBrachnges = Set.empty, requiredProperties = Set("texCoords", "texture"))(
      new Shader(Shader.VertexShader, """
          attribute vec2 texCoords;
          varying vec2 ecTexCoords;
          
          void resolveColor() {
            ecTexCoords = texCoords;
          }
        """
      ),
      new Shader(Shader.FragmentShader, """
          varying vec2 ecTexCoords;
          uniform sampler2D texture;
          
          vec4 resolveColor() {
            return texture2D(texture, ecTexCoords);
          }
        """
      )
    ))
  }
}

class Branch
  (val name: String, val subBrachnges: Set[String], val requiredProperties: Set[String])
  (val shaders: Shader*)
{
  private[renderer] var requiredGeometry: Array[Int] = _
  private[renderer] var requiredMaterial: Array[Int] = _
  private[renderer] var requiredEnvironment: Array[Int] = _
}
