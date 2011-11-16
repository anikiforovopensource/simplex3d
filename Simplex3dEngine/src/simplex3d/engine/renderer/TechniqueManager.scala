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

import scala.collection._
import simplex3d.engine.util._
import simplex3d.engine.graphics._


class TechniqueManager[G <: GraphicsContext](implicit val graphicsContext: G)
extends graphics.TechniqueManager[G]
{
  val passManager = new PassManager[G]
  
  
  private val cache = new java.util.HashMap[immutable.Set[graphics.Shader], Technique]
  private def cachedTechnique(shaders: immutable.Set[graphics.Shader]) :Technique = {
    var technique = cache.get(shaders)
    if (technique == null) {
      technique = new Technique(graphicsContext, shaders)
      cache.put(shaders, technique)
    }
    technique
  }
  
  
  private val branchQueues = new mutable.HashMap[String, List[Branch]]
  
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
    
    def processQueue(
      branchName: String,
      shaders: mutable.ArrayBuffer[graphics.Shader],
      deadBranches: mutable.Set[String]
    )
    :Boolean = {
      val queue = branchQueues.get(branchName)
      if (!queue.isDefined) return false
      
      val branches = queue.get.iterator
      while (branches.hasNext) { val branch = branches.next()
        if (passesRequirements(branch)) {
          
          var resolved = true
          val dependencies = new mutable.ArrayBuffer[graphics.Shader]
          
          val subBranches = branch.subBranches.iterator
          while (resolved && subBranches.hasNext) { val subBranch = subBranches.next()
            if (deadBranches.contains(subBranch)) resolved = false
            else resolved = resolved && processQueue(subBranch, dependencies, deadBranches)
          }
          
          if (resolved) {
            shaders ++= branch.shaders
            shaders ++= dependencies
            return true
          }
          else {
            deadBranches += branchName
          }
        }
      }
      
      false
    }
    
    val mainBranch = geometry.mode match {
      case _: PointSprites => "ps_main"
      case _ => "main"
    }
    
    val shaders = mutable.ArrayBuffer[graphics.Shader]()
    val resolved = processQueue(mainBranch, shaders, mutable.Set[String]())
    
    if (resolved) cachedTechnique(shaders.toSet)
    else null
  }
  
  
  // Default setup.
  {
    register(new Branch("main", subBranches = Set("resolveColor"), requiredProperties = Set.empty)(
      new graphics.Shader(graphics.Shader.Vertex, """
          uniform mat4 se_modelViewProjectionMatrix;
          attribute vec3 vertices;
          
          void resolveColor();
          
          void main() {
            gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
            resolveColor();
          }
        """
      ),
      new graphics.Shader(graphics.Shader.Fragment, """
          vec4 resolveColor();
          
          void main() {
            gl_FragColor = resolveColor();
          }
        """
      )
    ))
    
    register(new Branch("resolveColor", subBranches = Set.empty, requiredProperties = Set.empty)(
      new graphics.Shader(graphics.Shader.Vertex, """
          void resolveColor() {}
        """
      ),
      new graphics.Shader(graphics.Shader.Fragment, """
          vec4 resolveColor() {
            return vec4(1);
          }
        """
      )
    ))
    
    
    register(new Branch("resolveColor", subBranches = Set.empty, requiredProperties = Set("texCoords", "texture"))(
      new graphics.Shader(graphics.Shader.Vertex, """
          attribute vec2 texCoords;
          varying vec2 ecTexCoords;
          
          void resolveColor() {
            ecTexCoords = texCoords;
          }
        """
      ),
      new graphics.Shader(graphics.Shader.Fragment, """
          varying vec2 ecTexCoords;
          uniform sampler2D texture;
          
          vec4 resolveColor() {
            return texture2D(texture, ecTexCoords);
          }
        """
      )
    ))
  }
  
  // Point sprite setup.
  {
    register(new Branch("ps_main", subBranches = Set("ps_resolveColor"), requiredProperties = Set.empty)(
      new graphics.Shader(graphics.Shader.Vertex, """
          uniform mat4 se_projectionMatrix;
          uniform ivec2 se_viewDimensions;
          
          uniform mat4 se_modelViewProjectionMatrix;
          
          uniform float se_pointSize;
          
          
          attribute vec3 vertices;
          
          void main() {
            gl_Position = se_modelViewProjectionMatrix*vec4(vertices, 1.0);
            gl_PointSize = se_pointSize*0.5*float(se_viewDimensions.y)*se_projectionMatrix[1][1]/gl_Position.w;
          }
        """
      ),
      new graphics.Shader(graphics.Shader.Fragment, """
          vec4 ps_resolveColor();
          
          void main() {
            gl_FragColor = ps_resolveColor();
          }
        """
      )
    ))
    
    register(new Branch("ps_resolveColor", subBranches = Set.empty, requiredProperties = Set.empty)(
      new graphics.Shader(graphics.Shader.Fragment, """
          vec4 ps_resolveColor() {
            return vec4(1);
          }
        """
      )
    ))
    
    register(new Branch("ps_resolveColor", subBranches = Set.empty, requiredProperties = Set("texture"))(
      new graphics.Shader(graphics.Shader.Fragment, """
          #version 120
          
          uniform sampler2D texture;
          
          vec4 ps_resolveColor() {
            return texture2D(texture, gl_PointCoord);
          }
        """
      )
    ))
  }
}

class Branch
  (val name: String, val subBranches: Set[String], val requiredProperties: Set[String])
  (val shaders: graphics.Shader*)
{
  private[renderer] var requiredGeometry: Array[Int] = _
  private[renderer] var requiredMaterial: Array[Int] = _
  private[renderer] var requiredEnvironment: Array[Int] = _
}
